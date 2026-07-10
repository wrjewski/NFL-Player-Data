# app.R

library(shiny)
library(nflreadr)
library(bslib)
library(DT)
library(lubridate)
library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)
library(purrr)

# Load local secrets (ODDS_API_KEY, etc.) from .Renviron if present.
# .Renviron is git-ignored — see .Renviron.example for the template.
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Data cache, opponent adjustment, distribution, and market-edge helpers.
source("R/data_pipeline.R")
source("R/opponent_adjustment.R")
source("R/distributions.R")
source("R/market_edge.R")

# Load UI modules
source("modules/ui_main_home.R")
source("modules/ui_team_stats.R")
source("modules/ui_betting.R")
source("modules/ui_home.R")
source("modules/ui_team.R")
source("modules/ui_position.R")
source("modules/ui_player.R")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  h2("NFL Player Stats App"),
  uiOutput("main_ui")
)

server <- function(input, output, session) {

  offense_positions <- c("QB","RB","WR","TE","FB","OL","C","G","T")
  defense_positions <- c("DL","DE","DT","LB","OLB","ILB","CB","S","FS","SS")

  current_page <- reactiveVal("home")
  selected_team <- reactiveVal(NULL)
  selected_position <- reactiveVal(NULL)
  selected_player <- reactiveVal(NULL)

  output$main_ui <- renderUI({
    switch(current_page(),
      "home" = ui_main_home(),
      "team_stats" = ui_team_stats(),
      "sports_betting" = ui_betting(),
      "player_stats" = ui_home(current_page, selected_team),
      "team" = ui_team(current_page, selected_team, selected_position, offense_positions, defense_positions),
      "position" = ui_position(current_page, selected_team, selected_position, selected_player),
      "player" = ui_player(current_page, selected_player)
    )
  })

  # --- Load NFL Data ---
  # Auto-detects the season instead of a hardcoded year, so this doesn't
  # need a code change every year. current_season follows nflverse's own
  # Labor Day cutover (the season with games being played/most recently
  # completed); current_roster_season follows the mid-March free agency
  # cutover, since teams start building next season's roster well before
  # that season's games begin.
  current_season <- nflreadr::get_current_season()
  current_roster_season <- nflreadr::get_current_season(roster = TRUE)

  # Reads from data_cache/ (populated by etl/refresh_cache.R) instead of
  # hitting nflverse on every session start. See R/data_pipeline.R.
  sched_all <- get_schedules_cached(seasons = current_season) %>%
    distinct(game_id, .keep_all = TRUE)
  team_stats_all <- get_team_stats_cached(seasons = current_season)

  # Opponent-adjusted offense/defense ratings (R/opponent_adjustment.R),
  # replacing the old raw-average team_summary which conflated a team's
  # strength with the strength of the schedule it happened to face.
  team_summary <- compute_opponent_adjusted_ratings(team_stats_all)

  # --- Odds API setup ---
  # Key is read from the ODDS_API_KEY environment variable (see .Renviron.example).
  # Never hardcode API keys in source — .Renviron is git-ignored for this reason.
  odds_api_key <- Sys.getenv("ODDS_API_KEY")
  if (identical(odds_api_key, "")) {
    warning(
      "ODDS_API_KEY is not set. Copy .Renviron.example to .Renviron and add your key. ",
      "Odds/betting features will be unavailable until this is set."
    )
  }
  odds_base <- "https://api.the-odds-api.com/v4"

  fetch_game_odds <- function() {
    resp <- tryCatch({
      GET(
        url = paste0(odds_base, "/sports/americanfootball_nfl/odds"),
        query = list(
          apiKey = odds_api_key,
          regions = "us",
          markets = "h2h,spreads,totals",
          oddsFormat = "american"
        )
      )
    }, error = function(e) {
      # A network failure here must not take down the whole app for every
      # session -- degrade to "no odds data" instead.
      warning("fetch_game_odds: request failed: ", conditionMessage(e))
      NULL
    })
    if (is.null(resp)) {
      return(list())
    }
    if (status_code(resp) != 200) {
      warning("fetch_game_odds: HTTP status ", status_code(resp))
      return(list())
    }
    txt <- content(resp, as = "text", encoding = "UTF-8")
    # Try parse JSON; if fails, return empty
    parsed <- tryCatch({
      fromJSON(txt, flatten = TRUE)
    }, error = function(e) {
      warning("fetch_game_odds: JSON parse error: ", e$message)
      list()
    })
    parsed
  }

  game_odds_json <- fetch_game_odds()

  odds_games_df <- reactive({
    map_dfr(game_odds_json, function(g) {
      # Only proceed if g is a list and has bookmakers
      if (!is.list(g) || is.null(g$bookmakers)) {
        return(tibble())
      }
      bm <- g$bookmakers[[1]]
      markets <- bm$markets
      h2h_m <- markets %>% keep(~ .x$key == "h2h")
      sp_m <- markets %>% keep(~ .x$key == "spreads")
      to_m <- markets %>% keep(~ .x$key == "totals")
      tibble(
        event_id = g$id,
        home_team = g$home_team,
        away_team = g$away_team,
        ml_home = if (!is.null(h2h_m[[1]])) h2h_m[[1]]$outcomes[[1]]$price else NA_real_,
        ml_away = if (!is.null(h2h_m[[1]])) h2h_m[[1]]$outcomes[[2]]$price else NA_real_,
        spread_line = if (!is.null(sp_m[[1]])) sp_m[[1]]$outcomes[[1]]$point else NA_real_,
        total_line = if (!is.null(to_m[[1]])) to_m[[1]]$outcomes[[1]]$point else NA_real_
      )
    })
  })

  # --- Game Predictions logic ---
  scheduled_with_stats <- reactive({
    req(input$pred_week)
    
    # Get games for the selected week
    games <- sched_all %>%
      filter(game_type == "REG", week == input$pred_week) %>%
      select(game_id, season, week, gameday, gametime, away_team, home_team, 
             away_score, home_score, spread_line, total_line, away_moneyline, home_moneyline)
    
    # Add team stats if available
    if (exists("team_summary") && nrow(team_summary) > 0) {
      games <- games %>%
      left_join(team_summary, by = c("home_team" = "team")) %>%
      left_join(team_summary, by = c("away_team" = "team"), suffix = c("_home", "_away"))
    }
    
    # Add odds data if available
    tryCatch({
    odds_data <- odds_games_df()
    if (nrow(odds_data) > 0 && "event_id" %in% names(odds_data)) {
        games <- games %>% 
          left_join(odds_data, by = c("game_id" = "event_id"), suffix = c("", "_odds"))
      }
    }, error = function(e) {
      # If odds fail, continue without them
      warning("Could not load odds data: ", e$message)
    })
    
    # Ensure required columns exist
    if (!"spread_line" %in% names(games)) games$spread_line <- NA_real_
    if (!"total_line" %in% names(games)) games$total_line <- NA_real_
    if (!"away_moneyline" %in% names(games)) games$away_moneyline <- NA_real_
    if (!"home_moneyline" %in% names(games)) games$home_moneyline <- NA_real_
    
    games
  })

  # Pulls a column's value if present, else NA -- lets us prefer the live
  # odds-API price but fall back to the schedule's own line when the API
  # is unavailable, without erroring on a missing column.
  get_col_or_na <- function(row, col) if (col %in% names(row)) row[[col]] else NA_real_

  predict_game_edge <- function(row) {
    required_cols <- c("adj_offense_home", "adj_defense_away", "adj_offense_away", "adj_defense_home")
    if (!all(required_cols %in% names(row)) || anyNA(row[required_cols])) {
      return("No Data")
    }

    # Opponent-adjusted margin (Item 3) converted to a win probability
    # (Item 5), so it can be compared against the market's own implied
    # probability instead of an arbitrary fixed point threshold.
    pred_margin <- (row$adj_offense_home - row$adj_defense_away) -
                   (row$adj_offense_away - row$adj_defense_home)
    pred_total <- row$adj_offense_home + row$adj_offense_away
    model_home_win_prob <- margin_to_win_prob(pred_margin)

    home_ml <- dplyr::coalesce(get_col_or_na(row, "ml_home"), get_col_or_na(row, "home_moneyline"))
    away_ml <- dplyr::coalesce(get_col_or_na(row, "ml_away"), get_col_or_na(row, "away_moneyline"))

    if (!is.na(home_ml) && !is.na(away_ml)) {
      market <- devig_two_way(american_to_prob(home_ml), american_to_prob(away_ml))
      edge <- compute_edge(model_home_win_prob, market$prob_a, min_edge = 0.04)
      if (edge$has_edge) {
        favored <- if (edge$edge > 0) "Home ML" else "Away ML"
        return(sprintf("%s (edge %+.0f%%)", favored, edge$edge * 100))
      }
    }

    total_line <- dplyr::coalesce(get_col_or_na(row, "total_line_odds"), get_col_or_na(row, "total_line"))
    if (!is.na(total_line) && abs(pred_total - total_line) > 7) {
      return(paste0("O/U: ", if (pred_total > total_line) "Over" else "Under"))
    }

    return("No Edge")
  }

  output$upcoming_games_table <- DT::renderDataTable({
    tryCatch({
    df <- scheduled_with_stats()
      if (nrow(df) == 0) {
        return(DT::datatable(data.frame(Message = "No games found for the selected week"), rownames = FALSE))
      }
    df$best_bet <- vapply(seq_len(nrow(df)), function(i) predict_game_edge(df[i, ]), character(1))
    cols <- c("season","week","gameday","gametime","away_team","home_team",
              "spread_line","total_line","away_moneyline","home_moneyline","best_bet")
      # Only include columns that exist
      cols <- intersect(cols, names(df))
    disp <- df[, cols, drop = FALSE]
    names(disp) <- gsub("_", " ", tools::toTitleCase(names(disp)))
    DT::datatable(disp, options = list(scrollX = TRUE), rownames = FALSE) %>%
      DT::formatStyle("Best bet", target = "cell",
        backgroundColor = DT::styleEqual(unique(disp$`Best bet`),
                                         rep("lightgreen", length(unique(disp$`Best bet`)))),
        fontWeight = "bold"
      )
    }, error = function(e) {
      # Return empty table with error message
      DT::datatable(data.frame(Error = paste("Unable to load game data:", e$message)), rownames = FALSE)
    })
  })

  # --- Game Predictions and Player Props logic ---
  # Dropdowns are now initialized in the UI with correct choices
  
  # Function to create mock prop lines when API fails
  create_mock_prop_lines <- function() {
    # Create sample prop lines for demonstration
    tibble(
      market = rep(c("player_pass_yds", "player_rush_yds", "player_receptions"), each = 5),
      player_id = rep(paste0("mock_player_", 1:5), 3),
      player_name = rep(c("Sample QB", "Sample RB", "Sample WR", "Sample TE", "Sample K"), 3),
      prop_line = c(
        rep(c(250, 100, 5, 3, 1.5), 1),  # Passing yards
        rep(c(80, 50, 20, 10, 5), 1),    # Rushing yards  
        rep(c(6, 4, 3, 2, 1), 1)         # Receptions
      )
    )
  }

  matchup_choices <- reactive({
    req(input$prop_week)
    sched_all %>%
      filter(season == current_season, game_type == "REG", week == input$prop_week) %>%
      mutate(label = paste(away_team, "vs", home_team)) %>%
      select(game_id, label)
  })

  observe({
    mc <- matchup_choices()
    if (nrow(mc) > 0) {
      updateSelectInput(session, "prop_matchup",
                        choices = setNames(mc$game_id, mc$label),
                        selected = mc$game_id[1])
    }
  })

  upcoming_player_props <- reactive({
    req(input$prop_week, input$prop_matchup)

    if (input$prop_matchup == "none") {
      return(tibble())
    }

    # Get the game info from the schedule data for the selected matchup
    selected_game <- sched_all %>% filter(game_id == input$prop_matchup)

    if (nrow(selected_game) == 0) {
        return(tibble())
    }

    week_num <- selected_game$week[1]
    home_team <- selected_game$home_team[1]
    away_team <- selected_game$away_team[1]

    # Load current rosters (Item 1: served from data_cache/, not live network)
    current_rosters <- get_rosters_cached(seasons = current_season)

    # Get current rosters for both teams
    home_players <- current_rosters %>%
      filter(team == home_team, position %in% c("QB", "RB", "WR", "TE")) %>%
      select(player_name = full_name, position, team)

    away_players <- current_rosters %>%
      filter(team == away_team, position %in% c("QB", "RB", "WR", "TE")) %>%
      select(player_name = full_name, position, team)

    all_players <- bind_rows(home_players, away_players)

    if (nrow(all_players) == 0) {
      return(tibble())
    }

    # Get recent stats for trend analysis
    recent_stats <- get_player_stats_cached(seasons = current_season) %>%
      filter(week <= week_num, week >= max(1, week_num - 3)) %>%
      select(player_name, position, passing_yards, rushing_yards, receiving_yards, receptions,
             passing_tds, rushing_tds, receiving_tds)

    # Item 2: injury status discounts predictions for players who are
    # doubtful/out instead of silently ignoring availability. Wrapped in
    # tryCatch so an unexpected schema/network hiccup degrades to "no
    # adjustment" rather than breaking the whole props table.
    injury_status <- tryCatch({
      get_injuries_cached(seasons = current_season) %>%
        filter(week == week_num) %>%
        select(player_name = full_name, team, report_status) %>%
        distinct(player_name, team, .keep_all = TRUE)
    }, error = function(e) {
      tibble(player_name = character(), team = character(), report_status = character())
    })

    # Item 2: Next Gen Stats completion % above expectation refines the QB
    # yardage projection beyond a raw box-score average.
    qb_cpoe <- tryCatch({
      get_nextgen_stats_cached(seasons = current_season, stat_type = "passing") %>%
        filter(week <= week_num, week >= max(1, week_num - 3)) %>%
        group_by(player_name = player_display_name) %>%
        summarise(avg_cpoe = mean(completion_percentage_above_expectation, na.rm = TRUE), .groups = "drop")
    }, error = function(e) {
      tibble(player_name = character(), avg_cpoe = numeric())
    })

    # Generate prop predictions based on recent averages, adjusted for
    # availability and (for QBs) recent passing accuracy trend.
    prop_predictions <- all_players %>%
      left_join(recent_stats, by = c("player_name", "position")) %>%
      group_by(player_name, team, position) %>%
      summarise(
        avg_passing = mean(passing_yards, na.rm = TRUE),
        avg_rushing = mean(rushing_yards, na.rm = TRUE),
        avg_receiving = mean(receiving_yards, na.rm = TRUE),
        avg_receptions = mean(receptions, na.rm = TRUE),
        avg_passing_tds = mean(passing_tds, na.rm = TRUE),
        avg_rushing_tds = mean(rushing_tds, na.rm = TRUE),
        avg_receiving_tds = mean(receiving_tds, na.rm = TRUE),
        games_played = n(),
        .groups = "drop"
      ) %>%
      filter(games_played > 0) %>%
      left_join(injury_status, by = c("player_name", "team")) %>%
      left_join(qb_cpoe, by = "player_name") %>%
      rowwise() %>%
      mutate(
        cpoe_factor = if (position == "QB" && !is.na(avg_cpoe)) {
          max(0.9, min(1.1, 1 + (avg_cpoe / 100)))
        } else {
          1
        },
        availability_factor = dplyr::case_when(
          is.na(report_status) ~ 1,
          report_status == "Out" ~ 0,
          report_status == "Doubtful" ~ 0.25,
          report_status == "Questionable" ~ 0.85,
          TRUE ~ 1
        ),
        passing_yards_pred = if (position == "QB" && !is.na(avg_passing)) round(avg_passing * cpoe_factor * availability_factor) else NA,
        rushing_yards_pred = if (position %in% c("QB", "RB") && !is.na(avg_rushing)) round(avg_rushing * availability_factor) else NA,
        receiving_yards_pred = if (position %in% c("WR", "TE", "RB") && !is.na(avg_receiving)) round(avg_receiving * availability_factor) else NA,
        receptions_pred = if (position %in% c("WR", "TE", "RB") && !is.na(avg_receptions)) round(avg_receptions * availability_factor) else NA,
        passing_tds_pred = if (position == "QB" && !is.na(avg_passing_tds)) round(avg_passing_tds * availability_factor * 10) / 10 else NA,
        rushing_tds_pred = if (position %in% c("QB", "RB") && !is.na(avg_rushing_tds)) round(avg_rushing_tds * availability_factor * 10) / 10 else NA,
        receiving_tds_pred = if (position %in% c("WR", "TE", "RB") && !is.na(avg_receiving_tds)) round(avg_receiving_tds * availability_factor * 10) / 10 else NA
      ) %>%
      ungroup() %>%
      pivot_longer(
        cols = c(passing_yards_pred, rushing_yards_pred, receiving_yards_pred, receptions_pred,
                passing_tds_pred, rushing_tds_pred, receiving_tds_pred),
        names_to = "prop_type",
        values_to = "prediction"
      ) %>%
      filter(!is.na(prediction)) %>%
      mutate(
        stat_family = if_else(grepl("tds", prop_type), "count", "yardage"),
        prop_type = case_when(
          prop_type == "passing_yards_pred" ~ "Passing Yards",
          prop_type == "rushing_yards_pred" ~ "Rushing Yards",
          prop_type == "receiving_yards_pred" ~ "Receiving Yards",
          prop_type == "receptions_pred" ~ "Receptions",
          prop_type == "passing_tds_pred" ~ "Passing TDs",
          prop_type == "rushing_tds_pred" ~ "Rushing TDs",
          prop_type == "receiving_tds_pred" ~ "Receiving TDs"
        ),
        # Item 4: the "line" a book would post is usually just under the
        # median prediction (so it isn't a push); report the model's
        # probability of clearing it instead of only a bare point estimate.
        model_line = if_else(stat_family == "count",
                              pmax(0, prediction - 0.5),
                              pmax(0, round(prediction * 2) / 2 - 0.5))
      ) %>%
      rowwise() %>%
      mutate(
        prob_over = if (stat_family == "count") {
          prob_over_count(prediction, model_line)
        } else {
          prob_over_yardage(prediction, model_line)
        }
      ) %>%
      ungroup() %>%
      mutate(
        confidence = case_when(
          games_played >= 3 ~ "High",
          games_played >= 2 ~ "Medium",
          TRUE ~ "Low"
        )
      ) %>%
      select(player_name, team, position, prop_type, prediction, model_line, prob_over, confidence) %>%
      arrange(team, position, player_name, prop_type)

    return(prop_predictions)
  })

  output$player_props_table <- DT::renderDataTable({
    tryCatch({
    df <- upcoming_player_props()
      if (nrow(df) == 0) {
        # Check if it's a data availability issue
        selected_game <- sched_all %>% filter(game_id == input$prop_matchup)
        if (nrow(selected_game) > 0) {
          week_num <- selected_game$week[1]
          message <- paste0("No player stats data available for Week ", week_num, 
                           ". Player stats are currently only available for Weeks 1-4.")
        } else {
          message <- "No player props data available for the selected matchup"
        }
        return(DT::datatable(data.frame(Message = message), rownames = FALSE))
      }
    disp <- df
    names(disp) <- gsub("_", " ", tools::toTitleCase(names(disp)))
    DT::datatable(disp, options = list(scrollX = TRUE), rownames = FALSE) %>%
      DT::formatPercentage("Prob Over", digits = 0)
    }, error = function(e) {
      # Return empty table with error message
      DT::datatable(data.frame(Error = paste("Unable to load player props data:", e$message)), rownames = FALSE)
    })
  })

  # --- Player Trends Analysis ---
  
  # Load historical data for trends analysis. This is called multiple times
  # per week-selection (once for matchup choices, once for trend content);
  # routing it through the cache (Item 1) means only the first call per
  # cache lifetime actually hits the network.
  load_trends_data <- function(seasons = (current_season - 5):current_season) {
    tryCatch({
      # Load historical data for trend analysis (last 5 seasons through the
      # current one)
      player_stats <- get_player_stats_cached(seasons = seasons)
      schedules <- get_schedules_cached(seasons = seasons)

      # Load rosters for the current season
      rosters <- get_rosters_cached(seasons = current_season)
      
      # Get unique player info from rosters
      roster_info <- rosters %>%
        select(player_id = gsis_id, full_name, position, team) %>%
        distinct(player_id, .keep_all = TRUE)
      
      # Join with player stats - use player_name from stats, full_name from rosters
      player_stats_clean <- player_stats %>%
        left_join(roster_info, by = "player_id") %>%
        filter(!is.na(full_name)) %>%
        mutate(
          opponent = "Overall",
          home_away = "Overall"
        ) %>%
        # Use full_name from rosters for consistency
        select(-player_name) %>%
        rename(player_name = full_name)
      
      list(
        player_stats = player_stats_clean,
        schedules = schedules,
        rosters = rosters
      )
    }, error = function(e) {
      warning("Could not load trends data: ", e$message)
      list(player_stats = NULL, schedules = NULL, rosters = NULL)
    })
  }
  
  # Analyze player vs specific opponent trends
  analyze_player_vs_opponent <- function(player_name, opponent_team, prop_type, games_back = 5) {
    trends_data <- load_trends_data()
    if (is.null(trends_data$player_stats)) return(NULL)
    
    # Get player's games vs this specific opponent
    player_games <- trends_data$player_stats %>%
      filter(full_name == player_name, opponent == opponent_team) %>%
      arrange(desc(week)) %>%
      head(games_back)
    
    if (nrow(player_games) == 0) return(NULL)
    
    # Calculate prop-specific stats
    prop_values <- case_when(
      prop_type == "Anytime TD" ~ player_games$rushing_tds + player_games$receiving_tds,
      prop_type == "Passing Yards" ~ player_games$passing_yards,
      prop_type == "Rushing Yards" ~ player_games$rushing_yards,
      prop_type == "Receiving Yards" ~ player_games$receiving_yards,
      prop_type == "Receptions" ~ player_games$receptions,
      prop_type == "Passing TDs" ~ player_games$passing_tds,
      prop_type == "Rushing TDs" ~ player_games$rushing_tds,
      prop_type == "Receiving TDs" ~ player_games$receiving_tds,
      TRUE ~ 0
    )
    
    # For anytime TD, convert to binary (hit/miss)
    if (prop_type == "Anytime TD") {
      hits <- sum(prop_values > 0, na.rm = TRUE)
      total_games <- sum(!is.na(prop_values))
      hit_rate <- ifelse(total_games > 0, hits / total_games, 0)
      
      return(list(
        player = player_name,
        opponent = opponent_team,
        prop_type = prop_type,
        games_analyzed = total_games,
        hits = hits,
        hit_rate = hit_rate,
        trend_text = paste0("Hit in ", hits, " of last ", total_games, " games vs ", opponent_team),
        recent_form = ifelse(hits >= total_games * 0.6, "Hot", ifelse(hits >= total_games * 0.4, "Average", "Cold"))
      ))
    } else {
      # For yardage/reception props
      avg_value <- mean(prop_values, na.rm = TRUE)
      recent_avg <- ifelse(nrow(player_games) >= 3, 
                          mean(prop_values[1:min(3, length(prop_values))], na.rm = TRUE),
                          avg_value)
      
      return(list(
        player = player_name,
        opponent = opponent_team,
        prop_type = prop_type,
        games_analyzed = nrow(player_games),
        avg_value = avg_value,
        recent_avg = recent_avg,
        trend_text = paste0("Averaging ", round(avg_value, 1), " in last ", nrow(player_games), " games vs ", opponent_team),
        recent_form = ifelse(recent_avg > avg_value * 1.1, "Hot", ifelse(recent_avg < avg_value * 0.9, "Cold", "Average"))
      ))
    }
  }
  
  # Analyze home/away specific trends
  analyze_home_away_trends <- function(player_name, prop_type, home_away = "both", games_back = 6) {
    trends_data <- load_trends_data()
    if (is.null(trends_data$player_stats)) {
      return(NULL)
    }
    
    # Get player's recent games from historical data
    player_games <- trends_data$player_stats %>%
      filter(player_name == !!player_name) %>%
      arrange(desc(season), desc(week)) %>%
      head(games_back)
    
    if (nrow(player_games) == 0) {
      return(NULL)
    }
    
    # Calculate prop-specific stats
    prop_values <- case_when(
      prop_type == "Anytime TD" ~ player_games$rushing_tds + player_games$receiving_tds,
      prop_type == "Passing Yards" ~ player_games$passing_yards,
      prop_type == "Rushing Yards" ~ player_games$rushing_yards,
      prop_type == "Receiving Yards" ~ player_games$receiving_yards,
      prop_type == "Receptions" ~ player_games$receptions,
      prop_type == "Passing TDs" ~ player_games$passing_tds,
      prop_type == "Rushing TDs" ~ player_games$rushing_tds,
      prop_type == "Receiving TDs" ~ player_games$receiving_tds,
      TRUE ~ 0
    )
    
    location_text <- "recent games"
    
    # Remove NA values
    prop_values <- prop_values[!is.na(prop_values)]
    
    if (length(prop_values) == 0) return(NULL)
    
    # For anytime TD, convert to binary (hit/miss)
    if (prop_type == "Anytime TD") {
      hits <- sum(prop_values > 0, na.rm = TRUE)
      total_games <- length(prop_values)
      hit_rate <- ifelse(total_games > 0, hits / total_games, 0)
      
      return(list(
        recent_avg = hit_rate,
        recent_hits = hits,
        games_analyzed = total_games,
        recent_form = ifelse(hits >= total_games * 0.6, "Hot", ifelse(hits >= total_games * 0.4, "Average", "Cold"))
      ))
    } else {
      # For yardage/reception props
      recent_avg <- mean(prop_values, na.rm = TRUE)
      avg_value <- recent_avg  # Use recent average as baseline
      
      return(list(
        recent_avg = recent_avg,
        recent_hits = NA,
        games_analyzed = length(prop_values),
        recent_form = ifelse(recent_avg > avg_value * 1.1, "Hot", ifelse(recent_avg < avg_value * 0.9, "Cold", "Average"))
      ))
    }
  }
  
  # Get all trending players for upcoming games
  get_upcoming_trends <- function(week = 5) {
    trends_data <- load_trends_data()
    if (is.null(trends_data$player_stats) || is.null(trends_data$schedules)) {
      warning("Trends data is null")
      return(data.frame())
    }
    
    # Get current-season schedule data
    current_schedule <- trends_data$schedules %>%
      filter(season == current_season, week == !!week) %>%
      select(game_id, home_team, away_team, week, season)
    
    if (nrow(current_schedule) == 0) {
      warning("No schedule data found for week ", week)
      return(data.frame())
    }
    
    # Get current rosters for teams playing this week
    teams_playing <- unique(c(current_schedule$home_team, current_schedule$away_team))
    
    upcoming_players <- trends_data$rosters %>%
      filter(team %in% teams_playing, 
             position %in% c("QB", "RB", "WR", "TE")) %>%
      head(15)  # Limit for performance
    
    if (nrow(upcoming_players) == 0) {
      warning("No players found for teams: ", paste(teams_playing, collapse = ", "))
      return(data.frame())
    }
    
    # Analyze trends for each player
    all_trends <- list()
    
    for (i in 1:nrow(upcoming_players)) {
      player <- upcoming_players$full_name[i]
      player_team <- upcoming_players$team[i]
      player_position <- upcoming_players$position[i]
      
      # Get opponent for this player's team
      player_game <- current_schedule %>%
        filter(home_team == player_team | away_team == player_team) %>%
        slice(1)
      
      if (nrow(player_game) == 0) next
      
      opponent <- if (player_game$home_team == player_team) {
        player_game$away_team
      } else {
        player_game$home_team
      }
      
      # Analyze different prop types based on position
      prop_types <- case_when(
        player_position == "QB" ~ c("Passing Yards", "Passing TDs", "Rushing Yards"),
        player_position == "RB" ~ c("Rushing Yards", "Receiving Yards", "Anytime TD"),
        player_position %in% c("WR", "TE") ~ c("Receiving Yards", "Receptions", "Anytime TD"),
        TRUE ~ c("Anytime TD")
      )
      
      for (prop_type in prop_types) {
        ha_trend <- analyze_home_away_trends(player, prop_type, "both", 6)
        
        if (!is.null(ha_trend) && ha_trend$games_analyzed >= 1) {
          trend_text <- case_when(
            prop_type %in% c("Passing Yards", "Rushing Yards", "Receiving Yards", "Receptions") ~
              paste0("Averaging ", round(ha_trend$recent_avg, 1), " in last ", ha_trend$games_analyzed, " recent games"),
            prop_type %in% c("Passing TDs", "Anytime TD") ~
              paste0("Hit in ", ha_trend$recent_hits, " of last ", ha_trend$games_analyzed, " recent games"),
            TRUE ~ paste0("Recent form: ", ha_trend$recent_avg, " average")
          )
          
          all_trends[[length(all_trends) + 1]] <- data.frame(
            Player = player,
            Opponent = paste0("vs ", opponent),
            Prop_Type = prop_type,
            Games_Analyzed = ha_trend$games_analyzed,
            Trend_Text = trend_text,
            Recent_Form = ha_trend$recent_form,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(all_trends) == 0) {
      warning("No trends found for any players")
      return(data.frame())
    }
    
    trends_df <- do.call(rbind, all_trends)
    
    # Add trend strength
    trends_df$Trend_Strength <- case_when(
      trends_df$Recent_Form == "Hot" & trends_df$Games_Analyzed >= 5 ~ "Strong",
      trends_df$Recent_Form == "Hot" & trends_df$Games_Analyzed >= 3 ~ "Moderate", 
      trends_df$Recent_Form == "Cold" & trends_df$Games_Analyzed >= 5 ~ "Strong",
      trends_df$Recent_Form == "Cold" & trends_df$Games_Analyzed >= 3 ~ "Moderate",
      TRUE ~ "Weak"
    )
    
    return(trends_df)
  }
  
  # Get available matchups for the selected week
  get_available_matchups <- function(week) {
    if (is.null(week)) return(list("All Games" = "all"))
    
    trends_data <- load_trends_data()
    if (is.null(trends_data$schedules)) {
      return(list("All Games" = "all"))
    }
    
    # Get the current-season schedule for the specified week
    week_games <- trends_data$schedules %>%
      filter(season == current_season, week == !!week) %>%
      select(home_team, away_team, game_id)
    
    if (nrow(week_games) == 0) {
      return(list("All Games" = "all"))
    }
    
    # Create matchup list
    matchups <- list("All Games" = "all")
    
    for (i in 1:nrow(week_games)) {
      game <- week_games[i, ]
      matchup_name <- paste0(game$away_team, " @ ", game$home_team)
      matchup_id <- paste0(game$away_team, "_", game$home_team)
      matchups[[matchup_name]] <- matchup_id
    }
    
    return(matchups)
  }
  
  # Update matchup choices when week changes
  observe({
    if (!is.null(input$trends_week)) {
      matchups <- get_available_matchups(input$trends_week)
      updateSelectInput(session, "trends_matchup", choices = matchups, selected = "all")
    }
  })
  
  # Reactive for trends data
  trends_data <- reactive({
    req(input$trends_week)
    get_upcoming_trends(input$trends_week)
  })
  
  # Filtered trends data based on user selection
  filtered_trends_data <- reactive({
    req(trends_data())
    df <- trends_data()
    
    if (is.null(df) || nrow(df) == 0) return(df)
    
    # Filter by specific matchup if selected
    if (input$trends_matchup != "all") {
      # Extract teams from matchup selection
      teams <- strsplit(input$trends_matchup, "_")[[1]]
      if (length(teams) == 2) {
        # Filter trends to only show players from this specific matchup
        df <- df %>% filter(
          grepl(teams[1], Opponent) | grepl(teams[2], Opponent)
        )
      }
    }
    
    # Apply form filter
    if (input$trends_filter == "hot") {
      df <- df %>% filter(Recent_Form == "Hot")
    } else if (input$trends_filter == "cold") {
      df <- df %>% filter(Recent_Form == "Cold")
    }
    # "all" doesn't need filtering
    
    return(df)
  })
  
  # Render trends table
  output$trends_table <- DT::renderDataTable({
    tryCatch({
      df <- filtered_trends_data()
      if (is.null(df) || nrow(df) == 0) {
        filter_msg <- ifelse(input$trends_filter == "hot", "hot trending", 
                            ifelse(input$trends_filter == "cold", "cold trending", "trending"))
        return(DT::datatable(data.frame(Message = paste0("No ", filter_msg, " data available for the selected week")), rownames = FALSE))
      }
      
      # Format the data for display
      display_df <- df %>%
        select(Player, Opponent, Prop_Type, Games_Analyzed, Trend_Text, Recent_Form, Trend_Strength) %>%
        mutate(
          Prop_Type = case_when(
            Prop_Type == "anytime_td" ~ "Anytime TD",
            Prop_Type == "passing_yards" ~ "Passing Yards",
            Prop_Type == "rushing_yards" ~ "Rushing Yards", 
            Prop_Type == "receiving_yards" ~ "Receiving Yards",
            Prop_Type == "receptions" ~ "Receptions",
            Prop_Type == "passing_tds" ~ "Passing TDs",
            Prop_Type == "rushing_tds" ~ "Rushing TDs",
            Prop_Type == "receiving_tds" ~ "Receiving TDs",
            TRUE ~ Prop_Type
          ),
          Confidence = case_when(
            Games_Analyzed >= 5 ~ "High",
            Games_Analyzed >= 3 ~ "Medium", 
            TRUE ~ "Low"
          )
        ) %>%
        select(Player, Opponent, Prop_Type, Games_Analyzed, Confidence, Trend_Strength, Trend_Text, Recent_Form)
      
      DT::datatable(display_df, 
                   options = list(scrollX = TRUE, pageLength = 20), 
                   rownames = FALSE) %>%
        DT::formatStyle("Recent_Form", 
                       backgroundColor = DT::styleEqual(c("Hot", "Average", "Cold"), 
                                                       c("lightgreen", "lightyellow", "lightcoral")),
                       fontWeight = "bold") %>%
        DT::formatStyle("Confidence",
                       backgroundColor = DT::styleEqual(c("High", "Medium", "Low"),
                                                       c("lightblue", "lightyellow", "lightpink")),
                       fontWeight = "bold") %>%
        DT::formatStyle("Trend_Strength",
                       backgroundColor = DT::styleEqual(c("Strong", "Moderate", "Weak"),
                                                       c("lightgreen", "lightyellow", "lightgray")),
                       fontWeight = "bold")
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Unable to load trends data:", e$message)), rownames = FALSE)
    })
  })

  # Other modules & navigation
  source("modules/server_player_stats.R", local = TRUE)
  source("modules/server_team_stats.R", local = TRUE)
  server_team_stats(input, output, session)

  observeEvent(input$go_team_stats, current_page("team_stats"))
  observeEvent(input$go_player_stats, current_page("player_stats"))
  observeEvent(input$go_betting, current_page("sports_betting"))
  observeEvent(input$back_home, current_page("home"))
  observeEvent(input$back_team, current_page("team"))
  observeEvent(input$back_position, current_page("position"))

  observe({
    teams <- get_cached_data("teams", function() nflreadr::load_teams())
    lapply(teams$team_abbr, function(tb) {
      observeEvent(input[[paste0("team_", tb)]], {
        selected_team(tb)
        current_page("team")
      })
    })
  })

  observe({
    all_pos <- c(offense_positions, defense_positions)
    lapply(all_pos, function(pos) {
      observeEvent(input[[paste0("pos_", pos)]], {
        selected_position(pos)
        current_page("position")
      })
    })
  })

  observe({
    # Uses the roster season (mid-March cutover), not the stats season --
    # players should be clickable as soon as they're on a current roster,
    # even before that season's games have started.
    rosters <- get_rosters_cached(seasons = current_roster_season)
    lapply(rosters$gsis_id, function(pid) {
      observeEvent(input[[paste0("player_", pid)]], {
        selected_player(pid)
        current_page("player")
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui = ui, server = server)
