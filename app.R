# app.R

library(shiny)
library(nflreadr)
library(bslib)
library(DT)
library(lubridate)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)

# Load local secrets (ODDS_API_KEY, etc.) from .Renviron if present.
# .Renviron is git-ignored — see .Renviron.example for the template.
if (file.exists(".Renviron")) readRenviron(".Renviron")

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
  sched_all <- nflreadr::load_schedules(seasons = 2025) %>%
    distinct(game_id, .keep_all = TRUE)
  team_stats_all <- nflreadr::load_team_stats(seasons = 2025)

  team_summary <- team_stats_all %>%
    group_by(team, season) %>%
    summarise(
      avg_passing_yards = mean(passing_yards, na.rm = TRUE),
      avg_rushing_yards = mean(rushing_yards, na.rm = TRUE),
      avg_completions = mean(completions, na.rm = TRUE),
      avg_attempts = mean(attempts, na.rm = TRUE),
      avg_def_tackles = mean(def_tackles_solo + def_tackles_with_assist, na.rm = TRUE),
      avg_def_sacks = mean(def_sacks, na.rm = TRUE),
      avg_def_interceptions = mean(def_interceptions, na.rm = TRUE),
      avg_passes_defended = mean(def_pass_defended, na.rm = TRUE),
      avg_total_offense = avg_passing_yards + avg_rushing_yards,
      avg_total_defense = avg_def_tackles + avg_def_sacks + avg_def_interceptions,
      .groups = "drop"
    )

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
    resp <- GET(
      url = paste0(odds_base, "/sports/americanfootball_nfl/odds"),
      query = list(
        apiKey = odds_api_key,
        regions = "us",
        markets = "h2h,spreads,totals",
        oddsFormat = "american"
      )
    )
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

  fetch_prop_odds <- function(game_id) {
    # First, we need to get the API event ID from our game ID
    # The API uses different event IDs than our schedule data
    # Let's get all available games from the API and match by teams
    
    # Get the game info from our schedule
    game_info <- sched_all %>% filter(game_id == game_id)
    if (nrow(game_info) == 0) {
      return(list())
    }
    
    # Get all available games from API
    api_resp <- GET(
      url = paste0(odds_base, "/sports/americanfootball_nfl/odds"),
      query = list(
        apiKey = odds_api_key,
        regions = "us",
        markets = "h2h",
        oddsFormat = "american"
      )
    )
    
    if (status_code(api_resp) != 200) {
      warning("fetch_prop_odds: Failed to get API games")
      return(list())
    }
    
    api_games <- fromJSON(content(api_resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
    
    # Find matching game by team names (handle team name differences)
    # Convert team abbreviations to full names for matching
    team_name_map <- list(
      "DAL" = "Dallas Cowboys", "PHI" = "Philadelphia Eagles", "KC" = "Kansas City Chiefs",
      "LAC" = "Los Angeles Chargers", "ATL" = "Atlanta Falcons", "TB" = "Tampa Bay Buccaneers",
      "CLE" = "Cleveland Browns", "CIN" = "Cincinnati Bengals", "IND" = "Indianapolis Colts",
      "MIA" = "Miami Dolphins", "JAX" = "Jacksonville Jaguars", "CAR" = "Carolina Panthers",
      "NE" = "New England Patriots", "LV" = "Las Vegas Raiders", "NO" = "New Orleans Saints",
      "ARI" = "Arizona Cardinals", "NYJ" = "New York Jets", "PIT" = "Pittsburgh Steelers",
      "WAS" = "Washington Commanders", "NYG" = "New York Giants", "GB" = "Green Bay Packers",
      "SF" = "San Francisco 49ers", "LAR" = "Los Angeles Rams", "MIN" = "Minnesota Vikings",
      "DET" = "Detroit Lions", "BUF" = "Buffalo Bills", "SEA" = "Seattle Seahawks",
      "TEN" = "Tennessee Titans", "DEN" = "Denver Broncos"
    )
    
    away_full <- team_name_map[[game_info$away_team[1]]] %||% game_info$away_team[1]
    home_full <- team_name_map[[game_info$home_team[1]]] %||% game_info$home_team[1]
    
    matching_game <- api_games %>%
      filter(
        (away_team == away_full & home_team == home_full) |
        (away_team == home_full & home_team == away_full)
      )
    
    if (nrow(matching_game) == 0) {
      warning("fetch_prop_odds: No matching game found in API for ", game_info$away_team[1], " vs ", game_info$home_team[1])
      # Fallback: use any available game for demonstration
      if (nrow(api_games) > 0) {
        cat("Using fallback game for demonstration:", api_games$away_team[1], "vs", api_games$home_team[1], "\n")
        matching_game <- api_games[1, ]
      } else {
        return(list())
      }
    }
    
    api_event_id <- matching_game$id[1]
    cat("Found API event ID:", api_event_id, "for game", game_info$away_team[1], "vs", game_info$home_team[1], "\n")
    
    # Now get player props for the API event ID
    # Use the correct market names that we know work
    available_markets <- c("player_receptions", "player_pass_yds", "player_rush_yds")
    
    all_props <- list()
    
    for (market in available_markets) {
      resp <- GET(
        url = paste0(odds_base, "/sports/americanfootball_nfl/events/", api_event_id, "/odds"),
        query = list(
          apiKey = odds_api_key,
          regions = "us",
          markets = market,
          oddsFormat = "american"
        )
      )
      
      if (status_code(resp) == 200) {
    txt <- content(resp, as = "text", encoding = "UTF-8")
    parsed <- tryCatch({
      fromJSON(txt, flatten = FALSE)
    }, error = function(e) {
          warning("fetch_prop_odds: JSON parse error for ", market, ": ", e$message)
      list()
    })
        
        # Extract prop data from DraftKings (most reliable)
        if (length(parsed) > 0 && "bookmakers" %in% names(parsed)) {
          # Find DraftKings bookmaker (bookmakers is a list)
          dk_idx <- which(sapply(parsed$bookmakers, function(x) x$key == "draftkings"))
          if (length(dk_idx) > 0) {
            dk_bookmaker <- parsed$bookmakers[[dk_idx]]
            # Find the market we're looking for
            market_idx <- which(sapply(dk_bookmaker$markets, function(x) x$key == market))
            if (length(market_idx) > 0) {
              market_data <- dk_bookmaker$markets[[market_idx]]$outcomes
              if (length(market_data) > 0) {
                # Convert to data frame and add market type
                market_df <- do.call(rbind, lapply(market_data, function(x) {
                  data.frame(
                    player_name = x$description,
                    prop_type = x$name,
                    prop_line = x$point,
                    price = x$price,
                    market_type = market,
                    stringsAsFactors = FALSE
                  )
                }))
                all_props[[market]] <- market_df
              }
            }
          }
        }
      } else {
        warning("fetch_prop_odds: HTTP status ", status_code(resp), " for market ", market)
      }
    }
    
    all_props
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

  predict_game_edge <- function(row) {
    # Check if we have team stats data
    if (!all(c("avg_total_offense_home", "avg_total_defense_away", 
               "avg_total_offense_away", "avg_total_defense_home") %in% names(row))) {
      return("No Data")
    }
    
    pred_margin <- (row$avg_total_offense_home - row$avg_total_defense_away) -
                   (row$avg_total_offense_away - row$avg_total_defense_home)
    pred_total <- (row$avg_total_offense_home + row$avg_total_offense_away) / 2
    
    if (!is.na(row$spread_line) && abs(pred_margin - row$spread_line) > 3) {
      return(paste0("Spread: ", if (pred_margin > row$spread_line) "Home" else "Away"))
    }
    if (!is.na(row$total_line) && abs(pred_total - row$total_line) > 7) {
      return(paste0("O/U: ", if (pred_total > row$total_line) "Over" else "Under"))
    }
    return("ML")
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
      filter(season == 2025, game_type == "REG", week == input$prop_week) %>%
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

  # Load additional data sources for advanced predictions
  load_advanced_data <- function(season = 2020:2025) {
    tryCatch({
      # Load play-by-play data for situational analysis
      pbp_data <- nflreadr::load_pbp(seasons = season)
      
      # Load schedule for weather and game context
      schedule_data <- nflreadr::load_schedules(seasons = season)
      
      # Load snap counts for role analysis
      snap_data <- nflreadr::load_snap_counts(seasons = season)
      
      list(
        pbp = pbp_data,
        schedule = schedule_data,
        snaps = snap_data
      )
    }, error = function(e) {
      warning("Could not load advanced data: ", e$message)
      list(pbp = NULL, schedule = NULL, snaps = NULL)
    })
  }
  
  # Get game context (weather, etc.)
  get_game_context <- function(game_id, schedule_data) {
    if (is.null(schedule_data)) return(NULL)
    
    game_info <- schedule_data %>% filter(game_id == !!game_id)
    if (nrow(game_info) == 0) return(NULL)
    
    list(
      weather = game_info$weather[1],
      temperature = game_info$temp[1],
      wind = game_info$wind[1],
      dome = game_info$roof[1] == "dome"
    )
  }
  
  # Analyze player vs specific opponent
  get_player_vs_opponent_stats <- function(player_id, opponent_team, historical_stats, seasons = 2025) {
    if (is.null(historical_stats)) return(NULL)
    
    # This would require game-level data to match players vs specific opponents
    # For now, return NULL but this could be enhanced with more detailed data
    return(NULL)
  }
  
  # Advanced prediction algorithms with ML-style feature engineering
  predict_qb_passing_yards <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    # Use last 5 games for better trend analysis
    recent_games <- player_stats %>% 
      filter(season_type == "REG") %>%
      arrange(desc(week)) %>% 
      head(5)
    
    if (nrow(recent_games) == 0) return(200) # Default fallback
    
    # Calculate multiple base metrics
    base_avg <- mean(recent_games$passing_yards, na.rm = TRUE)
    recent_trend <- if (nrow(recent_games) >= 3) {
      mean(recent_games$passing_yards[1:2], na.rm = TRUE) - mean(recent_games$passing_yards[3:5], na.rm = TRUE)
    } else 0
    
    # Consistency factor (lower std dev = higher consistency)
    consistency <- 1 - (sd(recent_games$passing_yards, na.rm = TRUE) / max(mean(recent_games$passing_yards, na.rm = TRUE), 1))
    consistency <- max(0.5, min(1.2, consistency))
    
    # Team offensive strength vs opponent defensive weakness
    team_pass_off <- team_stats$passing_yards_per_game[1] %||% 230
    opp_pass_def <- opponent_stats$passing_yards_allowed_per_game[1] %||% 230
    league_avg <- 230
    
    # Matchup factor: team offense vs opponent defense
    matchup_factor <- (team_pass_off / league_avg) * (league_avg / opp_pass_def)
    matchup_factor <- max(0.8, min(1.3, matchup_factor))
    
    # Home field advantage (slightly stronger)
    home_factor <- ifelse(home_away == "home", 1.03, 0.97)
    
    # Game script factor (based on team records)
    team_win_pct <- team_stats$win_percentage[1] %||% 0.5
    opp_win_pct <- opponent_stats$win_percentage[1] %||% 0.5
    game_script <- ifelse(team_win_pct < opp_win_pct, 1.05, 0.95) # Underdog throws more
    
    # Weather factor (if available)
    weather_factor <- 1.0
    if (!is.null(game_context) && !is.null(game_context$weather)) {
      if (grepl("rain|snow|wind", tolower(game_context$weather))) {
        weather_factor <- 0.9
      }
    }
    
    # Combine all factors
    prediction <- base_avg * consistency * matchup_factor * home_factor * game_script * weather_factor
    
    # Apply trend adjustment (recent performance matters more)
    prediction <- prediction + (recent_trend * 0.3)
    
    # Realistic bounds
    prediction <- max(150, min(450, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_qb_rushing_yards <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    recent_games <- player_stats %>% arrange(desc(week)) %>% head(4)
    base_avg <- mean(recent_games$rushing_yards, na.rm = TRUE)
    
    # If no recent data, use career average
    if (is.na(base_avg) || base_avg == 0) {
      base_avg <- mean(player_stats$rushing_yards, na.rm = TRUE)
    }
    
    # Conservative team factors (max 15% adjustment)
    team_rush_off <- team_stats$rushing_yards_per_game[1] %||% 120
    league_avg <- 120
    team_factor <- min(1.15, max(0.85, team_rush_off / league_avg))
    
    # Conservative opponent factor (max 10% adjustment)
    opp_rush_def <- opponent_stats$rushing_yards_allowed_per_game[1] %||% 120
    opp_factor <- min(1.1, max(0.9, league_avg / opp_rush_def))
    
    home_factor <- ifelse(home_away == "home", 1.01, 0.99)
    
    prediction <- base_avg * team_factor * opp_factor * home_factor
    
    # Apply realistic bounds for QB rushing yards (0-80 yards)
    prediction <- max(0, min(80, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_qb_passing_tds <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    recent_games <- player_stats %>% arrange(desc(week)) %>% head(4)
    base_avg <- mean(recent_games$passing_tds, na.rm = TRUE)
    
    # If no recent data, use career average
    if (is.na(base_avg) || base_avg == 0) {
      base_avg <- mean(player_stats$passing_tds, na.rm = TRUE)
    }
    
    # Conservative team factor (max 15% adjustment)
    team_pass_off <- team_stats$passing_yards_per_game[1] %||% 230
    league_avg <- 230
    team_factor <- min(1.15, max(0.85, team_pass_off / league_avg))
    
    # Conservative opponent factor (max 10% adjustment)
    opp_pass_def <- opponent_stats$passing_yards_allowed_per_game[1] %||% 230
    opp_factor <- min(1.1, max(0.9, league_avg / opp_pass_def))
    
    home_factor <- ifelse(home_away == "home", 1.05, 0.95)
    
    prediction <- base_avg * team_factor * opp_factor * home_factor
    
    # Apply realistic bounds for QB passing TDs (0-6 TDs)
    prediction <- max(0, min(6, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_qb_passing_attempts <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    recent_games <- player_stats %>% arrange(desc(week)) %>% head(4)
    base_avg <- mean(recent_games$attempts, na.rm = TRUE)
    
    # If no recent data, use career average
    if (is.na(base_avg) || base_avg == 0) {
      base_avg <- mean(player_stats$attempts, na.rm = TRUE)
    }
    
    # Conservative game script factor (max 10% adjustment)
    team_win_pct <- team_stats$win_percentage[1] %||% 0.5
    opp_win_pct <- opponent_stats$win_percentage[1] %||% 0.5
    game_script_factor <- ifelse(team_win_pct < opp_win_pct, 1.1, 0.9)
    
    # Conservative team factor (max 10% adjustment)
    team_pass_att <- team_stats$pass_attempts_per_game[1] %||% 35
    league_avg_att <- 35
    team_factor <- min(1.1, max(0.9, team_pass_att / league_avg_att))
    
    home_factor <- ifelse(home_away == "home", 1.02, 0.98)
    
    prediction <- base_avg * game_script_factor * team_factor * home_factor
    
    # Apply realistic bounds for QB passing attempts (15-50 attempts)
    prediction <- max(15, min(50, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_wr_receptions <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    # Use last 6 games for better role detection and trend analysis
    recent_games <- player_stats %>% 
      filter(season_type == "REG") %>%
      arrange(desc(week)) %>% 
      head(6)
    
    if (nrow(recent_games) == 0) return(1.0)
    
    # Calculate advanced metrics
    recent_receptions <- recent_games$receptions
    recent_targets <- recent_games$targets
    recent_yards <- recent_games$receiving_yards
    
    # Games where player was active (had targets)
    active_games <- recent_games[recent_targets > 0, ]
    games_played <- nrow(active_games)
    
    if (games_played == 0) return(0.5)
    
    # Role detection based on multiple factors
    avg_targets <- mean(active_games$targets, na.rm = TRUE)
    avg_receptions <- mean(active_games$receptions, na.rm = TRUE)
    target_share <- avg_targets / 35  # Assuming 35 team targets per game
    catch_rate <- mean(active_games$receptions / active_games$targets, na.rm = TRUE)
    
    # Trend analysis (last 3 vs previous 3)
    if (nrow(active_games) >= 4) {
      last_half <- mean(active_games$receptions[1:min(3, nrow(active_games))], na.rm = TRUE)
      first_half <- mean(active_games$receptions[max(1, nrow(active_games)-2):nrow(active_games)], na.rm = TRUE)
      trend_factor <- ifelse(first_half > 0, last_half / first_half, 1)
    } else {
      trend_factor <- 1
    }
    
    # Consistency factor (lower variance = higher consistency)
    consistency <- 1 - (sd(active_games$receptions, na.rm = TRUE) / max(mean(active_games$receptions, na.rm = TRUE), 1))
    consistency <- max(0.6, min(1.3, consistency))
    
    # Role-based base prediction
    if (avg_targets >= 8) {
      # WR1 - high target share
      base_avg <- avg_receptions * 0.9
    } else if (avg_targets >= 5) {
      # WR2 - moderate target share
      base_avg <- avg_receptions * 0.8
    } else if (avg_targets >= 3) {
      # WR3 - low target share
      base_avg <- avg_receptions * 0.7
    } else if (avg_targets >= 1) {
      # WR4+ - very low target share
      base_avg <- avg_receptions * 0.5
    } else {
      base_avg <- 0.5
    }
    
    # Team passing volume factor
    team_pass_att <- team_stats$pass_attempts_per_game[1] %||% 35
    league_avg_att <- 35
    team_volume_factor <- team_pass_att / league_avg_att
    
    # Opponent pass defense factor
    opp_pass_def <- opponent_stats$passing_yards_allowed_per_game[1] %||% 230
    league_avg_def <- 230
    opp_def_factor <- league_avg_def / opp_pass_def
    
    # Target share adjustment based on team's passing tendencies
    team_target_share <- target_share * team_volume_factor
    
    # Home field advantage
    home_factor <- ifelse(home_away == "home", 1.02, 0.98)
    
    # Game script factor (teams behind throw more)
    team_win_pct <- team_stats$win_percentage[1] %||% 0.5
    opp_win_pct <- opponent_stats$win_percentage[1] %||% 0.5
    game_script <- ifelse(team_win_pct < opp_win_pct, 1.03, 0.97)
    
    # Weather factor
    weather_factor <- 1.0
    if (!is.null(game_context) && !is.null(game_context$weather)) {
      if (grepl("rain|snow|wind", tolower(game_context$weather))) {
        weather_factor <- 0.95  # Slightly lower in bad weather
      }
    }
    
    # Combine all factors
    prediction <- base_avg * consistency * team_target_share * opp_def_factor * 
                  home_factor * game_script * weather_factor * trend_factor
    
    # Apply play probability (based on recent activity)
    play_probability <- min(1.0, games_played / 6)
    prediction <- prediction * play_probability
    
    # Realistic bounds
    prediction <- max(0, min(10, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_wr_receiving_yards <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    # Use only regular season data from current season
    recent_games <- player_stats %>% 
      filter(season_type == "REG") %>%
      arrange(desc(week)) %>% 
      head(5)  # Look at last 5 games for better role detection
    
    # Calculate role-based prediction with trend analysis
    recent_yards <- recent_games$receiving_yards
    recent_targets <- recent_games$targets
    games_played <- sum(recent_yards > 0 | recent_targets > 0, na.rm = TRUE)
    avg_yards_when_active <- mean(recent_yards[recent_yards > 0], na.rm = TRUE)
    avg_targets_when_active <- mean(recent_targets[recent_targets > 0], na.rm = TRUE)
    
    # Check for declining trend (last 2 games vs previous 3)
    if (nrow(recent_games) >= 3) {
      last_2_games <- mean(recent_games$receiving_yards[1:2], na.rm = TRUE)
      prev_3_games <- mean(recent_games$receiving_yards[3:5], na.rm = TRUE)
      trend_factor <- ifelse(prev_3_games > 0, last_2_games / prev_3_games, 1)
    } else {
      trend_factor <- 1
    }
    
    # Determine role based on usage patterns with much stricter thresholds
    if (games_played == 0) {
      # Player hasn't played recently - very low prediction
      base_avg <- 1
    } else if (avg_targets_when_active > 10) {
      # WR1 - high usage
      base_avg <- avg_yards_when_active * 0.85 * trend_factor
    } else if (avg_targets_when_active > 6) {
      # WR2 - moderate usage
      base_avg <- avg_yards_when_active * 0.75 * trend_factor
    } else if (avg_targets_when_active > 3) {
      # WR3 - low usage
      base_avg <- avg_yards_when_active * 0.6 * trend_factor
    } else if (avg_targets_when_active > 1) {
      # WR4 - very low usage
      base_avg <- avg_yards_when_active * 0.4 * trend_factor
    } else {
      # WR5 or rarely used - minimal usage
      base_avg <- max(1, avg_yards_when_active * 0.2 * trend_factor)
    }
    
    # Apply probability of playing (based on recent games played)
    play_probability <- min(1.0, games_played / 5)
    base_avg <- base_avg * play_probability
    
    # Apply additional penalty for players with declining usage
    if (trend_factor < 0.5) {
      base_avg <- base_avg * 0.6  # Additional 40% penalty for declining usage
    }
    
    # Very conservative adjustments (max 3% total)
    team_ypt <- team_stats$yards_per_attempt[1] %||% 7.5
    league_avg_ypt <- 7.5
    team_factor <- min(1.01, max(0.99, team_ypt / league_avg_ypt))
    
    home_factor <- ifelse(home_away == "home", 1.005, 0.995)
    
    prediction <- base_avg * team_factor * home_factor
    
    # Apply realistic bounds for WR receiving yards (0-100 yards)
    prediction <- max(0, min(100, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_rb_rushing_attempts <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    # Use only regular season data from current season
    recent_games <- player_stats %>% 
      filter(season_type == "REG") %>%
      arrange(desc(week)) %>% 
      head(5)  # Look at last 5 games for better role detection
    
    # Calculate role-based prediction with trend analysis
    recent_carries <- recent_games$carries
    games_played <- sum(recent_carries > 0, na.rm = TRUE)
    avg_carries_when_active <- mean(recent_carries[recent_carries > 0], na.rm = TRUE)
    
    # Check for declining trend (last 2 games vs previous 3)
    if (nrow(recent_games) >= 3) {
      last_2_games <- mean(recent_games$carries[1:2], na.rm = TRUE)
      prev_3_games <- mean(recent_games$carries[3:5], na.rm = TRUE)
      trend_factor <- ifelse(prev_3_games > 0, last_2_games / prev_3_games, 1)
    } else {
      trend_factor <- 1
    }
    
    # Determine role based on usage patterns with much stricter thresholds
    if (games_played == 0) {
      # Player hasn't played recently - very low prediction
      base_avg <- 0.5
    } else if (avg_carries_when_active > 18) {
      # Workhorse RB - high usage
      base_avg <- avg_carries_when_active * 0.85 * trend_factor
    } else if (avg_carries_when_active > 12) {
      # Starting RB - moderate usage
      base_avg <- avg_carries_when_active * 0.75 * trend_factor
    } else if (avg_carries_when_active > 6) {
      # Backup RB - low usage
      base_avg <- avg_carries_when_active * 0.6 * trend_factor
    } else if (avg_carries_when_active > 2) {
      # Third string - very low usage
      base_avg <- avg_carries_when_active * 0.4 * trend_factor
    } else {
      # Rarely used - minimal usage
      base_avg <- max(0.5, avg_carries_when_active * 0.3 * trend_factor)
    }
    
    # Apply probability of playing (based on recent games played)
    play_probability <- min(1.0, games_played / 5)
    base_avg <- base_avg * play_probability
    
    # Apply additional penalty for players with declining usage
    if (trend_factor < 0.5) {
      base_avg <- base_avg * 0.7  # Additional 30% penalty for declining usage
    }
    
    # Very conservative adjustments (max 3% total)
    team_rush_att <- team_stats$rush_attempts_per_game[1] %||% 25
    league_avg_rush <- 25
    team_factor <- min(1.01, max(0.99, team_rush_att / league_avg_rush))
    
    home_factor <- ifelse(home_away == "home", 1.005, 0.995)
    
    prediction <- base_avg * team_factor * home_factor
    
    # Apply realistic bounds for RB rushing attempts (0-15 attempts)
    prediction <- max(0, min(15, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_rb_rushing_yards <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    # Use only regular season data from current season
    recent_games <- player_stats %>% 
      filter(season_type == "REG") %>%
      arrange(desc(week)) %>% 
      head(5)  # Look at last 5 games for better role detection
    
    # Calculate role-based prediction
    recent_yards <- recent_games$rushing_yards
    recent_carries <- recent_games$carries
    games_played <- sum(recent_yards > 0 | recent_carries > 0, na.rm = TRUE)
    avg_yards_when_active <- mean(recent_yards[recent_yards > 0], na.rm = TRUE)
    avg_carries_when_active <- mean(recent_carries[recent_carries > 0], na.rm = TRUE)
    
    # Determine role based on usage patterns
    if (games_played == 0) {
      # Player hasn't played recently - very low prediction
      base_avg <- 3
    } else if (avg_carries_when_active > 15) {
      # Workhorse RB - high usage
      base_avg <- avg_yards_when_active * 0.9  # Slight regression
    } else if (avg_carries_when_active > 8) {
      # Starting RB - moderate usage
      base_avg <- avg_yards_when_active * 0.8  # Some regression
    } else if (avg_carries_when_active > 3) {
      # Backup RB - low usage
      base_avg <- avg_yards_when_active * 0.7  # More regression
    } else {
      # Third string/rarely used - very low usage
      base_avg <- max(3, avg_yards_when_active * 0.5)  # Heavy regression
    }
    
    # Apply probability of playing (based on recent games played)
    play_probability <- min(1.0, games_played / 5)
    base_avg <- base_avg * play_probability
    
    # Very conservative adjustments (max 5% total)
    team_ypc <- team_stats$yards_per_carry[1] %||% 4.2
    league_avg_ypc <- 4.2
    team_factor <- min(1.02, max(0.98, team_ypc / league_avg_ypc))
    
    home_factor <- ifelse(home_away == "home", 1.01, 0.99)
    
    prediction <- base_avg * team_factor * home_factor
    
    # Apply realistic bounds for RB rushing yards (0-120 yards)
    prediction <- max(0, min(120, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_rb_receiving_yards <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    recent_games <- player_stats %>% arrange(desc(week)) %>% head(4)
    base_avg <- mean(recent_games$receiving_yards, na.rm = TRUE)
    
    # If no recent data, use career average
    if (is.na(base_avg) || base_avg == 0) {
      base_avg <- mean(player_stats$receiving_yards, na.rm = TRUE)
    }
    
    # Conservative target share factor (max 20% adjustment)
    target_share <- mean(recent_games$targets, na.rm = TRUE) / 25  # Use league average
    target_share <- ifelse(is.na(target_share), 0.08, target_share)
    target_factor <- min(1.2, max(0.8, target_share / 0.08))
    
    # Conservative team factor (max 10% adjustment)
    team_pass_att <- team_stats$pass_attempts_per_game[1] %||% 35
    team_factor <- min(1.1, max(0.9, team_pass_att / 35))
    
    home_factor <- ifelse(home_away == "home", 1.02, 0.98)
    
    prediction <- base_avg * target_factor * team_factor * home_factor
    
    # Apply realistic bounds for RB receiving yards (0-100 yards)
    prediction <- max(0, min(100, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_te_receptions <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    recent_games <- player_stats %>% arrange(desc(week)) %>% head(4)
    base_avg <- mean(recent_games$receptions, na.rm = TRUE)
    
    # If no recent data, use career average
    if (is.na(base_avg) || base_avg == 0) {
      base_avg <- mean(player_stats$receptions, na.rm = TRUE)
    }
    
    # Conservative target share factor (max 20% adjustment)
    target_share <- mean(recent_games$targets, na.rm = TRUE) / 25  # Use league average
    target_share <- ifelse(is.na(target_share), 0.12, target_share)
    target_factor <- min(1.2, max(0.8, target_share / 0.12))
    
    # Conservative team factor (max 10% adjustment)
    team_pass_att <- team_stats$pass_attempts_per_game[1] %||% 35
    team_factor <- min(1.1, max(0.9, team_pass_att / 35))
    
    home_factor <- ifelse(home_away == "home", 1.02, 0.98)
    
    prediction <- base_avg * target_factor * team_factor * home_factor
    
    # Apply realistic bounds for TE receptions (0-12 receptions)
    prediction <- max(0, min(12, prediction))
    
    return(round(prediction, 1))
  }
  
  predict_te_receiving_yards <- function(player_stats, team_stats, opponent_stats, home_away, game_context = NULL) {
    recent_games <- player_stats %>% arrange(desc(week)) %>% head(4)
    base_avg <- mean(recent_games$receiving_yards, na.rm = TRUE)
    
    # If no recent data, use career average
    if (is.na(base_avg) || base_avg == 0) {
      base_avg <- mean(player_stats$receiving_yards, na.rm = TRUE)
    }
    
    # Conservative yards per target factor (max 15% adjustment)
    ypt <- mean(recent_games$receiving_yards / recent_games$targets, na.rm = TRUE)
    ypt <- ifelse(is.na(ypt), 7.5, ypt)
    ypt_factor <- min(1.15, max(0.85, ypt / 7.5))
    
    # Conservative team factor (max 10% adjustment)
    team_ypt <- team_stats$yards_per_attempt[1] %||% 7.5
    team_factor <- min(1.1, max(0.9, team_ypt / 7.5))
    
    home_factor <- ifelse(home_away == "home", 1.02, 0.98)
    
    prediction <- base_avg * ypt_factor * team_factor * home_factor
    
    # Apply realistic bounds for TE receiving yards (0-150 yards)
    prediction <- max(0, min(150, prediction))
    
    return(round(prediction, 1))
  }

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
    
    # Load current rosters
    current_rosters <- nflreadr::load_rosters(seasons = 2025)
    
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
    recent_stats <- nflreadr::load_player_stats(seasons = 2025) %>%
      filter(week <= week_num, week >= max(1, week_num - 3)) %>%
      select(player_name, position, passing_yards, rushing_yards, receiving_yards, receptions, 
             passing_tds, rushing_tds, receiving_tds)
    
    # Generate simple prop predictions based on recent averages
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
      rowwise() %>%
            mutate(
        passing_yards_pred = if (position == "QB" && !is.na(avg_passing)) round(avg_passing) else NA,
        rushing_yards_pred = if (position %in% c("QB", "RB") && !is.na(avg_rushing)) round(avg_rushing) else NA,
        receiving_yards_pred = if (position %in% c("WR", "TE", "RB") && !is.na(avg_receiving)) round(avg_receiving) else NA,
        receptions_pred = if (position %in% c("WR", "TE", "RB") && !is.na(avg_receptions)) round(avg_receptions) else NA,
        passing_tds_pred = if (position == "QB" && !is.na(avg_passing_tds)) round(avg_passing_tds * 10) / 10 else NA,
        rushing_tds_pred = if (position %in% c("QB", "RB") && !is.na(avg_rushing_tds)) round(avg_rushing_tds * 10) / 10 else NA,
        receiving_tds_pred = if (position %in% c("WR", "TE", "RB") && !is.na(avg_receiving_tds)) round(avg_receiving_tds * 10) / 10 else NA
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
        prop_type = case_when(
          prop_type == "passing_yards_pred" ~ "Passing Yards",
          prop_type == "rushing_yards_pred" ~ "Rushing Yards", 
          prop_type == "receiving_yards_pred" ~ "Receiving Yards",
          prop_type == "receptions_pred" ~ "Receptions",
          prop_type == "passing_tds_pred" ~ "Passing TDs",
          prop_type == "rushing_tds_pred" ~ "Rushing TDs",
          prop_type == "receiving_tds_pred" ~ "Receiving TDs"
        ),
        prop_line = prediction,
        confidence = case_when(
          games_played >= 3 ~ "High",
          games_played >= 2 ~ "Medium", 
          TRUE ~ "Low"
        )
      ) %>%
      select(player_name, team, position, prop_type, prop_line, confidence) %>%
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
    DT::datatable(df, options = list(scrollX = TRUE), rownames = FALSE)
    }, error = function(e) {
      # Return empty table with error message
      DT::datatable(data.frame(Error = paste("Unable to load player props data:", e$message)), rownames = FALSE)
    })
  })

  # --- Player Trends Analysis ---
  
  # Load historical data for trends analysis
  load_trends_data <- function(seasons = 2020:2025) {
    tryCatch({
      # Load historical data for trend analysis (2020-2025)
      player_stats <- nflreadr::load_player_stats(seasons = seasons)
      schedules <- nflreadr::load_schedules(seasons = seasons)
      
      # Load 2025 rosters for current season
      rosters <- nflreadr::load_rosters(seasons = 2025)
      
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
    
    # Get 2025 schedule data
    current_schedule <- trends_data$schedules %>%
      filter(season == 2025, week == !!week) %>%
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
    
    # Get real 2025 schedule for the specified week
    week_games <- trends_data$schedules %>%
      filter(season == 2025, week == !!week) %>%
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
    teams <- nflreadr::load_teams()
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
    rosters <- nflreadr::load_rosters()
    lapply(rosters$gsis_id, function(pid) {
      observeEvent(input[[paste0("player_", pid)]], {
        selected_player(pid)
        current_page("player")
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui = ui, server = server)
