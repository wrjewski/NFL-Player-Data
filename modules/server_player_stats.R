# This code renders a data table of player stats based on the selected player.
# It dynamically adjusts the stats shown depending on the player's position.

output$player_stats_table <- DT::renderDataTable({
  # Make sure a player is selected before running the rest of the code
  req(selected_player())

  # Load player stats for all seasons from 1999 through the current season.
  # get_current_season() follows nflverse's own Labor Day cutover rather
  # than the calendar year, so this doesn't request a season that doesn't
  # have any data yet during the off-season. Served from data_cache/ (see
  # R/data_pipeline.R) instead of hitting nflverse on every session.
  stats <- get_player_stats_cached(
    seasons = 1999:nflreadr::get_current_season()
  )

  # Filter the stats to include only rows for the selected player
  player_data <- stats[stats$player_id == selected_player(), ]

  # If the player has no stats, return nothing
  if (nrow(player_data) == 0) return(NULL)

  # Sort the stats by season and week so the log is chronological
  player_data <- player_data[order(player_data$season, player_data$week), ]

  # These are common columns to always show for any player
  base_cols <- c("season", "week", "team", "opponent_team", "position")

  # Get the player's position — we use the first non-missing value found
  pos <- na.omit(player_data$position)[1]

  # Based on the player's position, determine which stat columns to include
  # Each position has a custom list of stat fields relevant to that role
  stat_cols <- switch(pos,
    "QB" = c("completions", "attempts", "passing_yards", "passing_tds",
             "passing_interceptions", "sacks_suffered", "sack_yards_lost",
             "sack_fumbles", "sack_fumbles_lost", "passing_air_yards",
             "passing_yards_after_catch", "passing_first_downs",
             "passing_epa", "passing_cpoe", "passing_2pt_conversions"),

    "RB" = c("carries", "rushing_yards", "rushing_tds", "rushing_epa",
             "receptions", "receiving_yards", "receiving_tds"),

    "WR" = c("receptions", "targets", "receiving_yards", "receiving_tds",
             "receiving_air_yards", "receiving_yards_after_catch",
             "receiving_first_downs", "receiving_epa"),

    "TE" = c("receptions", "targets", "receiving_yards", "receiving_tds",
             "receiving_air_yards", "receiving_yards_after_catch",
             "receiving_first_downs", "receiving_epa"),

    "FB" = c("carries", "rushing_yards", "rushing_tds", "rushing_epa",
             "receptions", "receiving_yards", "receiving_tds"),

    # Offensive Line and Defensive Line share defensive stats
    "OL" = , "C" = , "G" = , "T" = , "DL" = , "DE" = , "DT" = c(
      "def_tackles_solo", "def_tackle_assists", "def_tackles_with_assist", # nolint
      "def_sacks", "def_tackles_for_loss", "def_qb_hits"),

    # Linebackers and similar positions with pass defense and tackle stats
    "LB" = , "OLB" = , "ILB" = , "CB" = , "S" = , "FS" = , "SS" = c(
      "def_interceptions", "def_tackles_solo", "def_tackle_assists", # nolint
      "def_tackles_with_assist", "def_sacks", "def_tackles_for_loss",
      "def_qb_hits", "def_pass_defended", "def_tds"),

    # Kickers and Punters
    "K" = c("fg_made", "fg_att", "fg_pct", "pat_made", "pat_att"),
    "P" = c("punt_returns", "punt_return_yards"),

    # Default fallback: if position is unknown, use no extra stat columns
    character(0)
  )

  # Combine the base columns and position-specific stats
  cols_to_use <- c(base_cols, stat_cols)

  # Keep only the columns that exist in the data to avoid errors
  cols_to_use <- intersect(cols_to_use, names(player_data))

  # Subset the player data to the selected columns
  player_data <- player_data[, cols_to_use, drop = FALSE]

  # Clean up column names: replace underscores with spaces and capitalize
  colnames(player_data) <- gsub(
    "_", " ", tools::toTitleCase(colnames(player_data))
  )

  # Create the DataTable UI component
  DT::datatable(
    player_data,
    options = list(
      pageLength = 25,  # Show 25 rows per page
      scrollX = TRUE    # Enable horizontal scroll if table is wide
    ),
    rownames = FALSE   # Do not show row numbers
  )
})
