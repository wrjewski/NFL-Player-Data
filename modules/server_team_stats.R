# modules/server_team_stats.R

server_team_stats <- function(input, output, session) {

  # Reactive expression that waits for the button click
  team_stats_filtered <- eventReactive(input$go_show_team_stats, {
    req(input$team_stats_team, input$season_range, input$summary_type)

    stats_all <- nflreadr::load_team_stats(
      seasons = input$season_range[1]:input$season_range[2]
    )

    # Filter by team
    stats_sel <- stats_all[stats_all$team == input$team_stats_team, , drop = FALSE]

    # If summary_type = "week", show all rows; if "season", maybe reduce/group
    if (input$summary_type == "season") {
      # Example: take only the last week of each season
      # You could customize this if "season" summary means something else
      stats_sel <- stats_sel[stats_sel$week == max(stats_sel$week, na.rm = TRUE), ]
    }

    stats_sel
  })

  output$team_stats_table <- DT::renderDataTable({
    data <- team_stats_filtered()
    if (nrow(data) == 0) {
      return(NULL)
    }

    # Define which columns to show
    visible_cols <- c(
      "season", "week", "team", "opponent_team",
      "completions", "attempts", "passing_yards", "passing_tds",
      "passing_interceptions", "carries", "rushing_yards",
      "rushing_tds", "receptions", "receiving_yards",
      "receiving_tds", "def_tackles_solo", "def_sacks",
      "def_interceptions", "penalties", "penalty_yards"
    )

    visible_cols <- visible_cols[visible_cols %in% names(data)]
    data_to_show <- data[, visible_cols, drop = FALSE]

    # Clean up names for display
    colnames(data_to_show) <- gsub("_", " ", tools::toTitleCase(colnames(data_to_show)))

    DT::datatable(
      data_to_show,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })
}
