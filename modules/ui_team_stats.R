# modules/ui_team_stats.R

ui_team_stats <- function() {
  tagList(
    actionButton("back_home", "<- Back"),
    h3("Team Stats Explorer"),

    # Team selector
    selectInput(
      inputId = "team_stats_team",
      label = "Select Team:",
      choices = sort(nflreadr::load_teams()$team_abbr),
      selected = NULL
    ),

    # Season range slider
    sliderInput(
      inputId = "season_range",
      label = "Select Season Range:",
      min = 1999,
      max = lubridate::year(Sys.Date()),
      value = c(2000, lubridate::year(Sys.Date())),
      sep = ""
    ),

    # Summary / view type
    radioButtons(
      inputId = "summary_type",
      label = "Summary Level:",
      choices = c("Season Summary" = "season", "Game Log" = "week"),
      inline = TRUE
    ),

    # Show stats button
    actionButton(
      inputId = "go_show_team_stats",
      label = "Show Stats",
      style = "margin-top: 10px; width: 200px;"
    ),

    br(), br(),

    DT::dataTableOutput("team_stats_table")
  )
}
