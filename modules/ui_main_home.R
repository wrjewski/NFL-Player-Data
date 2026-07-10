# modules/ui_main_home.R

ui_main_home <- function() {
  tagList(
    h3("Welcome to the NFL Stats App"),
    br(),
    fluidRow(
      column(
        width = 4,
        actionButton(
          inputId = "go_team_stats",
          label = "Team Stats",
          style = "width: 100%; padding: 20px; font-size: 18px;"
        )
      ),
      column(
        width = 4,
        actionButton(
          inputId = "go_player_stats",
          label = "Player Stats",
          style = "width: 100%; padding: 20px; font-size: 18px;"
        )
      ),
      column(
        width = 4,
        actionButton(
          inputId = "go_betting",
          label = "Sports Betting",
          style = "width: 100%; padding: 20px; font-size: 18px;"
        )
      )
    )
  )
}
