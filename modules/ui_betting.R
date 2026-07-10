# modules/ui_betting.R

ui_betting <- function() {
  tagList(
    actionButton("back_home", "<- Back"),
    tabsetPanel(
      tabPanel("Game Predictions",
        br(),
        h4("Upcoming Games & Predictions"),
        selectInput("pred_week", "Select Week:", choices = 1:18, selected = 18),
        DT::dataTableOutput("upcoming_games_table")
      ),
      tabPanel("Player Props",
        br(),
        h4("Player Prop Predictions"),
        p("Predictions based on historical performance and team matchups"),
        selectInput("prop_week", "Select Week:", choices = 1:18, selected = 5),
        selectInput("prop_matchup", "Select Matchup:", choices = list("Select a week first" = "none")),
        DT::dataTableOutput("player_props_table")
      ),
      tabPanel("Trends",
        br(),
        h4("Player Prop Trends"),
        p("Analyze recent form and historical performance against specific opponents"),
        fluidRow(
          column(4, selectInput("trends_week", "Select Week:", choices = 1:18, selected = 5)),
          column(4, selectInput("trends_matchup", "Select Matchup:", 
                               choices = list("All Games" = "all"), 
                               selected = "all")),
          column(4, selectInput("trends_filter", "Filter by Form:", 
                               choices = list("All Trends" = "all", "Hot Only" = "hot", "Cold Only" = "cold"), 
                               selected = "hot"))
        ),
        br(),
        h5("Player Trends - Recent Form Analysis"),
        DT::dataTableOutput("trends_table"),
        br(),
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px;",
          h6("How to Read Trends:", style = "font-weight: bold; margin-bottom: 10px;"),
          tags$ul(
            tags$li("vs [Team]: Historical performance against that specific opponent"),
            tags$li("home games: Performance in home games only"),
            tags$li("away games: Performance in away games only"),
            tags$li("Hot: Recent form significantly above average"),
            tags$li("Average: Recent form around historical average"),
            tags$li("Cold: Recent form below historical average"),
            tags$li("Confidence: Based on number of games analyzed (High: 5+, Medium: 3-4, Low: 2)"),
            tags$li("Trend Strength: How reliable the trend is (Strong: 3+ games, Moderate: 2 games, Weak: 1 game)")
          )
        )
      )
    )
  )
}
