# Test the dropdown fix
library(shiny)
library(dplyr)

# Simulate the same data structure as the main app
sched_all <- data.frame(
  season = 2025,
  week = rep(1:5, each = 4),
  game_id = paste0("2025_", sprintf("%02d", rep(1:5, each = 4)), "_", 
                   rep(c("A","B","C","D"), 5), "_", 
                   rep(c("E","F","G","H"), 5)),
  away_team = rep(c("A","B","C","D"), 5),
  home_team = rep(c("E","F","G","H"), 5),
  game_type = "REG",
  home_score = NA
)

ui <- fluidPage(
  titlePanel("Dropdown Fix Test"),
  tabsetPanel(
    tabPanel("Game Predictions",
      br(),
      selectInput("pred_week", "Select Week:", choices = list("Loading..." = "loading")),
      textOutput("pred_week_display")
    ),
    tabPanel("Player Props",
      br(),
      selectInput("prop_week", "Select Week:", choices = list("Loading..." = "loading")),
      selectInput("prop_matchup", "Select Matchup:", choices = list("Select a week first" = "none")),
      textOutput("prop_week_display"),
      textOutput("prop_matchup_display")
    )
  )
)

server <- function(input, output, session) {
  # Same logic as the main app
  observe({
    weeks <- sort(unique(sched_all$week[!is.na(sched_all$week)]))
    if (length(weeks) > 0) {
      updateSelectInput(session, "pred_week", choices = weeks, selected = max(weeks))
      updateSelectInput(session, "prop_week", choices = weeks, selected = max(weeks))
    }
  })
  
  # Matchup choices
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
  
  # Display selected values
  output$pred_week_display <- renderText({
    paste("Selected prediction week:", input$pred_week)
  })
  
  output$prop_week_display <- renderText({
    paste("Selected prop week:", input$prop_week)
  })
  
  output$prop_matchup_display <- renderText({
    paste("Selected matchup:", input$prop_matchup)
  })
}

# Run the test app
shinyApp(ui = ui, server = server)
