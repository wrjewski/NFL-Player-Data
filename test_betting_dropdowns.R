# Test script to verify betting dropdown functionality
library(shiny)
library(dplyr)

# Create a minimal test version of the betting UI and server logic
ui <- fluidPage(
  titlePanel("Betting Dropdown Test"),
  tabsetPanel(
    tabPanel("Game Predictions",
      br(),
      selectInput("pred_week", "Select Week:", choices = NULL),
      textOutput("pred_week_output")
    ),
    tabPanel("Player Props",
      br(),
      selectInput("prop_week", "Select Week:", choices = NULL),
      selectInput("prop_matchup", "Select Matchup:", choices = NULL),
      textOutput("prop_week_output"),
      textOutput("prop_matchup_output")
    )
  )
)

server <- function(input, output, session) {
  # Simulate schedule data
  sched_all <- data.frame(
    season = 2025,
    week = rep(1:5, each = 2),
    game_id = 1001:1010,
    away_team = rep(c("A","B","C","D","E"), each = 2),
    home_team = rep(c("F","G","H","I","J"), each = 2),
    game_type = "REG",
    home_score = NA
  )
  
  # Test the fixed logic - both dropdowns should be populated
  observe({
    weeks <- sort(unique(sched_all$week[!is.na(sched_all$week)]))
    if (length(weeks) > 0) {
      updateSelectInput(session, "pred_week", choices = weeks, selected = max(weeks))
      updateSelectInput(session, "prop_week", choices = weeks, selected = max(weeks))
    }
  })
  
  # Matchup choices for Player Props
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
  
  # Outputs to show the selected values
  output$pred_week_output <- renderText({
    paste("Selected prediction week:", input$pred_week)
  })
  
  output$prop_week_output <- renderText({
    paste("Selected prop week:", input$prop_week)
  })
  
  output$prop_matchup_output <- renderText({
    paste("Selected matchup:", input$prop_matchup)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
