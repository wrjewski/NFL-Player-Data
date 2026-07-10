# Test data loading functionality
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
  titlePanel("Data Loading Test"),
  selectInput("test_week", "Select Week:", choices = 1:5, selected = 5),
  tableOutput("games_table")
)

server <- function(input, output, session) {
  output$games_table <- renderTable({
    req(input$test_week)
    games <- sched_all %>%
      filter(is.na(home_score), game_type == "REG", week == input$test_week)
    
    if (nrow(games) == 0) {
      data.frame(Message = "No games found for the selected week")
    } else {
      games
    }
  })
}

shinyApp(ui = ui, server = server)
