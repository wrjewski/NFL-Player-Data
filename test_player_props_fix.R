# Test player props fix
library(shiny)
library(dplyr)

# Simulate the data structures
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

# Simulate player stats
stats25 <- data.frame(
  player_id = rep(paste0("player_", 1:20), 5),
  player_name = rep(paste0("Player", 1:20), 5),
  week = rep(1:5, each = 20),
  team = rep(c("A","B","C","D","E","F","G","H"), 25),
  opponent_team = rep(c("E","F","G","H","A","B","C","D"), 25),
  passing_yards = sample(100:400, 100),
  rushing_yards = sample(0:150, 100),
  receptions = sample(0:10, 100)
)

ui <- fluidPage(
  titlePanel("Player Props Fix Test"),
  selectInput("test_week", "Select Week:", choices = 1:5, selected = 1),
  selectInput("test_matchup", "Select Matchup:", choices = NULL),
  tableOutput("players_table")
)

server <- function(input, output, session) {
  # Update matchup choices
  matchup_choices <- reactive({
    req(input$test_week)
    sched_all %>%
      filter(week == input$test_week) %>%
      mutate(label = paste(away_team, "vs", home_team)) %>%
      select(game_id, label)
  })
  
  observe({
    mc <- matchup_choices()
    if (nrow(mc) > 0) {
      updateSelectInput(session, "test_matchup",
                        choices = setNames(mc$game_id, mc$label),
                        selected = mc$game_id[1])
    }
  })
  
  output$players_table <- renderTable({
    req(input$test_week, input$test_matchup)
    
    # Get the game info from the schedule data
    selected_game <- sched_all %>% filter(game_id == input$test_matchup)
    if (nrow(selected_game) > 0) {
      # Filter player stats for the selected game using team and week
      players <- stats25 %>% 
        filter(week == selected_game$week[1] & 
               (team == selected_game$home_team[1] | team == selected_game$away_team[1]))
    } else {
      players <- stats25[0, ]
    }
    
    if (nrow(players) == 0) {
      data.frame(Message = "No players found for the selected matchup")
    } else {
      players[, c("player_name", "team", "passing_yards", "rushing_yards", "receptions")]
    }
  })
}

shinyApp(ui = ui, server = server)
