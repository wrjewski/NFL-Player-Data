# Test complete player props functionality
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

# Simulate player stats with realistic data
stats25 <- data.frame(
  player_id = rep(paste0("player_", 1:20), 5),
  player_name = rep(c("QB Smith", "RB Johnson", "WR Brown", "TE Davis", "K Wilson",
                      "QB Jones", "RB Williams", "WR Miller", "TE Garcia", "K Anderson",
                      "QB Taylor", "RB Moore", "WR White", "TE Lee", "K Clark",
                      "QB Wilson", "RB Davis", "WR Jones", "TE Smith", "K Brown"), 5),
  position = rep(c("QB", "RB", "WR", "TE", "K",
                   "QB", "RB", "WR", "TE", "K",
                   "QB", "RB", "WR", "TE", "K",
                   "QB", "RB", "WR", "TE", "K"), 5),
  week = rep(1:5, each = 20),
  team = rep(c("A","B","C","D","E","F","G","H"), 25),
  opponent_team = rep(c("E","F","G","H","A","B","C","D"), 25),
  passing_yards = sample(150:350, 100, replace = TRUE),
  rushing_yards = sample(0:120, 100, replace = TRUE),
  receiving_yards = sample(0:150, 100, replace = TRUE),
  receptions = sample(0:8, 100, replace = TRUE),
  fg_made = sample(0:4, 100, replace = TRUE)
)

ui <- fluidPage(
  titlePanel("Complete Player Props Test"),
  selectInput("test_week", "Select Week:", choices = 1:5, selected = 1),
  selectInput("test_matchup", "Select Matchup:", choices = NULL),
  h4("Player Analysis"),
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
      # Create analysis from player stats (simulating the app logic)
      analysis <- players %>%
        mutate(
          market = "player_stats",
          predicted = case_when(
            position == "QB" ~ passing_yards * 1.05,
            position %in% c("RB", "WR", "TE") ~ (rushing_yards + receiving_yards) * 1.1,
            position == "K" ~ fg_made * 1.2,
            TRUE ~ NA_real_
          ),
          pick = case_when(
            position == "QB" & passing_yards > 200 ~ "Strong Pass Game",
            position %in% c("RB", "WR", "TE") & (rushing_yards + receiving_yards) > 100 ~ "High Usage",
            position == "K" & fg_made > 2 ~ "Reliable Kicker",
            TRUE ~ "Monitor"
          ),
          confidence = case_when(
            position == "QB" ~ abs(passing_yards - 200),
            position %in% c("RB", "WR", "TE") ~ abs((rushing_yards + receiving_yards) - 100),
            position == "K" ~ abs(fg_made - 2),
            TRUE ~ 0
          )
        ) %>%
        filter(!is.na(predicted)) %>%
        select(player_name, position, team, pick, confidence, passing_yards, rushing_yards, receiving_yards)
    }
  })
}

shinyApp(ui = ui, server = server)
