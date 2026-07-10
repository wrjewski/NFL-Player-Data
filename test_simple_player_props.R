# Simple test for player props
library(shiny)
library(dplyr)
library(nflreadr)

ui <- fluidPage(
  titlePanel("Simple Player Props Test"),
  selectInput("test_week", "Select Week:", choices = 1:5, selected = 1),
  selectInput("test_matchup", "Select Matchup:", choices = NULL),
  h4("Player Analysis"),
  tableOutput("players_table")
)

server <- function(input, output, session) {
  # Load data
  sched_all <- nflreadr::load_schedules(seasons = 2025) %>%
    distinct(game_id, .keep_all = TRUE)
  
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
    
    # Load player stats
    stats25 <- nflreadr::load_player_stats(seasons = 2025)
    
    # Get the game info
    selected_game <- sched_all %>% filter(game_id == input$test_matchup)
    
    if (nrow(selected_game) == 0) {
      return(data.frame(Message = "No game found"))
    }
    
    # Filter players
    players <- stats25 %>% 
      filter(week == selected_game$week[1] & 
             (team == selected_game$home_team[1] | team == selected_game$away_team[1]))
    
    if (nrow(players) == 0) {
      return(data.frame(Message = "No players found"))
    }
    
    # Create analysis
    analysis <- players %>%
      mutate(
        pick = case_when(
          position == "QB" & passing_yards > 200 ~ "Strong Pass Game",
          position %in% c("RB", "WR", "TE") & (rushing_yards + receiving_yards) > 100 ~ "High Usage",
          position == "K" & fg_made > 2 ~ "Reliable Kicker",
          TRUE ~ "Monitor"
        )
      ) %>%
      filter(!is.na(pick)) %>%
      select(player_name, position, team, pick, passing_yards, rushing_yards, receiving_yards)
    
    if (nrow(analysis) == 0) {
      data.frame(Message = "No analysis available")
    } else {
      analysis
    }
  })
}

shinyApp(ui = ui, server = server)
