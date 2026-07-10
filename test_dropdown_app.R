library(shiny)
library(dplyr)

# Dummy schedule data
sched_all <- data.frame(
  season = 2025,
  week = rep(1:5, each = 2),
  game_id = 1001:1010,
  away_team = rep(c("A","B","C","D","E"), each = 2),
  home_team = rep(c("F","G","H","I","J"), each = 2),
  game_type = "REG",
  home_score = NA
)

ui <- fluidPage(
  title = "Betting Tab Test",
  tabsetPanel(
    tabPanel("Game Predictions",
      br(),
      selectInput("pred_week", "Select Week:", choices = NULL),
      tableOutput("tbl_games")
    ),
    tabPanel("Player Props",
      br(),
      selectInput("prop_week", "Select Week:", choices = NULL),
      selectInput("prop_matchup", "Select Matchup:", choices = NULL),
      tableOutput("tbl_props")
    )
  )
)

server <- function(input, output, session) {

  # Populate weeks in both dropdowns
  observe({
    weeks <- sort(unique(sched_all$week))
    updateSelectInput(session, "pred_week", choices = weeks, selected = weeks[1])
    updateSelectInput(session, "prop_week", choices = weeks, selected = weeks[1])
  })

  output$tbl_games <- renderTable({
    req(input$pred_week)
    sched_all %>% filter(week == input$pred_week)
  })

  # Matchups reactive
  matchups_for_week <- reactive({
    req(input$prop_week)
    sched_all %>%
      filter(week == input$prop_week) %>%
      mutate(label = paste(away_team, "vs", home_team)) %>%
      select(game_id, label)
  })

  observe({
    mm <- matchups_for_week()
    if (nrow(mm) > 0) {
      updateSelectInput(session, "prop_matchup",
        choices = setNames(mm$game_id, mm$label),
        selected = mm$game_id[1]
      )
    }
  })

  output$tbl_props <- renderTable({
    req(input$prop_week, input$prop_matchup)
    # Just show the matchup row for demo
    sched_all %>% filter(game_id == input$prop_matchup)
  })
}

shinyApp(ui, server)
