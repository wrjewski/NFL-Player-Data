# Simple test to verify dropdown functionality
library(shiny)

ui <- fluidPage(
  titlePanel("Simple Dropdown Test"),
  selectInput("test_week", "Select Week:", choices = list("Loading..." = "loading")),
  textOutput("selected_week")
)

server <- function(input, output, session) {
  # Simulate the same logic as the main app
  weeks <- 1:18
  
  observe({
    invalidateLater(1000, session)
    tryCatch({
      updateSelectInput(session, "test_week", choices = weeks, selected = max(weeks))
    }, error = function(e) {
      cat("Error updating dropdown:", e$message, "\n")
    })
  })
  
  output$selected_week <- renderText({
    paste("Selected week:", input$test_week)
  })
}

shinyApp(ui = ui, server = server)
