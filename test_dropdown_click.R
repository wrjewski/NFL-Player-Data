# Test dropdown clicking behavior
library(shiny)

ui <- fluidPage(
  titlePanel("Dropdown Click Test"),
  selectInput("test_week", "Select Week:", choices = list("Loading..." = "loading")),
  textOutput("selected_week"),
  actionButton("test_btn", "Test Button")
)

server <- function(input, output, session) {
  weeks <- 1:18
  
  # Initialize dropdown once
  observe({
    updateSelectInput(session, "test_week", choices = weeks, selected = max(weeks))
  })
  
  output$selected_week <- renderText({
    paste("Selected week:", input$test_week)
  })
  
  observeEvent(input$test_btn, {
    showNotification("Button clicked! Dropdown should still work.", type = "message")
  })
}

shinyApp(ui = ui, server = server)
