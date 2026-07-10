# Test UI initialization approach
library(shiny)

ui <- fluidPage(
  titlePanel("UI Initialization Test"),
  tabsetPanel(
    tabPanel("Test Tab",
      br(),
      selectInput("test_week", "Select Week:", choices = 1:18, selected = 18),
      textOutput("selected_week")
    )
  )
)

server <- function(input, output, session) {
  output$selected_week <- renderText({
    paste("Selected week:", input$test_week)
  })
}

shinyApp(ui = ui, server = server)
