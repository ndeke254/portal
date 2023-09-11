library(shiny)

ui <- fluidPage(
  titlePanel("Extract Date and Time from Input"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_text", "Enter Date and Time (e.g., Wednesday 06-09-2023 00:00:00):")
    ),
    mainPanel(
      textOutput("output_datetime")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$input_text, {
    input_string <- input$input_text
    if (!is.null(input_string) && nchar(input_string) > 0) {
      tryCatch({
        parsed_date_time <- strptime(input_string, format = "%A %d-%m-%Y %H:%M:%S")
        formatted_date_time <- format(parsed_date_time, format = "%Y-%m-%d %H:%M:%S")
      }, error = function(e) {
        output$output_datetime <- renderText({
          paste("Error: Invalid date and time format.")
        })
      })
    }
  })
}

shinyApp(ui, server)
