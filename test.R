library(shiny)

ui <- navbarPage(
 title = "Hover Text Example",
 tabPanel(
  "Home",
  actionButton("myButton", "Click Me", title = "Hover Text: Perform an Action")
 )
)

server <- function(input, output) {
 # Add your server logic here
}

shinyApp(ui, server)
