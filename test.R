library(shiny)
library(shinydashboard)

ui <- dashboardPage(
 dashboardHeader(),
 dashboardSidebar(),
 dashboardBody(
  box(
   title = "My ValueBox Title",
   width = 6,
   status = "purple",
   solidHeader = TRUE,
   valueBoxOutput("myValueBox")
  )
 )
)

server <- function(input, output) {
 output$myValueBox <- renderValueBox({
  valueBox(
   value = 42,
   subtitle = "Subtitle Text",
   icon = icon("dashboard")
  )
 })
}

shinyApp(ui, server)
