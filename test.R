library(shiny)
library(shinydashboard)

ui <- dashboardPage(
 dashboardHeader(
  title = "My Shiny App",
  tags$li(class = "dropdown",
          tags$a(href = "#",
                 tags$img(src = "logo.png",
                          height = "30px",
                          style = "margin-top: -5px;")
          )
  ),
  tags$style(HTML('.skin-blue .main-header .logo { padding-top: 0px; }'))
 ),
 dashboardSidebar(),
 dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)
