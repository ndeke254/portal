library(shiny)
library(shinyjs)

ui <- fluidPage(
 useShinyjs(),
 tags$head(
  tags$style(HTML("
      .moving-sentence {
        color: red;
        font-weight: 700;
        white-space: nowrap;
        overflow: hidden;
        position: absolute;
      }
    "))
 ),
 mainPanel(
  div(class = "moving-sentence", "This is a moving sentence.")
 )
)

server <- function(input, output, session) {
 shinyjs::runjs('
    var container = $(".moving-sentence");
    container.css("left", "100%");

    function moveSentence() {
      container.animate({left: "-100%"}, 30000, "linear", function() {
        container.css("left", "100%");
        moveSentence();
      });
    }

    moveSentence();
  ')
}

shinyApp(ui, server)
