library(shiny)
library(shinyjs)

ui <- fluidPage(
 useShinyjs(),  # Initialize shinyjs
 
 # Define custom CSS to make the textarea expandable
 tags$head(
  tags$style(
   HTML(".expandable-input {
             resize: vertical;
             min-height: 40px;
             }")
  )
 ),
 
 # Create the expandable input

server <- function(input, output, session) {
 # Add shinyjs code to resize the textarea dynamically
 runjs(
  "shinyjs.resizeTextarea = function() {
       var textarea = $('#expandable_input');
       textarea.height(0);
       textarea.height(textarea[0].scrollHeight);
     }"
 )
 
 # Observe changes in the input and trigger the resizing function
 observe({
  runjs("shinyjs.resizeTextarea();")
 })
}

shinyApp(ui, server)
