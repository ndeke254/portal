library(shiny)
library(stringr)

button <- '<button id="approve_ 2" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-check-to-slot" role="presentation" aria-label="check-to-slot icon"></i></button><button id="delete_ 2" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button><button id="edit_ 2" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>'

ui <- fluidPage(
 titlePanel("Dynamic Button Generation"),
 sidebarLayout(
  sidebarPanel(
   actionButton("addButton", "Add Button")
  ),
  mainPanel(
   tableOutput("buttonTable")
  )
 )
)

server <- function(input, output) {
 buttons <- reactiveValues(data = data.frame(id = numeric(0), button = character(0), stringsAsFactors = FALSE))
 
 observeEvent(input$addButton, {
  num_buttons <- nrow(buttons$data)
  new_id <- num_buttons + 1
  new_button <- str_replace(button, "2", as.character(new_id))
  buttons$data <- rbind(buttons$data, data.frame(id = new_id, button = as.character(new_button), stringsAsFactors = FALSE))
 })
 
 output$buttonTable <- renderTable({
  buttons$data
 }, sanitize.text.function = function(x) x)
}

shinyApp(ui, server)
