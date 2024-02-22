user_timeline_modal <- function(reg_no){
 shiny::modalDialog(
  includeScript("www/app.js"),
  includeCSS("www/styles.css"),
  useWaiter(),
  title = strong("Actions Timeline"),
  fixedPanel(
   right = 350,
   top = 10,
   tags$div(
    actionButton(
     inputId = "ex",
     label = "",
     icon = icon("circle-xmark"),
    )
   )
  ),
  size = "m",
  easyClose = FALSE,
  footer = NULL,
  box(
   title = reg_no,
   solidHeader = TRUE,
   width = 12,
   div(
    style = "
    display: flex;
    flex-direction: column;
    flex-wrap: nowrap;
    align-content: center;
    justify-content: center;",
    div(
    id = "end",
    style = "height: 242px; width: 384px;"
   ),
   shinyjs::hidden(
   uiOutput("empty")
   )
   )
   ) 
  )|>
  shiny::showModal()
}
