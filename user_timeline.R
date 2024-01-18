user_timeline_modal <- function(reg_no){
 shiny::modalDialog(
  title = strong("Actions Timeline"),
  tags$div(actionButton(inputId = "ex",
                        label = "",
                        icon = icon("circle-xmark"),
                        )
           ),
  size = "l",
  easyClose = FALSE,
  footer = NULL,
  box(
   style = "height: 425px; overflow-y: auto;",
   title = reg_no,
   solidHeader = TRUE,
   width = 12,
   uiOutput("timeline_block")
  ) 
 )|>
  shiny::showModal()
}
