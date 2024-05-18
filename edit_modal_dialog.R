edit_modal_dialog <- function() {
 shiny::modalDialog(
  title = "Edit Student Data",
  div(
   style = "display: flex; flex-wrap: nowrap;
     align-items: center; gap: 5px;",
    textInput("edit_name", 
              "Name", 
              width = "500px"),
    selectInput("edit_gender",
                "Gender", 
                choices = c("Male", "Female"),
                width = "150px",
                selected = NULL),
    numericInput("edit_ID", "ID",
                 value = "", 
                 width = "150px"),
    selectInput("edit_code",
                "Code",
                choices = c("X74", "X75"),
                width = "100px",
                selected = NULL),
    disabled(
     textInput("edit_reg",
               "Registration Number",
               value = "",
               width = "300px")
    ),
    fileInput("edit_photoInput",
              "Select Photo", 
              width = "700px",
              accept = c("image/jpeg", "image/png")
    ),
    hidden(textInput("edit_course", label = "Course"),
           textInput("edit_buttons", label = "")
           )
   ),
  size = 'xl',
  easyClose = FALSE,
  footer = div(
   shiny::actionButton(inputId = "save",
                       label   = "SAVE"
                       ),
   shiny::actionButton(inputId = "cancel",
                       label   = "CANCEL",
                       )
  )
 ) |>
  shiny::showModal()
}
