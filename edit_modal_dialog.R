edit_modal_dialog <- function() {
 shiny::modalDialog(
  title = "Edit Student Data",
  div(
   splitLayout(
    cellWidths = c("18.286%","8.286%","9.286%", # nolint
                   "5.286%","12.286%","24.286%","0%","2.286%"
    ),
    textInput("edit_name", 
              "Name", 
              width = "225px"),
    selectInput("edit_gender",
                "Gender", 
                choices = c("Male", "Female"),
                width = "100px",
                selected = NULL),
    numericInput("edit_ID", "ID",
                 value = "", 
                 width = "110px"),
    selectInput("edit_code",
                "Code",
                choices = c("X74", "X75"),
                width = "55px",
                selected = NULL),
    disabled(
     textInput("edit_reg",
               "Registration Number",
               value = "",
               width = "150px")
    ),
    fileInput("edit_photoInput",
              "Select Photo", 
              accept = c("image/jpeg", "image/png")
    ),
    column(4,
           imageOutput("edit_previewImage", width = "100%", height = "65px")
    ),
    hidden(textInput("edit_course", label = "Course"),
           textInput("edit_buttons", label = "")
           )
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
