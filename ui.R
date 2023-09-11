admin <- read_csv("data/admin.csv",show_col_types = FALSE) # nolint

ui <- navbarPage(
 position = c("fixed-top"), # nolint
 windowTitle = tags$head(
  tags$link(rel = "icon",
            type = "image/png",
            href = "logo.png"),
  tags$title("University of Kitui")), # nolint
 title = tags$img(src = "logo.png"), # nolint
 theme = bslib::bs_theme(4),
 tabPanel(
  title = "Lecturer",
  value = "lecturer",
  icon = icon("person-chalkboard"),
  useShinyjs(),
  useShinyFeedback(),
  useShinydashboard(),
  includeCSS("www/styles.css"),
  bsTooltip("reset","Reset","right","hover"),
  tabsetPanel(
   tabPanel(
    title = "RESULTS",
  splitLayout(
   cellWidths = c("10.286%","14.286%","5.286%", # nolint
                  "24.286%","5.286%","4.286%","10.286%"), # nolint
   selectizeInput(
    inputId = "reg",
    label = "Registration number", # nolint
    multiple = FALSE,
    choices = NULL
   ),
   disabled(
    textInput("name", "Student name", placeholder = "Selected student")
   ),
   selectizeInput(
    inputId = "code",
    label = "Code", # nolint
    multiple = FALSE,
    choices = NULL
   ),
   disabled(
    textInput("course", "Course", placeholder = "Selected course")
   ),
   numericInput("score", "Score", value = NULL, min = 1, max = 99),
   disabled(
    textInput("grade", "Grade"),
    hidden(textInput("id","ID")),
    hidden(textInput("time", "Time")),
    hidden(textInput("lecturer", "Lecturer")),
    hidden(textInput("actions", "Actions"))
   ),
   conditionalPanel(
    condition = "input.confirm_edit", 
    tags$div(id = "exit_button",
              actionButton(inputId = "exit",
                           label = "Exit",
                           title = "Exit mode")
    )
   ),
   div(
    style = "padding-top: 25px;",
    loadingButton("submit", "Submit",
                  style = "width: 100px",
                  loadingLabel = "Entering",
                  loadingSpinner = "cog")
    )
   ),
  tags$hr(),
  DT::dataTableOutput("marks")
  ),
  tabPanel(
   title = "LESSONS"
   )
  )
 ),
 tabPanel(
  title = "Administrator", # nolint
  value = "admin",
  icon = icon("table-cells"),  
  tabsetPanel(
   tabPanel(
    title = "STATISTICS",
    fluidRow(
     valueBoxOutput("registered_no",
                   width = 2),
     valueBoxOutput("available_marks",
                   width = 2),
     valueBoxOutput("missing_marks",
                   width = 2),
     valueBoxOutput("failed_marks",
                    width = 2),
     valueBoxOutput("waiting_promotion",
                   width = 2),
     valueBoxOutput("grad_students",
                   width = 2)
     ),
    h1("SET REGISTRATION DATE"),
    fluidRow(
    column(3,  align = "center",
    dateInput("target_date", "Select a End Date:", 
              format = "dd-mm-yyyy"
              ),
    timeInput("select_time","Enter End time"),
    actionButton("open","Open",icon = icon("unlock")),
    actionButton("close","Close",icon = icon("lock"))
    ),
    column(3, align = "center",
           h1("Your proposed Close Date and Time is:"),
           textOutput("flip_time"),
           DT::dataTableOutput("set_time")
           )
    )
    ),
   tabPanel(
    title = "REGISTRATION",
  splitLayout(
   cellWidths = c("18.286%","8.286%","9.286%", # nolint
                  "5.286%","12.286%","24.286%","0%","2.286%"
                  ),
   textInput("Name", 
             "Name", 
             width = "225px"),
   selectInput("Gender", 
               "Gender",
               choices = c("Male", "Female"),
               width = "100px",
               selected = NULL),
   numericInput("ID", "ID",
                value = "", 
                width = "110px"),
   selectInput("Code",
               "Code",
               choices = c("X74", "X75"),
               width = "55px",
               selected = NULL),
   disabled(
    hidden(
     textInput("Date", 
               "Date",
               value = format(Sys.time(), "%d-%m-%Y %H:%M"),
               width = "120px")
     ),
    textInput("Reg",
              "Registration Number",
              value = "",
              width = "150px")
    ),
   fileInput("photoInput",
             "Select Photo", 
             accept = c("image/jpeg", "image/png")
             ),
   hidden(textInput("Course", label = "Course"),
          textInput("Buttons", label = ""),
          textInput("toggle", label = "", value = "0"),
          textInput("editmode", label = "", value = "0"),
          textInput("Year", label = "Year", value = "1")
          ),
   conditionalPanel(
   condition = "input.confirm_editreg", 
   tags$div(id = "reset_button",
   actionButton(inputId = "reset",
                label = "Exit",
                title = "Exit mode")
   )
   ),
   loadingButton("registerButton", "Register",
               style = "width: 110px;margin-top: -34px;",
               loadingLabel = "Registering",
               loadingSpinner = "cog")
   ),
  tags$hr(),
  DT::dataTableOutput("registrationTable"),
 ),
 tabPanel(   
  title = "RESULTS",
  h1("APPROVED RESULTS"),
  DT::dataTableOutput("approved_marks")
  )
 )
 ),
 tabPanel(
  title = "Student",
  value = "student",
  icon = icon("children"),
  textOutput("student_name"),
  textOutput("student_course"),
  tabsetPanel(
   tabPanel(
   title = "TIMETABLE",
  splitLayout(
   cellWidths = c("10.286%","5.286%","24.286%","5.286%"),
 selectizeInput(
  inputId = "student_reg",
  label = "Registration number", # nolint
  multiple = FALSE,
  choices = NULL
  )
 ),
 tags$hr(),
 h1("Student Timetable"),
 DT::dataTableOutput("timetable")
 ),
 tabPanel(
  title = "REGISTRATION",
  fluidRow(
  column(6, align = "center",
         disabled(
  selectizeInput(
   inputId = "register_code",
   label = "Code", # nolint
   multiple = FALSE,
   choices = NULL
  ),
   textInput(
    inputId = "register_unit",
    label = "Register Unit", # nolint
    value =  NULL
   )
  ),
  div(
   style = "padding: 15px;",
   actionBttn("register",label = "Register")
   )
  ),
  column(6, align = "center",
         tags$div(
          style = "color: red;",
         textOutput("notice")
         ),
         conditionalPanel(
          condition = "input.open",
          tags$div(id = "flip",
         #Flipdown time for submission
         flipdownr::flipdown(
          downto = admin[[1,1]],
          id = "flipdown",
          theme = "youkous"
          )
         )
         )
         )
  ),
  h1("Student Units"),
  DT::dataTableOutput("registered_units"),
 ),
 tabPanel(
  title = "RESULTS",
  h1("Released Marks"),
  DT::dataTableOutput("student_marks")
  ),
 tabPanel(
  title = "ANALYSIS",
  fluidRow(
   column(4,
          h1("Final Averages"),
          DT::dataTableOutput("year_averages"),
          conditionalPanel(
           condition = "input.student_reg",
           disabled(
          loadingButton("transcripts", style = "width: 100px",
                        label = "Transcripts",
                        loadingLabel = "Printing"
                         )
           )
          ),
          shinyjs::hidden(
           downloadButton("download",
                          label = "",
                          icon = icon("download")
                        )
           )
          ),
   column(4,
          h1("Final graph"),
          echarts4rOutput("graph")
          ),
   column(4,
          h1("Final Score"),
          echarts4rOutput("clock")
   )
   )
  )
 )
 )
 )
