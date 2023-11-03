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
    hidden(textInput("actions", "Actions")),
    hidden(textInput("student_year", label = "Year", value = 0))
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
              format = "dd-mm-yyyy",
              min = Sys.Date()
              ),
    timeInput("select_time","Enter End time"),
    actionButton("open","Open",icon = icon("unlock")),
    actionButton("close","Close",icon = icon("lock"))
    ),
    column(3, align = "center",
           h1("Your proposed Close Date and Time is:"),
           textOutput("flip_time"),
           tags$div(
            id = "table",
           DT::dataTableOutput("set_time")
           )
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
  div(style = " margin-top: 45px;
                margin-left: 1100px;
                position: absolute;",
  progress_circle(value = 0, 
                shiny_id = "bar",
                color = "green",
                trail_width = 4,
                width = "100%",
                height = "100px",
                )
  ),
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
  column(6,
         splitLayout(
         disabled(
  selectizeInput(
   inputId = "register_code",
   label = "Code", # nolint
   multiple = FALSE,
   choices = NULL
  )),
  selectizeInput(
   inputId = "type" ,
   label = "Exam Type", # nolint
   multiple = FALSE,
   choices = c("FIRST ATTEMPT","RETAKE 1", "RETAKE 2","SUPPLEMENTARY")
  ),
  disabled(
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
  )),
  column(6, align = "center",
         tags$div(
          style = "color: red; font-weight: bold;",
         textOutput("notice")
         ),
          tags$div(id = "flip",
                    column(1,textOutput( "days")),
                    column(1,textOutput( "hours" )),
                    column(1, textOutput( "minutes")),
                    column(1,textOutput("seconds"))
          )
         )
  ),
  h1("Student Units"),
  DT::dataTableOutput("registered_units"),
 ),
 tabPanel(
  title = "RESULTS",
  fluidRow(
   column(6,
          h1("Year 1 Marks"),
          DT::dataTableOutput("year_1_marks")
          ),
  column(6,
         h1("Year 2 Marks"),
         DT::dataTableOutput("year_2_marks")
         )
  ),
  fluidRow(
  column(6,
         h1("Year 3 Marks"),
         DT::dataTableOutput("year_3_marks")
         ),
  column(6,
         h1("Year 4 Marks"),
         DT::dataTableOutput("year_4_marks")
         )
  )
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