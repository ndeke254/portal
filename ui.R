ui <- navbarPage(
 position = c("fixed-top"), # nolint
 footer = tags$div(
  tags$hr(),
  tags$div(
   class = "p7",
    tags$div(
    class = "social",
    actionLink("linkedin",
               label = "",
               icon = icon("linkedin"),
               onclick = "window.open('https://www.linkedin.com/in/jefferson-ndeke-027062202/')")
   ),
   tags$div(
    class = "social",
    actionLink("github",
               label = "",
               icon = icon("github"),
               onclick = "window.open('https://github.com/ndeke254')")
   ),
   tags$div(
    class = "social",
    actionLink("twitter",
               label = "",
               icon = icon("twitter"),
               onclick = "window.open('https://twitter.com/jefferson_ndeke')")
    )
  ),
  class = "announcement_container",
  tags$div(
  class = "moving-sentence",
  style = "padding-top: 20px; padding-bottom: 20px;",
  textOutput("announcement")
  )
  ),
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
  tabsetPanel(
   tabPanel(
    title = "RESULTS",
    box(
     title = "Enter Student Details",
     solidHeader = TRUE,
     status = "primary",
     width = 12,
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
    hidden(textInput("id","ID", value = 0)),
    hidden(textInput("time", "Time")),
    hidden(textInput("lecturer", "Lecturer")),
    hidden(textInput("actions", "Actions"))
    ),
   div(
    style = "padding-top: 25px;",
    loadingButton("submit", "Submit",
                  style = "width: 100px",
                  loadingLabel = "Entering",
                  loadingSpinner = "cog")
    )
  )
  ),
  tags$hr(),
  box(
   title = "Released Exam Results",
   solidHeader = TRUE,
   status = "primary",
   width = 12,
  DT::dataTableOutput("marks")
  )
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
     box(
      title = "Real-time Track",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
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
     )
     ),
    box(
     title = "Course Registration",
     solidHeader = TRUE,
     status = "primary",
     width = 12,
     fluidRow(
     column(12, align = "center",
     br(),
     switchInput(inputId = "switch")
     )
    ),
    fluidRow(
    column(3,  align = "center",
           box(
            title = "Set Date/Time",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
    dateInput("target_date", "Select a End Date:", 
              format = "dd-mm-yyyy",
              min = Sys.Date()
              ),
    timeInput("select_time","Enter End time"),
    br(),
    tags$div(
    actionButton("open","Open",icon = icon("unlock")), 
    actionButton("close","Close",icon = icon("lock"))
    ))
    ),
    column(3, align = "center",
           box(
            title = "End Date/Time",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
           textOutput("flip_time"),
           tags$div(
            id = "table",
           DT::dataTableOutput("set_time")
           )
           )
           ),
    column(3, align = "center",
           box(
            title = "CountDown",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
           textOutput("remaining_time"),
           br(),
      shinyWidgets::progressBar(id = "progress_bar",
                                value = 50
                                )
      )
      ),
    column(3, align = "center",
           box(
            title = "Write Notice",
            solidHeader = TRUE,
            status = "primary",
            width = 12,
           textAreaInput(inputId = "post_writeup",
                         label = "",
                         value = "",
                         placeholder = "Enter your announcement here..."),
           br(),
           tags$div(
            actionButton("post","Post",icon = icon("bullhorn")),
            actionButton("unpost","Edit",icon = icon("pencil"))
           )
           )
           )
    )
    )
    ),
   tabPanel(
    title = "REGISTRATION",
    box(
     title = "Enter Student Details",
     solidHeader = TRUE,
     status = "primary",
     width = 12,
  splitLayout(
   cellWidths = c("18.286%","8.286%","9.286%", # nolint
                  "5.286%","12.286%","24.286%","0%","2.286%"
                  ),
   textInput("Name", 
             "Name", 
             width = "225px",
             placeholder = "Enter Student name"
             ),
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
               value = format(Sys.time(), "%d/%m/%Y %H:%M:%S"),
               width = "120px")
     ),
    textInput("Reg",
              "Registration Number",
              value = "",
              width = "150px",
              placeholder = "Enter Registration number"
    )
    ),
   fileInput("photoInput",
             "Select Photo", 
             accept = c("image/jpeg", "image/png")
             ),
   column(4,
          imageOutput("previewImage", width = "100%", height = "65px")
   ),
   hidden(textInput("Course", label = "Course"),
          textInput("Buttons", label = ""),
          textInput("Year", label = "Year", value = "1")
          ),
   loadingButton("registerButton", "Register",
               style = "width: 110px;margin-top: 30px;",
               loadingLabel = "Registering",
               loadingSpinner = "cog")
   )
  ),
  tags$hr(),
  box(
   title = "Registered Students Details",
   solidHeader = TRUE,
   status = "primary",
   width = 12,
  DT::dataTableOutput("registrationTable")
  )
   )
  )
  ),
 tabPanel(
  title = "Student",
  value = "student",
  icon = icon("children"),
  tabsetPanel(
   tabPanel(
   title = "TIMETABLE",
   box(
    title = "Student Details",
    solidHeader = TRUE,
    status = "primary",
    width = 12,
    splitLayout(
     selectizeInput(
      inputId = "student_reg",
      label = "Registration number", # nolint
      multiple = FALSE,
      choices = NULL
     ),
    textOutput("student_name"),
    textOutput("student_course"),
    progress_circle(value = 0, 
                        shiny_id = "bar",
                        color = "green",
                        trail_width = 4,
                        width = "100%",
                        height = "100px",
                    )
    )
    ),
   tags$hr(),
   box(
  title = "Student Timetable",
  solidHeader = TRUE,
  status = "primary",
  width = 12,
 DT::dataTableOutput("timetable"),
 br()
 )
 ),
 tabPanel(
  title = "REGISTRATION",
  br(),
   box(
    title = "Course Details",
    solidHeader = TRUE,
    status = "primary",
    width = 12,
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
    value =  NULL,
    placeholder = "Selected Unit"
   )
  ),
  div(
   loadingButton("register", "Register",
                 style = "width: 110px;",
                 loadingLabel = "Registering",
                 loadingSpinner = "cog")
   
  ),
         tags$div(
          style = "color: red; font-weight: bold;",
         textOutput("notice"),
          tags$div(id = "flip",
                    tags$div(
                     class = "time",
                     textOutput("days"),
                     "Days"),
                   tags$div(
                    class = "time",
                    textOutput("hours" ),
                    "Hours"),
                   tags$div(
                    class = "time",
                    textOutput("minutes"),
                    "Minutes"),
                   tags$div(
                    class = "time",
                    textOutput("seconds"),
                    "Seconds")
                   )
         )
  )
  ),
  box(
   title = "Student - Year Courses",
   solidHeader = TRUE,
   status = "primary",
   width = 12,
   DT::dataTableOutput("registered_units"),
   br()
   )
  ),
 tabPanel(
  title = "RESULTS",
  br(),
  fluidRow(
   column(6,
          box(
           solidHeader = TRUE,
           status = "primary",
           width = 12,
           title = "FIRST YEAR",
           DT::dataTableOutput("year_1_marks")
          )
          ),
  column(6,
         box(
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          title = "SECOND YEAR",
         DT::dataTableOutput("year_2_marks")
         )
         )
  ),
  fluidRow(
   column(6,
   box(
    solidHeader = TRUE,
    status = "primary",
    width = 12,
    title = "THIRD YEAR",
    DT::dataTableOutput("year_3_marks")
    )
   ),
  column(6,
         box(
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          title = paste("FOURTH YEAR"),
          DT::dataTableOutput("year_4_marks")
          )
         )
  )
  ),
 tabPanel(
  title = "ANALYSIS",
  fluidRow(
   column(4,
          box(
           title = "Final Averages",
           solidHeader = TRUE,
           status = "primary",
           width = 12,
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
          )
   ),
  column(4,
  box(
   title = "Final graph",
   solidHeader = TRUE,
   status = "primary",
   width = 12,
   echarts4rOutput("graph")
   )
  ),
  column(4,
  box(
   title = "Final Score",
   solidHeader = TRUE,
   status = "primary",
   width = 12,
   echarts4rOutput("clock")
   )
  )
  )
 )
  )
 )
 )
