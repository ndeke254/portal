ui <- navbarPage(
 position = c("fixed-top"), # nolint
 footer = tags$div(
    tags$hr(),
    tags$div(
      class = "p7",
      tags$div(
        class = "social",
        actionLink(
                   inputId = "linkedin",
                   label = "",
                   icon = icon("linkedin"),
                   onclick = "window.open(
                     'https://www.linkedin.com/in/jefferson-ndeke-027062202/')")
      ),
      tags$div(
        class = "social",
        actionLink(
                   inputId = "github",
                   label = "",
                   icon = icon("github"),
                   onclick = "window.open('https://github.com/ndeke254')")
      ),
      tags$div(
        class = "social",
        actionLink(
                   inputId = "twitter",
                   label = "",
                   icon = icon("twitter"),
                   onclick = "window.open(
                     'https://twitter.com/jefferson_ndeke')")
      ),
      tags$div(
        class = "social",
        actionLink(
                   inputId = "email",
                   label = "",
                   icon = icon("envelope"),
                   onclick = "window.open('mailto:musumbijefferson@gmail.com')")
      ),
      tags$div(
        class = "social",
        actionLink(
                   inputId = "phone",
                   label = "",
                   icon = icon("phone"),
                   onclick = "window.open('070-------')")
      ),
      br(),
      br(),
      br(),
      tags$div(
       style = "font-size: 10px;",
      "© 2024 Jefferson. All rights reserved."
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
 title = tags$img(src = "logo.png", draggable = FALSE), # nolint
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
            selectizeInput(
              inputId = "reg",
              label = "Registration number", # nolint
              multiple = FALSE,
              choices = NULL
            ),
            disabled(
              textInput(
                inputId = "name",
                label = "Student name",
                placeholder = "Selected student"
              )
            ),
            selectizeInput(
              inputId = "code",
              label = "Code", # nolint
              multiple = FALSE,
              choices = NULL
            ),
            disabled(
              textInput(
                inputId = "course",
                label = "Course",
                placeholder = "Selected course"
              )
            ),
            numericInput(
              inputId = "score",
              label = "Score",
              value = NULL,
              min = 1,
              max = 99
            ),
            disabled(
              textInput("grade", "Grade"),
              hidden(textInput("id", "ID", value = 0)),
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
        title = "REGISTRATION",
        box(
          title = "Enter Student Details",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          splitLayout(
            cellWidths = c("15.111%", "7.111%", "11.111%", "5.111%", "11.111%",
                           "20.111%", "11.111%", "1.111%"),
            cellArgs = list(style = "padding: 3px"),
            textInput(
             label = "Name",
             inputId = "Name",
             placeholder = "Enter Student name"
             ),
            selectInput(
              inputId = "Gender",
              label = "Gender",
              choices = c("Male", "Female"),
              selected = NULL
            ),
            numericInput(
              inputId = "ID", "ID",
              value = ""
            ),
            selectInput(
              inputId = "Code",
              label = "Code",
              choices = c("X74", "X75"),
              selected = NULL
            ),
            disabled(
              hidden(
                textInput(
                  inputId = "Date",
                  label = "Date",
                  value = format(Sys.time(), "%d/%m/%Y %H:%M:%S")
                )
              ),
              textInput(
                inputId = "Reg",
                label = "Registration Number",
                value = "",
                placeholder = "Enter Registration number"
              )
            ),
            fileInput(
              inputId = "photoInput",
              label = "Select Photo",
              accept = c("image/jpeg", "image/png"),
            ),
            imageOutput(
              outputId = "previewImage",
              width = "100%",
              height = "65px"
            ),
            tags$div(
              hidden(
                textInput(
                  inputId = "Course",
                  label = "Course"
                )
              ),
              hidden(
                textInput(
                  inputId = "Buttons",
                  label = ""
                )
              ),
              hidden(
                textInput(
                  inputId = "Year",
                  label = "Year",
                  value = "1"
                )
              )
            ),
            loadingButton(
              inputId = "registerButton",
              label = "Register",
              style = "width: 110px; margin-top: 30px;",
              loadingLabel = "Registering",
              loadingSpinner = "cog"
            )
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
      ),
      tabPanel(
        title = "STATISTICS",
        box(
          title = "Real-time Track",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          fluidRow(
            valueBoxOutput(
              outputId = "registered_no",
              width = 2
            ),
            valueBoxOutput(
              outputId = "available_marks",
              width = 2
            ),
            valueBoxOutput( 
              outputId = "missing_marks",
              width = 2
            ),
            valueBoxOutput(
              outputId = "failed_marks",
              width = 2
            ),
            valueBoxOutput(
              outputId = "waiting_promotion",
              width = 2
            ),
            valueBoxOutput(
              outputId = "grad_students",
              width = 2
            )
          )
        ),
        box(
          title = "Course Registration",
          solidHeader = TRUE,
          status = "primary",
          width = 12,
          fluidRow(
            column(
              width = 12,
              align = "center",
              br(),
              switchInput(inputId = "switch")
            )
          ),
          fluidRow(
            column(
              width = 3,
              align = "center",
              box(
                title = "Set Date/Time",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                dateInput(
                  inputId = "target_date",
                  label = "Select a End Date:",
                  format = "dd-mm-yyyy",
                  min = Sys.Date()
                ),
                timeInput(
                  inputId = "select_time",
                  label = "Enter End time"
                ),
                br(),
                tags$div(
                  actionButton(
                    inputId = "open",
                    label = "Open",
                    icon = icon("unlock")
                  ),
                  actionButton(
                    inputId = "close",
                    label = "Close",
                    icon = icon("lock")
                  )
                )
              )
            ),
            column(3, align = "center",
              box(
                title = "End Date/Time",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                textOutput(
                  outputId = "flip_time"
                ),
                tags$div(
                  id = "table",
                  DT::dataTableOutput("set_time")
                )
              )
            ),
            column(
              width = 3,
              align = "center",
              box(
                title = "CountDown",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                textOutput("remaining_time"),
                br(),
                shinyWidgets::progressBar(
                  id = "progress_bar",
                  value = 50
                )
              )
            ),
            column(
              width = 3,
              align = "center",
              box(
                title = "Write Notice",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                textAreaInput(
                  inputId = "post_writeup",
                  label = "",
                  value = "",
                  placeholder = "Enter your announcement here..."
                ),
                br(),
                tags$div(
                  actionButton(
                    inputId = "post",
                    label = "Post",
                    icon = icon("bullhorn")
                  ),
                  actionButton(
                    inputId = "unpost",
                    label = "Edit",
                    icon = icon("pencil")
                  )
                )
              )
            )
          )
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
            disabled(
             textInput(
              inputId = "student_name",
              label = "Student name",
              placeholder = "Selected student"
             )
            ),
            disabled(
             textInput(
              inputId = "student_course",
              label = "Student course",
              placeholder = "Degree programme"
             )
            ),
            disabled(
             textInput(
              inputId = "current_year",
              label = "Year of study",
              placeholder = "Current student year"
             )
            ),
            progress_circle(
              value = 0,
              shiny_id = "bar",
              color = "green",
              trail_width = 4,
              width = "100%",
              height = "100px"
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
           selectizeInput(
            inputId = "type",
            label = "Exam Type", # nolint
            multiple = FALSE,
            choices = c("FIRST ATTEMPT", "RETAKE 1",
                        "RETAKE 2", "SUPPLEMENTARY")
           ),
            disabled(
              selectizeInput(
                inputId = "register_code",
                label = "Code", # nolint
                multiple = FALSE,
                choices = NULL
              )
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
              loadingButton(
                inputId = "register",
                label = "Register",
                style = "width: 110px;",
                loadingLabel = "Registering",
                loadingSpinner = "cog"
              )
            ),
            tags$div(
              style = "color: red; font-weight: bold;",
              textOutput("notice"),
              tags$div(id = "flip",
                tags$div(
                  class = "time",
                  textOutput(
                    outputId = "days"
                  ),
                  "Days"
                ),
                tags$div(
                  class = "time",
                  textOutput(
                    outputId = "hours"
                  ),
                  "Hours"
                ),
                tags$div(
                  class = "time",
                  textOutput(
                    outputId = "minutes"
                  ),
                  "Minutes"
                ),
                tags$div(
                  class = "time",
                  textOutput(
                    outputId = "seconds"
                  ),
                  "Seconds"
                )
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
       title = "TOKENS",
       br(),
        column(
         width = 12,
         fluidRow(
         box(
        title = "Tuition Fees Track",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        div(
         id = "tuition",
         valueBoxOutput(
          outputId = "total_fees",
          width = 3
         ),
         valueBoxOutput(
          outputId = "paid_fees",
          width = 3
         ),
         valueBoxOutput( 
          outputId = "balance_fees",
          width = 3
         )
         )
        )
       )
       ),
      column(12,
             box(
              solidHeader = TRUE,
                status = "primary",
                width = 12,
                title = paste("Student Tokens"),
                DT::dataTableOutput("student_tokens")
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
                downloadButton(
                  outputId = "download",
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
