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
 title = tags$div(
  tags$img(src = "logo.png", draggable = FALSE),
  actionBttn(
   inputId = "version",
   label = textOutput("version_text"), 
   style = "stretch",
   color = "primary"
  )
 ),
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
           style = "display: flex; flex-wrap: nowrap; 
           align-items: center; gap: 5px;",
           selectizeInput(
            inputId = "reg",
            label = "Registration number", # nolint
            multiple = FALSE,
            choices = NULL,
            width = "150px"
           ),
           disabled(
            textInput(
             inputId = "name",
             label = "Student name",
             placeholder = "Selected student",
             width = "250px"
            )
           ),
           selectizeInput(
            inputId = "code",
            label = "Code", # nolint
            multiple = FALSE,
            choices = NULL,
            width = "100px"
            ),
           disabled(
            textInput(
             inputId = "course",
             label = "Course",
             placeholder = "Selected course",
             width = "300px"
             )
           ),
           numericInput(
            inputId = "score",
            label = "Score",
            value = NULL,
            min = 1,
            max = 99,
            width = "100px"
            ),
           disabled(
            textInput("grade", "Grade", width = "100px"),
            hidden(textInput("id", "ID", value = 0)),
            hidden(textInput("time", "Time")),
            hidden(textInput("lecturer", "Lecturer")),
            hidden(textInput("actions", "Actions"))
           ),
           loadingButton("submit", "Submit",
                         style = "width: 100px",
                         loadingSpinner = "cog"
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
     style = "display: flex; flex-wrap: nowrap;
     align-items: center; gap: 5px;",
     textInput(
      label = "Name",
      inputId = "Name",
      placeholder = "Enter Student name",
      width = "700px"
      ),
     selectInput(
      inputId = "Gender",
      label = "Gender",
      choices = c("Male", "Female"),
      selected = NULL,
      width = "300px"
      ),
     numericInput(
      inputId = "ID", "ID",
      value = "",
      width = "400px"
      ),
     selectInput(
      inputId = "Code",
      label = "Code",
      choices = c("X74", "X75"),
      selected = NULL,
      width = "300px"
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
       placeholder = "Enter Registration number",
       width = "500px"
       )
      ),
     fileInput(
      inputId = "photoInput",
      label = "Select Photo",
      accept = c("image/jpeg", "image/png"),
      width = "900px"
      ),
     imageOutput(
      outputId = "previewImage",
      width = "6%",
      height = "65px"
      ),
     hidden(
      textInput(inputId = "Course", label = "Course"),
      textInput(inputId = "Buttons", label = ""),
      textInput(inputId = "Year", label = "Year", value = "1")
      ),
     loadingButton(
      inputId = "registerButton",
      label = "Register",
      style = "width: 110px;",
      loadingLabel = "Registering",
      loadingSpinner = "cog"
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
      column(3, 
             align = "center",
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
    ),
   tabPanel(
    title = "UPDATES",
    value = "version",
    icon = icon("code-compare"),
    box(
     title = "Update App Version",
     solidHeader = TRUE,
     status = "primary",
     width = 12,
     style = "display: flex; flex-wrap: nowrap;
     align-items: center; gap: 5px;",
     selectizeInput(
      inputId = "change_extent",
      label = "Extent of changes", # nolint
      multiple = FALSE,
      choices = c("", "Major", "Minor", "Patch"),
      width = "200px"
     ),
     selectizeInput(
      inputId = "change_type",
      label = "Nature of change", # nolint
      multiple = FALSE,
      choices = c("", "Add", "Remove", "Modify/Rewrite"),
      width = "200px"
     ),
     selectizeInput(
      inputId = "affected_area",
      label = "Affected area", # nolint
      multiple = FALSE,
      choices = c("", "Tab", "Feature", "Style", "Bug fix"),
      width = "200px"
     ),
     selectizeInput(
      inputId = "admin_thought",
      label = "The change is involves", # nolint
      multiple = FALSE,
      choices = c("", "Significant rewrites", 
                  "Architectural modifications", 
                  "Minor fixes"
                  ),
      width = "500px"
     ),
     disabled(
     textAreaInput(
      inputId = "summary_changes",
      value = "",
      label = "Summary",
      width = "600px",
      placeholder = "Summary of changes..."
      )
     ),
     loadingButton(
      inputId = "updateVersion",
      label = "Update",
      style = "width: 110px;",
      loadingLabel = "Updating...",
      loadingSpinner = "cog"
     )
     )
    )
   )
  ),
 tabPanel(
  title = "Student",
  value = "student",
  icon = icon("children"),
  div(
   id = "studentDetails",
   style = "padding-top: 70px;",
  box(
   title = "Student Details",
   solidHeader = TRUE,
   status = "primary",
   width = 12,
   style = "display: flex; flex-wrap: nowrap;
     align-items: center; gap: 5px;",
    selectizeInput(
     inputId = "student_reg",
     label = "Registration number", # nolint
     multiple = FALSE,
     choices = NULL,
     width = "400px"
    ),
    disabled(
     textInput(
      inputId = "student_name",
      label = "Student name",
      placeholder = "Selected student",
      width = "400px"
      )
    ),
    disabled(
     textInput(
      inputId = "student_course",
      label = "Student course",
      placeholder = "Degree programme",
      width = "700px"
      )
    ),
    disabled(
     textInput(
      inputId = "current_year",
      label = "Year of study",
      placeholder = "Current student year",
      width = "400px"
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
  div(
   id = "studentTabs",
  style = "margin-top: -80px;",
  tabsetPanel(
   tabPanel(
    title = "TIMETABLE",
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
     style = "display: flex; flex-wrap: nowrap;
     align-items: center; gap: 5px;",
     selectizeInput(
      inputId = "type",
      label = "Exam Type", # nolint
      multiple = FALSE,
      choices = c("FIRST ATTEMPT", "RETAKE 1",
                  "RETAKE 2", "SUPPLEMENTARY"),
      width = "250px"
      ),
     disabled(
      selectizeInput(
       inputId = "register_code",
       label = "Code", # nolint
       multiple = FALSE,
       choices = NULL,
       width = "200px"
       )
      ),
     disabled(
      textInput(
       inputId = "register_unit",
       label = "Register Unit", # nolint
       value =  NULL,
       placeholder = "Selected Unit",
       width = "300px"
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
      style = "color: red; font-weight: bold; margin-left: 85px;
      border-radius: 5px; padding: 5px; box-shadow: 1px 1px 3px #00000099;",
      textOutput("notice"),
      hr(),
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
    ),
    box(
     title = "Student - Registered Courses",
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
         ), valueBoxOutput(
          outputId = "paid_fees",
          width = 3
         ),
         valueBoxOutput( 
          outputId = "balance_fees",
          width = 3
         ),
         valueBoxOutput( 
          outputId = "last_token",
          width = 3
         )
         )
        )
       )
       ),
       fluidRow(
      column(12,
             box(
              solidHeader = TRUE,
                status = "primary",
                width = 12,
                title = paste("Student Tokens"),
                DT::dataTableOutput("student_tokens")
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
)
