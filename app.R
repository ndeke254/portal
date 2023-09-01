library(shiny)
library(rdrop2)
library(shinyFeedback) # nolint
library(shinyjs)
library(shinyBS) # nolint
library(readr)
library(shinyvalidate)
library(DT) # nolint
library(tidyverse)
library(prodlim)
library(shinyWidgets) # nolint: object_name_linter.
library(stringr)
library(echarts4r)
library(pagedown)
#import data
units <- read_csv("data/units.csv",show_col_types = FALSE) # nolint
source("modal_dialog.R")
# Define the fields we want to save from the form
marks_fields <- c("reg", "name", "code", "course",
            "score", "grade", "time", "lecturer","actions")
users <- c("DR.JOHN", "PROF.MAKORE", "MR.WAMAE", "MS.MUTUA", "DR.OCHIENG")# nolint
reg_fields <- c("Name",	"Gender",	"ID",	"Reg","Code","Course", "Date",	"Buttons","Year")
# use this list for all your toasts
myToastOptions <- list( # nolint
 positionClass = "toast-top-right", # nolint
 progressBar = FALSE,
 timeOut = 3000,
 closeButton = FALSE,
 # same as defaults
 newestOnTop = TRUE,
 preventDuplicates = TRUE,
 showDuration = 300,
 hideDuration = 1000,
 extendedTimeOut = 1000,
 showEasing = "linear",
 hideEasing = "linear",
 showMethod = "fadeIn", # nolint
 hideMethod = "fadeOut" # nolint
)
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
  title = "RESULTS"
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
  selectizeInput(
   inputId = "register_code",
   label = "Code", # nolint
   multiple = FALSE,
   choices = NULL
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
server <- function(input, output, session){
 #create admin icons
 shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
   inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
 }
 #ADMINISTRTORS
 registeredDir <- "registered_students"
 # Create reactive data frame for registration data
 #data manipulation function
 registrationData <- function(){
  #create an ideal button
  ideal <- '<button id="deletereg_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;deletereg_button&quot; , this.id, {priority: &quot;event&quot;})" title="Delete"> <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="editreg_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;editreg_button&quot; , this.id, {priority: &quot;event&quot;})" title="Edit"> <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
    <button id="year_ 1" type="button" class="btn btn-default action-button" style="color: red;"   onclick="Shiny.onInputChange( &quot;year_button&quot; , this.id, {priority: &quot;event&quot;})" title="Promote"> <i class="fa fa-plus" role="presentation" aria-label="file-pen icon"></i></button>
     <button id="Edited" type="button" class="btn btn-default action-button" style="background-color: #e9ecef; " disabled title="Edited">NO</button>'
  #registrationData <-  Read all the files into a list
  registered_files <- list.files(registeredDir, full.names = TRUE) 
  if (length(registered_files) == 0 ) {
    data <- data.frame( 
    Name = character(0), 
    Gender = character(0), 
    ID = character(0),
    Reg = character(0), 
    Code = character(0),
    Course = character(0),
    Date = character(0), 
    Buttons = character(0),
    Year = character(0),
    Filename = character(0),
    stringsAsFactors = FALSE
    )
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value = ideal
    )
   data
  } else if (length(registered_files) == 1){
   registered_data <- lapply(registered_files, read.csv,
                             stringsAsFactors = FALSE)
   #set as a data frame
   registered_data <- as.data.frame(registered_data)
   #Assign name column
   registered_data$Filename <- registered_files
   #data set
   data <- registered_data
   # Set column names using colnames
   colnames(data) <- c("Name", "Gender", "ID", "Reg", "Code","Course","Date",
                       "Buttons","Year","Filename"
                       )
   data
   button <- data |> select(Buttons)
   button  <- button[[1,1]]
   # Find all IDs containing a number
   ids <- str_extract_all(button, "(?<=id=\")\\w+\\s\\d+(?=\")")[[1]]
   # Replace IDs with the next number
   new_button <- button
   for (id in ids) {
    number <- str_extract(id, "\\d+")
    next_number <- as.numeric(number) + 1
    new_id <- gsub("\\d+", next_number, id)
    new_button <- str_replace(new_button, id, new_id)
   }
   new <- gsub("background-color: #0025ff8f;", 
               "background-color: #e9ecef;", new_button)
   new1 <- gsub("YES","NO",new)  
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value =  new1
   )
   data
  }else {
   registered_data <- lapply(registered_files, read.csv, 
                             stringsAsFactors = FALSE)
   # Concatenate all data together into one data.frame
   registered_data <- do.call(rbind, registered_data)
   #arrange with latest on top
   registered_data <- apply(registered_data, 2, rev)
   #set as a data frame
   registered_data <- as.data.frame(registered_data)
   #assign Filename column
   registered_data$Filename <-  rev(registered_files)
   #combine the two sets of data
   data <- registered_data |> arrange(desc(Date))
   # Set column names using colnames
   colnames(data) <- c("Name", "Gender", "ID", "Reg", "Code","Course","Date",
                       "Buttons","Year","Filename"
   )
   data
   button <- data |> select(Buttons)
   button  <- button[[1,1]]
   # Find all IDs containing a number
   ids <- str_extract_all(button, "(?<=id=\")\\w+\\s\\d+(?=\")")[[1]]
   # Replace IDs with the next number
   new_button <- button
   for (id in ids) {
    number <- str_extract(id, "\\d+")
    next_number <- as.numeric(number) + 1
    new_id <- gsub("\\d+", next_number, id)
    new_button <- str_replace(new_button, id, new_id)
   }
   new <- gsub("background-color: #0025ff8f;", 
               "background-color: #e9ecef;", new_button)
   new1 <- gsub("YES","NO",new)  
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value =  new1
   )
  }
  data
  } 
# Create a vector to store used numbers
 usedNumbers <- function() {
  # Extract numbers between slashes
  numbers <- sub(".*/(\\d+)/.*", "\\1", registrationData()$Reg)
  numbers <- as.numeric(numbers)
  numbers
 }
 # Create reactive value to store the generated number
 generatedNumber <- reactiveVal(NULL)
 new_reg <- function(){
  if(length(usedNumbers()) == 9000) {
   showToast("info",
             "All Registration Numbers taken!",
             keepVisible = TRUE,
             .options = myToastOptions )
  } else {
   code <- isolate(input$Code)
   date <-Sys.time()
   year <- format(date, "%Y")
   # Generate a unique 4-digit number
   num <- sample(setdiff(1000:9999, usedNumbers()), 1)
   # Update the generatedNumber reactive value
   generatedNumber(num)
   reg <- paste0(code, "/", num, "/", year)
   updateTextInput(session, "Reg", value = reg)
  }
 }
 
 observeEvent(input$Code, {
  if (!is.null(code) && !is.null(date) && input$editmode == 0) {
   new_reg()
  } else if (!is.null(code) && !is.null(date) && input$editmode == 1){
   #image to delete
   string <- input$Reg
   #replacing string
   replacement <- input$Code
   # Replace characters before the first slash with the new value
   result <- sub("^[^/]+", replacement, string)
   #update the regInput 
   updateTextInput(session = session, inputId = "Reg",
                   value = result
   )
  }
 })

 # Event handler for register button click
 observeEvent(input$registerButton, {
  photo <- isolate(input$photoInput)
  # Check if the ID input has 8 characters
  if (nchar(input$ID) != 8 || is.na(input$ID)) {
   showToast("error",
             "ID should be 8 characters!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()
  }
  # Check if the name input is empty
  if (is.null(input$Name) || input$Name == "") {
   showToast("info",
             "Enter Name!",
             .options = myToastOptions ) 
   resetLoadingButton("registerButton")
   return()
  }
  # Check if the reg input is empty
  if (is.null(input$Reg) || input$Reg == "") {
   showToast("info",
             "Refresh Registration number field!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()
  }
  # Check if a photo is uploaded
  if (is.null(photo) || is.na(photo$datapath) || input$toggle == "0"){
   showToast("error",
             "Please appload Photo!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()
  }
  #ensure no duplicates for ID
  # Check if the entered ID already exists in the column
  if(input$ID %in%registrationData()$ID && input$editmode == 0) {
   showToast("error",
             "ID Number Exists!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()  # Stop execution if duplicate ID
  }
  if(input$editmode == "0"){
  # Whenever all fields are filled, aggregate form data
  formData <- reactive({
   data <- sapply(reg_fields, function(x) input[[x]])
   data
  })
  formData()
  # Show the previous responses
  # (update with current response when Submit is clicked)
  saveData <- function(data) {
   data <- t(data)
   # Create a unique file name
   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), 
                       digest::digest(data))
   # Write the file to the local system
   write.csv(
    x = data,
    file = file.path(registeredDir, fileName),
    row.names = FALSE, quote = TRUE
   )
  }
  saveData(formData())
  # Get the file extension of the uploaded photo
  extension <- tools::file_ext(photo$name) 
  # Generate a unique filename using the Reg value and file extension
  filename <- paste0(generatedNumber(), ".", extension)
  # Move the uploaded photo to the desired directory and rename it
  file.rename(photo$datapath, paste0("data/images/",filename))
  # Clear the input fields
  updateTextInput(session, "Name", value = "")
  updateNumericInput(session, "ID", value = "")
  updateSelectInput(session, "Gender", selected = NULL)
  updateTextInput(session, "toggle", value = "0")
  new_reg()
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   choices = registrationData()$Reg,
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "student_reg",
   choices = registrationData()$Reg,
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
  # Render the registration data table
  output$registrationTable <- renderDataTable(
   registrationData(),server = TRUE, escape = FALSE, selection = "none",
   options = list(
    columnDefs = list(
     list(targets = c(1,2,5,6,7,8), orderable = FALSE),#Disable sorting
     list(targets = c(5,6,10), visible = FALSE)# column you want to hide
    )
   )
  )
  showToast("success",
            "New student Registered!",
            .options = myToastOptions )
  resetLoadingButton("registerButton")
  reset("photoInput")
  return()
  }
  
  if(input$editmode == "1"){
   selectedRow <- as.numeric(strsplit(input$editreg_button, "_")[[1]][2])
   search_string <- paste0("editreg_ ",selectedRow)
   row <- registrationData()[grepl(search_string,
                                           registrationData()$Buttons), ]
   originalFile <- row$Filename 
   # Source directory and file name
   source_dir <- "registered_students"
   fileName <- gsub("registered_students/", "", originalFile)
   file_name <- fileName 
   # Destination directory
   dest_dir <- "edited_results"
   #image to delete
   string <- row$Reg
   # Extract the number between slashes
   number <- str_extract(string, "(?<=/)[0-9]+(?=/)")
   # Get the list of image available
   image_files <- list.files("data/images", full.names = TRUE) 
   # Generate a unique filename using the regInput value and file extension
   filename <- paste0(number, ".")
   #filter the image to remove
   image <- image_files[grepl(filename, image_files)]
   #remove existing photo
   file.remove(image)
   # Create the full paths
   row <- subset(row, select = -Filename)
   write.csv(
    x = row,
    file = file.path(dest_dir, fileName),
    row.names = FALSE, quote = TRUE
   )
   #change button appearance
   button <- row$Buttons
   new <- gsub("background-color: #e9ecef;", 
               "background-color: #0025ff8f;", button)
   new1 <- gsub("NO","YES",new)
   #create new file
   newFile <- data.frame(
    Name = input$Name, 
    Gender = input$Gender,
    ID = input$ID, 
    Reg = input$Reg,	
     Code = input$Code, 
    Course = input$Course,	
    Date = row$Date, 
    Buttons = new1,
    Year = row$Year
    )
   # Write the file to the local system
   write.csv(
    x = newFile,
    file = file.path(registeredDir, file_name),
    row.names = FALSE, quote = TRUE
   )
   # Get the file extension of the uploaded photo
   extension <- tools::file_ext(photo$name) 
   # Generate a unique filename using the regInput value and file extension
   filenames <- paste0(number, ".", extension)
   # Move the uploaded photo to the desired directory and rename it
   file.rename(photo$datapath, paste0("data/images/",filenames))
   #show new data frame
    output$registrationTable <- renderDataTable(
   registrationData(),server = TRUE, escape = FALSE, selection = "none",
   options = list(
    columnDefs = list(
     list(targets = c(5,6,10), visible = FALSE),# column you want to hide
     list(targets = c(1,2,5,6,7,8), orderable = FALSE)#Disable sorting
    )
   )
  )
   # Clear the input fields
   updateTextInput(session, "Name", value = "")
   updateNumericInput(session, "ID", value = "")
   updateSelectInput(session, "Gender", selected = NULL)
   updateTextInput(session, "toggle", value = "0")
   reset("Date")
   reset("photoInput")
   new_reg()
   resetLoadingButton("registerButton")
   updateTextInput(session, "editmode", value = 0)
   showToast(
    "success", "Student Edited!", .options = myToastOptions
   )
   hideToast(animate = TRUE, session = session)
   shinyjqui::jqui_hide("#reset_button", effect = "fade")
   return()
  }
 })
 #reset toggle field
 observeEvent(input$photoInput, {
  updateTextInput(session, "toggle", value = "1")
  updateTextInput(session = session, 
                  inputId = "Date",
                  value = format(Sys.time(), "%d-%m-%Y %H:%M")
  )
 })
 # Render the first registration data table
 output$registrationTable <- renderDataTable(
  registrationData(),server = TRUE, escape = FALSE, selection = "none",
  options = list(
   columnDefs = list(
    list(targets = c(5,6,10), visible = FALSE),# column you want to hide
    list(targets = c(1,2,5,6,7,8), orderable = FALSE)#Disable sorting
   )
  )
 )

 #LECTURERS
 #entered data
 releasedDir <- "released_results"
 #update choices
 updateSelectizeInput(
  session = session,
  inputId = "reg",
  choices = registrationData()$Reg,
  selected = "",
  server = TRUE,
  options = list(maxOptions = 3)
 )
 #update choices
 updateSelectizeInput(
  session = session,
  inputId = "student_reg",
  choices = registrationData()$Reg,
  selected = "",
  server = TRUE,
  options = list(maxOptions = 3)
 )

 updateTextInput(
  session = session,
  inputId = "id",
  value = 0
 )
 
 #data manipulation function
 loadData <- function(){
  #create an ideal button 
 ideal <- '<button id="approve_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})" title="Approve">  <i class="fas fa-check-to-slot" role="presentation" aria-label="check-to-slot icon"></i></button>
    <button id="delete_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})" title="Delete">  <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="edit_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})" title="Edit">  <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
      <button id="Edited" type="button" class="btn btn-default action-button" style="background-color: #e9ecef; " disabled title="Edited">NO</button>'
  # Read all the files into a list
  released_files <- list.files(releasedDir, full.names = TRUE) 
  if (length(released_files) == 0) {
   data <- data.frame(reg = character(0), 
                      name = character(0),
                      code =  character(0),
                      course = character(0),
                      score = numeric(0),
                      grade = character(0),
                      time =  character(0),
                      lecturer =  character(0),
                      action =  character(0),
                      stringsAsFactors = FALSE)
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "actions",
    value = ideal
   )
   data
  } else if (length(released_files) == 1){
   released_data <- lapply(released_files, read.csv, stringsAsFactors = FALSE)
   #set as a data frame
   released_data <- as.data.frame(released_data)
   #change to 1 decimal place
   released_data$score <- as.numeric(released_data$score) |> round(0)
   released_data$filename <- released_files
   #data set
   data <- released_data
   data
    button <- data |> select(actions)
    button  <- button[[1,1]]
    # Find all IDs containing a number
    ids <- str_extract_all(button, "(?<=id=\")\\w+\\s\\d+(?=\")")[[1]]
    # Replace IDs with the next number
    new_button <- button
    for (id in ids) {
     number <- str_extract(id, "\\d+")
     next_number <- as.numeric(number) + 1
     new_id <- gsub("\\d+", next_number, id)
     new_button <- str_replace(new_button, id, new_id)
    }
    new <- gsub("color: green;", 
                "color: red;" , new_button) 
    new1 <- gsub("YES","NO",new)  
    new2 <- gsub("background-color: #0025ff8f;", 
                "background-color: #e9ecef;", new1)
    #update the buttons
    updateTextInput(
     session = session,
     inputId = "actions",
     value =  new2
    )
   data
   }else {
   released_data <- lapply(released_files, read.csv, stringsAsFactors = FALSE)
   # Concatenate all data together into one data.frame
   released_data <- do.call(rbind, released_data)
   #arrange with latest on top
   released_data <- apply(released_data, 2, rev)
   #set as a data frame
   released_data <- as.data.frame(released_data)
   #change to 1 decimal place
   released_data$score <- as.numeric(released_data$score) |> round(0)
   released_data$filename <-  rev(released_files)
   #combine the two sets of data
   data <- released_data |> arrange(desc(time))
   data
   button <- data |> select(actions)
   button  <- button[[1,1]]
   # Find all IDs containing a number
   ids <- str_extract_all(button, "(?<=id=\")\\w+\\s\\d+(?=\")")[[1]]
   # Replace IDs with the next number
   new_button <- button
   for (id in ids) {
    number <- str_extract(id, "\\d+")
    next_number <- as.numeric(number) + 1
    new_id <- gsub("\\d+", next_number, id)
    new_button <- str_replace(new_button, id, new_id)
   }
   new <- gsub("color: green;", 
               "color: red;" , new_button) 
   new1 <- gsub("YES","NO",new)  
   new2 <- gsub("background-color: #0025ff8f;", 
                "background-color: #e9ecef;", new1)
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "actions",
    value =  new2
   )
   data
  }
 }
 # When the Submit button is clicked, save the form data
 observeEvent(input$submit, {
  #put control to remove NA values
  if (input$reg == "" | input$code == "" | is.na(input$score)
      | input$grade == "") {
   resetLoadingButton("submit")
   showToast(
    "error", "Fill all fields!", .options = myToastOptions
   )
  } else if(input$reg != "" && input$code != "" && !is.na(input$score)
            && input$grade != "" && input$id == 0 ) {
   # Whenever all fields are filled, aggregate form data
   formData <- reactive({
    data <- sapply(marks_fields, function(x) input[[x]])
    data
   })
   formData()
   # Show the previous responses
   # (update with current response when Submit is clicked)
   saveData <- function(data) {
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), 
                        digest::digest(data))
    # Write the file to the local system
    write.csv(
     x = data,
     file = file.path(releasedDir, fileName),
     row.names = FALSE, quote = TRUE
    )
   }
   saveData(formData())
   resetLoadingButton("submit")
   # Show the previous responses
   # (update with current response when Submit is clicked)
   output$marks <- DT::renderDataTable(
    loadData(), server = TRUE, escape = FALSE, selection = "none",
    options = list(
     columnDefs = list(
      list(targets = c(10), visible = FALSE),# column you want to hide
      list(targets = c(1,2,3,4,5,6,7,8,9), orderable = FALSE)#Disable sorting
     )
    )
   )
   #reset all fields to blank
   #update choices
   updateSelectizeInput(
    session = session,
    inputId = "reg",
    choices = registrationData()$Reg,
    server = TRUE,
    selected = "",
    options = list(maxOptions = 3)
   )
   updateSelectizeInput(
    session = session,
    inputId = "code",
    choices = NULL,
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3)
   )
   updateNumericInput(
    session = session,
    inputId = "score",
    value = ""
   )
   updateTextInput(
    session = session,
    inputId = "id",
    value = 0
   )
   showToast(
    "success", "Mark entered", .options = myToastOptions
   )
  } else if(input$reg != "" && input$code != "" && !is.na(input$score)
            && input$grade != "" && input$id == 1 ) {
   row <- loadData()[loadData()$reg %in%input$reg &
                      loadData()$code %in%input$code, ] 
   originalFile <- row$filename 
   # Source directory and file name
   source_dir <- "released_results"
   fileName <- gsub("released_results/", "", originalFile)
   file_name <- fileName 
   # Destination directory
   dest_dir <- "edited_results"
   # Create the full paths
   write.csv(
    x = row,
    file = file.path(dest_dir, fileName),
    row.names = FALSE, quote = TRUE
   )
   #change button appearance
   button <- row$actions
   new <- gsub("background-color: #e9ecef;", 
               "background-color: #0025ff8f;", button)
   new1 <- gsub("NO","YES",new)
   #create new file
   newFile <- data.frame(
    reg = row$reg, name = row$name, code = row$code, course = row$course,
    score = input$score,	grade = input$grade,	time = row$time, 
    lecturer = row$lecturer, actions = new1
   )
   fileName <- gsub("released_results/", "", row$filename)
   # Write the file to the local system
   write.csv(
    x = newFile,
    file = file.path(releasedDir, fileName),
    row.names = FALSE, quote = TRUE
   )
   #show new data frame
   output$marks <- DT::renderDataTable(
    loadData(), server = TRUE, escape = FALSE, selection = "none",
    options = list(
     columnDefs = list(
      list(targets = c(10), visible = FALSE),# column you want to hide
      list(targets = c(1,2,3,4,5,6,7,8,9), orderable = FALSE)#Disable sorting
     )
    )
   )
   resetLoadingButton("submit")
   shinyjqui::jqui_hide("#exit_button", effect = "fade")
   showToast(
    "success", "Mark Edited!", .options = myToastOptions
   )
   #reset all fields to blank
   #update choices
   updateSelectizeInput(
    session = session,
    inputId = "reg",
    choices = registrationData()$Reg,
    server = TRUE,
    selected = "",
    options = list(maxOptions = 3)
   )
   updateSelectizeInput(
    session = session,
    inputId = "code",
    choices = NULL,
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3)
   )
   updateNumericInput(
    session = session,
    inputId = "score",
    value = ""
   )
   updateTextInput(
    session = session,
    inputId = "id",
    value = 0
   )
   hideToast(animate = TRUE, session = session)
  }
 })
 #update uneditable fields from database
 observe({
  x <- input$reg
  name_df <- registrationData() |> filter(Reg %in% x) %>% 
   select(Name)
  name <- name_df[[1]]
  updateTextInput(
   session = session,
   inputId = "name",
   value = name
  )
  
  y <- input$code
  course_df <- units |> filter(code %in% y) %>%  select(title)
  course <- course_df[[1]]
  updateTextInput(
   session = session,
   inputId = "course",
   value = course
  )
  #set a grade on scale
  z <- input$score
  grade <- case_when(
   z >= 70 ~ "A",
   z >= 60 ~ "B",
   z >= 50 ~ "C",
   z >= 40 ~ " D",
   z >= 1 ~ "E",
   TRUE ~ ""
  )
  t <- input$register_code
  course_df1 <- units |> filter(code %in% t) %>%  select(title)
  course1 <- course_df1[[1]]
  updateTextInput(
   session = session,
   inputId = "register_unit",
   value = course1
  )
  updateTextInput(
   session = session,
   inputId = "grade",
   value = grade
  )
  code <- input$Code
  # define course
  course <- reactive({
   if(code == "X74") {
    course <- "Economics"
   } else if (code == "X75") {
    course <- " Economics and Statistics"
   } else (
    return()
   )
  })
  updateTextInput(
   session = session,
   inputId = "Course",
   value = course()
  )
  data_entered <- c(input$reg, input$code)
  data_saved <- loadData() |>
   select(reg, code)
  #check for a row match
  if (is.na(row.match(data_entered, data_saved))) {
   enable("submit")
  } else if(row.match(data_entered, data_saved) && 
            input$id == 0) {
   row_wise <- loadData()[loadData()$reg %in%input$reg &
                           loadData()$code %in%input$code, ]
   updateNumericInput(
    session = session,
    inputId = "score",
    value = row_wise$score
   )
   showToast("error", "Mark already exists!",
             .options = myToastOptions
   )
   disable("submit")
   disable("score")
  } else if(row.match(data_entered, data_saved) && 
            input$id == 1) {
   enable("submit")
   enable("score")
  } else {
   return()
  }
 })
 observeEvent(input$score,{
  #record entry time and lecturer
  updateTextInput(
   session = session,
   inputId = "time",
   value = format(Sys.time(), "%d-%m-%Y %H:%M")
  )
  updateTextInput(
   session = session,
   inputId = "lecturer",
   value = sample(users, 1)
  )
  x <- input$score
  if(x > 99 |is.na(x)| x < 1 | nchar(x) > 2) {
   updateNumericInput(
    session = session,
    inputId = "score",
    value = ""
   )
  }else {
   return()
  }
 })
 observeEvent(input$reg, {
  enable("score")
  reg_no <- input$reg
  #student details
  student_data <- registrationData() |> filter(Reg %in% input$reg_no)
  student_year <- student_data$Year 
  student_course <- student_data$Code
  cleaned_reg <- gsub("/", "", reg_no)
  fileName <- paste0(cleaned_reg,".csv")
  registered_students <- list.files(registered_unitsDir, full.names = FALSE)
  t_b <- loadData()[grepl("green", loadData()$actions), ]
  t_b <- t_b |> filter(reg %in% input$reg_no) |>
   select(code, course, grade) 
  if(fileName %in% registered_students){
   file <- read.csv(file = paste0(registered_unitsDir,"/",fileName))
   #entered units
   file_1 <- file |>
    filter(Status %in% "REGISTERED")
   list1 <- as.vector(file_1$Code)
   file_2 <- file |>
    filter(Status %in% c("PASSED","FAILED"))
   list2 <- as.vector(file_2$Code)
   #student details
   student_data <- registrationData() |> filter(Reg %in% file$Reg[1])
   student_year <- student_data$Year 
   student_course <- student_data$Code
   student_units <- units |>
    filter(course %in% c("BOTH",student_course)) |>
    filter(year %in% student_year)
   list2 <- as.vector(student_units$code)
   list <- setdiff(list1, list2)
   if(length(list1) != 0 & input$id == 0){
    updateSelectizeInput(
     session = session,
     inputId = "code",
     choices = list1,
     selected = "",
     server = TRUE,
     options = list(maxOptions = 3)
    )
   }else if(length(list1) != 0 & input$id == 0){
    showToast("info", "All units passed!",
              .options = myToastOptions)
    updateSelectizeInput(
     session = session,
     inputId = "code",
     choices = NULL,
     server = TRUE
    )
   }else if(length(list1) == 0 & input$id == 0 & student_year <4) {
    showToast("info", "Register Year Units!",
              .options = myToastOptions)
  }
  }else if(reg_no == ""){
   updateSelectizeInput(
    session = session,
    inputId = "code",
    choices = NULL,
    server = TRUE
   )
  }else{
   showToast("info", "Register First Units!",
            .options = myToastOptions)
   updateSelectizeInput(
    session = session,
    inputId = "code",
    choices = NULL,
    server = TRUE
   )
 }
  })
 observeEvent(input$code, {
  enable("score")
 })
 #initial rendered data
 output$marks <- DT::renderDataTable(
  loadData(), server = TRUE, escape = FALSE, selection = "none",
  options = list(
   columnDefs = list(
    list(targets = c(10), visible = FALSE),# column you want to hide
    list(targets = c(1,2,3,4,5,6,7,8,9), orderable = FALSE)#Disable sorting
   )
  )
 )
 
 #delete a response file
  observeEvent(input$confirm_deletereg, {
   selectedRow <- as.numeric(strsplit(input$deletereg_button, "_")[[1]][2])
   search_string <- paste0("deletereg_ ",selectedRow)
   filtered_df <- registrationData()[grepl(search_string,
                                           registrationData()$Buttons), ]
   filename_delete <- filtered_df$Filename
   #image to delete
   string <- filtered_df$reg
   # Extract the number between slashes
   number <- str_extract(string, "(?<=/)[0-9]+(?=/)")
   # Get the list of image available
   image_files <- list.files("data/images", full.names = TRUE) 
   # Generate a unique filename using the regInput value and file extension
   filename <- paste0(number, ".")
   #filter the image to remove
   image <- image_files[grepl(filename, image_files)]
   #remove existing photo
   file.remove(image)
   # Source directory and file name
   source_dir <- "registered_students"
   fileName <- gsub("registered_students/", "", filename_delete)
   file_name <- fileName 
   # Delete file completely
   file.remove(paste0("registered_students/",file_name))
   #update choices
   updateSelectizeInput(
    session = session,
    inputId = "reg",
    choices = registrationData()$Reg,
    server = TRUE,
    selected = "",
    options = list(maxOptions = 3)
   )
   updateSelectizeInput(
    session = session,
    inputId = "student_reg",
    choices = registrationData()$Reg,
    server = TRUE,
    selected = "",
    options = list(maxOptions = 3)
   )
   output$registrationTable <- renderDataTable(
    registrationData(),server = TRUE, escape = FALSE, selection = "none",
    options = list(
     columnDefs = list(
      list(targets = c(5,6,10), visible = FALSE),# column you want to hide
      list(targets = c(1,2,5,6,7,8), orderable = FALSE)#Disable sorting
     )
    )
   )
   updateTextInput( session = session, inputId = "editmode", value = 0)
   hideToast(animate = TRUE, session = session)
   new_reg()
   showToast("success",
             "Student details Deleted!",
             .options = myToastOptions )
   removeModal()
 })
 #approve a response file
 observeEvent(input$confirm_approve, {
  selectedRow <- as.numeric(strsplit(input$approve_button, "_")[[1]][2])
  search_string <- paste0("approve_ ",selectedRow)
  filtered_df <- loadData()[grepl(search_string,
                                  loadData()$actions), ]
  grade <- filtered_df$grade
  filename_approve <- filtered_df$filename
  buttons <- filtered_df$actions
  file <- read.csv(filename_approve, stringsAsFactors = FALSE)
  # change buttons
  file$actions <- gsub("color: red;", 
                       "color: green;", buttons)
  #write the file
  write.csv(file,filename_approve,row.names = FALSE, quote = TRUE)
  #render new data table
  output$marks <- DT::renderDataTable(
   loadData(), server = TRUE, escape = FALSE, selection = "none",
   options = list(
    columnDefs = list(
     list(targets = c(10), visible = FALSE),# column you want to hide
     list(targets = c(1,2,3,4,5,6,7,8,9), orderable = FALSE)#Disable sorting
    )
   )
  )
  showToast("success", "Mark Approved!",
            .options = myToastOptions )
  updateNumericInput(
   session = session,
   inputId = "score",
   value = NULL
  )
  removeModal()
  reg_no <- filtered_df$reg
  cleaned_reg <- gsub("/", "", reg_no)
  fileName <- paste0(cleaned_reg,".csv")
  registered_students <- list.files(registered_unitsDir, full.names = FALSE)
  if(fileName %in% registered_students){
   code_filtered <- filtered_df$code[1]
   file <- read.csv(file = paste0(registered_unitsDir,"/",fileName))
   row_to_modify <- which(file$Code %in% code_filtered)
   update <- ifelse(grade == "E", "FAILED", "PASSED")
   file[row_to_modify, "Status"] <- update
   #student details
   student_data <- registrationData() |> filter(Reg %in% file$Reg[1])
   student_year <- student_data$Year 
   student_course <- student_data$Code
   #student_units
   student_units <- units |>
    filter(course %in% c("BOTH",student_course)) |>
    filter(year %in% student_year)
    list1 <- student_units$code
    list2 <- file$Code
   if(all(file$Status == "PASSED") & all(list1 %in% list2)){
    promote_button <- student_data$Buttons
    new <- gsub("color: red;", 
                "color: green;", promote_button)
    student_data$Buttons <- new
    fileNames <- student_data$Filename
    fileName1 <- gsub("registered_students/", "", fileNames)
    data <- subset(student_data, select = -Filename)
    write.csv(
     x =  data,
     file = file.path(registeredDir, fileName1),
     row.names = FALSE, quote = TRUE
    )
   }
    write.csv(
    x = file,
    file = file.path(registered_unitsDir, fileName),
    row.names = FALSE, quote = TRUE
   )
    # Render the registration data table
    output$registrationTable <- renderDataTable(
     registrationData(),server = TRUE, escape = FALSE, selection = "none",
     options = list(
      columnDefs = list(
       list(targets = c(5,6,10), visible = FALSE),# column you want to hide
       list(targets = c(1,2,5,6,7,8), orderable = FALSE)#Disable sorting
      )
     )
    )
  }
 })
 #edit a response file
 observeEvent(input$confirm_edit,{
  shinyjqui::jqui_show("#exit_button", effect = "fade")
  showToast("info", "Edit mode!", keepVisible = TRUE,
            .options = myToastOptions )
  
  updateTextInput(
   session = session,
   inputId = "id",
   value = 1
  )
  enable("score")
  enable("submit")
  selectedRow <- as.numeric(strsplit(input$edit_button, "_")[[1]][2])
  search_string <- paste0("edit_ ",selectedRow)
  filtered_df <- loadData()[grepl(search_string,
                                  loadData()$actions), ]
  #update fields
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   selected =  filtered_df$reg,
   choices = filtered_df$reg
  )
  updateSelectizeInput(
   session = session,
   inputId = "code",
   choices = filtered_df$code,
   selected =  filtered_df$code
  )
  updateNumericInput(
   session = session,
   inputId = "score",
   value =  filtered_df$score
  )
  removeModal()
 })
 #promote a student
 observeEvent(input$confirm_promote, {
  selectedRow <- as.numeric(strsplit(input$year_button, "_")[[1]][2])
  search_string <- paste0("year_ ",selectedRow)
  filtered_df <- registrationData()[grepl(search_string,
                                          registrationData()$Buttons), ]
  current_year <- as.numeric(filtered_df$Year)
  #check if all unit passed
  reg_no <- filtered_df$Reg
  cleaned_reg <- gsub("/", "", reg_no)
  fileName <- paste0(cleaned_reg,".csv")
  registered_students <- list.files(registered_unitsDir, full.names = FALSE)
  if(fileName %in% registered_students){
  file <- read.csv(file = paste0(registered_unitsDir,"/",fileName))
  #check if all units are available from timetable
  student_course <- filtered_df$Code
  student_units <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% current_year)
  list1 <- student_units$code
  list2 <- file$Code
  list <- setdiff(list1, list2) |> as.list()
  if(current_year < 4 & all(file$Status == "PASSED") & length(list)== 0) {
   new_year <- current_year + 1
   filtered_df$Year <- new_year
   originalFile <- filtered_df$Filename 
   promote_button <- filtered_df$Buttons
   new <- gsub("color: green;", 
               "color: red;", promote_button)
   filtered_df$Buttons <- new
   # Source directory and file name
   fileName <- gsub("registered_students/", "", originalFile)
   file_name <- fileName 
   # Write the file to the local system
   data <- subset(filtered_df, select = -Filename)
   write.csv(x = data,
    file = file.path(registeredDir, file_name),
    row.names = FALSE, quote = TRUE
   )
   # Render the registration data table
   output$registrationTable <- renderDataTable(
    registrationData(),server = TRUE, escape = FALSE, selection = "none",
    options = list(
     columnDefs = list(
      list(targets = c(5,6,10), visible = FALSE),# column you want to hide
      list(targets = c(1,2,5,6,7,8), orderable = FALSE)#Disable sorting
     )
    )
   )
  showToast(
   "success", "Student promoted!",
   .options = myToastOptions
  )
  removeModal()
  }else if(current_year == 4 & all(file$Status == "PASSED")& length(list)== 0){
   showToast(
    "error", "Horray! Waiting Graduation!",
    .options = myToastOptions
   )
   removeModal()
  }else if(current_year < 4 & all(file$Status != "PASSED") | length(list)!= 0){
   showToast("error",
             "Student has not passed this Year Units!",
             .options = myToastOptions)
   removeModal()
  }else{
   showToast("error",
             "Student has not passed this Year Units!",
             .options = myToastOptions)
   removeModal()
  }
  }else{
   showToast("error",
             "Student has no registered Units!",
             .options = myToastOptions)
   removeModal()
  }
 })
 #delete a response file
 observeEvent(input$confirm_delete, {
  selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
  search_string <- paste0("delete_ ",selectedRow)
  filtered_df <- loadData()[grepl(search_string,
                                  loadData()$actions), ]
  filename_delete <- filtered_df$filename
  # Source directory and file name
  source_dir <- "released_results"
  fileName <- gsub("released_results/", "", filename_delete)
  file_name <- fileName 
  # Destination directory
  dest_dir <- "deleted_results"
  # Create the full paths
  source_path <- file.path(source_dir, file_name)
  dest_path <- file.path(dest_dir, file_name)
  # Move the file
  file.rename(source_path, dest_path)
  #render new data table
  output$marks <- DT::renderDataTable(
   loadData(), server = TRUE, escape = FALSE, selection = "none",
   options = list(
    columnDefs = list(
     list(targets = c(10), visible = FALSE),# column you want to hide
     list(targets = c(1,2,3,4,5,6,7,8,9), orderable = FALSE)#Disable sorting
    )
   )
  )
  showToast("success",
            "Mark Deleted!",
            .options = myToastOptions )
  removeModal()
 })
 #STUDENT TAB
 registered_unitsDir <- "registered_units"
 reg_units <- function(){
  reg_no <- input$student_reg
  req(!reg_no %in% "")
  #student details
  student_data <- registrationData() |> filter(Reg %in% input$student_reg)
  student_year <- student_data$Year 
  cleaned_reg <- gsub("/", "", reg_no)
  fileName <- paste0(cleaned_reg,".csv")
  registered_students <- list.files(registered_unitsDir, full.names = FALSE)
  if(fileName %in% registered_students){
   file <- read.csv(file = paste0(registered_unitsDir,"/",fileName))
   file <- file |> filter(Year == student_year)|> select(-c(Date,Reg,Year)) |> arrange(Code)
   file
  }else{
   return()
  }
 }
 observeEvent(input$student_reg, {
  shinyjs::hide("download") 
 #student details
  student_data <- registrationData() |> filter(Reg %in% input$student_reg)
  student_year <- student_data$Year 
  student_course <- student_data$Code 
  #student name
  output$student_name <- renderText({
   paste(student_data$Reg, student_data$Name)
  })
  #student_course
  output$student_course <- renderText({
   paste(student_data$Code, student_data$Course,"Year",student_year)
  })
  #student_units
  student_units <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% student_year)
  list1 <- as.list(student_units$code)
  #already registered units
  list2 <- as.list(reg_units()$Code)
  #Remaining units
  list <- setdiff(list1, list2)
  #list update
  if(length(list) != 0){
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = list,
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3)
   )
  }else{
   showToast("info", "All Units registered!",
             .options = myToastOptions )
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = NULL,
    server = TRUE
   )
  }
  #student timetable
  datetime <- c(
   "Mon 08:00", "Tue 08:00", "Wed 08:00", "Thu 08:00", "Fri 08:00",
   "Mon 11:00", "Tue 11:00", "Wed 11:00", "Thu 11:00", "Fri 11:00",
   "Mon 14:00", "Tue 14:00", "Wed 14:00", "Thu 14:00", "Fri 14:00"
  )
  sample1 <- sample(datetime, size = 15, replace = TRUE)
  sample2 <- sample(users, size = 5, replace = TRUE)
  num_rows <- nrow(student_units)
  output$timetable <- DT::renderDataTable({
   student_units_table <- student_units |> 
    select(code, title) |>
    mutate(lecturer = rep(sample2, length.out = num_rows)) |>
    mutate(time = rep(sample1, length.out = num_rows))
   datatable(student_units_table, escape = FALSE, selection = "none",
             options = list(
              searching = FALSE,         # Hide search box
              paging = FALSE,            # Hide pagination
              ordering = FALSE,          # Disable ordering in all columns
              lengthMenu = list(FALSE),  # Hide entries selection
              language = list(
               info = ""  # Hide the information about entries
              )
             ) 
   )
  })
  #approved marks
  t_b_b <- loadData()[grepl("green", loadData()$actions), ]
  t_b <- t_b_b |> filter(reg %in% input$student_reg) |>
   select(code, course, grade) |>
   arrange(code)
  #student_tab marks
  output$student_marks <- DT::renderDataTable(
   datatable(t_b, escape = FALSE, selection = "none",
             options = list(
              searching = FALSE,         # Hide search box
              paging = FALSE,            # Hide pagination
              ordering = FALSE,          # Disable ordering in all columns
              lengthMenu = list(FALSE),  # Hide entries selection
              language = list(
               info = ""  # Hide the information about entries
              )
             ) 
   )
  )

  #first year
  chosen_units1 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 1)
  t_b_1 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units1$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes1 <- t_b_1$code
  cbind_1 <- chosen_units1 |> filter(code %in% rel_codes1)
  finale1 <- cbind(cbind_1, t_b_1) 
  n_total1 <-  nrow(finale1)
  total1 <- sum(finale1$score)
  average1 <- round(total1/n_total1,2)
  #second year
  chosen_units2 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 2)
  t_b_2 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units2$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes2 <- t_b_2$code
  cbind_2 <- chosen_units2 |> filter(code %in% rel_codes2)
  finale2 <- cbind(cbind_2, t_b_2) 
  n_total2 <-  nrow(finale2)
  total2 <- sum(finale2$score)
  average2 <- round(total2/n_total2,2)
  #third year
  chosen_units3 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 3)
  t_b_3 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units3$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes3 <- t_b_3$code
  cbind_3 <- chosen_units3 |> filter(code %in% rel_codes3)
  finale3 <- cbind(cbind_3, t_b_3) 
  n_total3 <-  nrow(finale3)
  total3 <- sum(finale3$score)
  average3 <- round(total3/n_total3,2)
  #fourth year
  chosen_units4 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 4)
  t_b_4 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units4$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes4 <- t_b_4$code
  cbind_4 <- chosen_units4 |> filter(code %in% rel_codes4)
  finale4 <- cbind(cbind_4, t_b_4) 
  n_total4 <-  nrow(finale4)
  total4 <- sum(finale4$score)
  average4 <- round(total4/n_total4,2)
  #final average
  student_f_units <- units |>
   filter(course %in% c("BOTH",student_course))
  t_b_f <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(rel_codes1,rel_codes2,
                                        rel_codes3,rel_codes4)) |>
   select(reg, code, course,grade, score)
  n_total <- nrow(t_b_f)
  total <- sum(t_b_f$score)
  average <- round(total/n_total,2)
  #To find final averages
  if(student_year == 1){
   #finale data
   data <- data.frame(
    Year = "First",
    Units = n_total1,
    Average = average1
   )
  }else if(student_year == 2){
   #finale data
   data <- data.frame(
    Year = c("First","Second"),
    Units = c(n_total1,n_total2),
    Average = c(average1,average2)
   )
  }else if(student_year == 3){
   #finale data
   data <- data.frame(
    Year = c("First","Second","Third"),
    Units = c(n_total1,n_total2,n_total3),
    Average = c(average1,average2,average3)
   )
  }else{
   #finale data
   data <- data.frame(
    Year = c("First","Second","Third","Fourth"),
    Units = c(n_total1,n_total2,n_total3,n_total4),
    Average = c(average1,average2,average3,average4)
   )
  }
  #transcript tables
  table_data1 <- t_b_1 |> select(code, course, grade) |> arrange(code) |>
   add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average1))
  table_data2 <- t_b_2 |> select(code, course, grade) |> arrange(code) |>
   add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average2))
  table_data3 <- t_b_3 |> select(code, course, grade) |> arrange(code) |> 
   add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average3))
  table_data4 <- t_b_4 |> select(code, course, grade) |> arrange(code) |>    add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average4))
  #allow transcripts download
  if(nrow(t_b) > 0){
   enable("transcripts")
  }else{
   disable("transcripts")
  }
  #comment
  a <- average
  class <- case_when(
   a >= 70 ~ "FIRST CLASS HONOURS",
   a >= 60 ~ "SECOND CLASS-UPPER",
   a >= 50 ~ "SECOND CLASS-LOWER",
   a >= 40 ~ "PASS",
   a >= 1 ~ "NONE",
   TRUE ~ ""
  )
  #saving file
  fileName <- gsub("/", "", input$student_reg)
  fileName <- paste0(fileName,".csv")
  registered_regs <- list.files(registered_unitsDir, full.names = FALSE)
  #availability conditions
  if(fileName %in% registered_regs){
  file <- read.csv(file = paste0(registered_unitsDir,"/",fileName))
  #missing units
  r_units <- as.list(t_b_f$code)
  m_units <- setdiff(list1, r_units)
  #failed units
  file_2 <- file |>
   filter(Status %in% "FAILED")
  list_2 <- as.list(file_2$Code)
  #print feedback
  if(length(m_units) > 0  & length(list_2) > 0){
   comment <- paste("FAIL: Missing units - ", 
                    paste(unlist(m_units), collapse = ", "),";",
                    "Failed units - ",paste(unlist(list_2), collapse = ", "))
  }else if(length(m_units)> 0){
   comment <- paste("FAIL: Missing units - ",
                    paste(unlist(m_units), collapse = ", "))
  }else if(length(list_2)> 0){
   comment <- paste("FAIL: Failed units - ",
                    paste(unlist(list_2), collapse = ", "))
  }else if(length(m_units)== 0  & length(list_2) == 0 & student_year == 4){
   comment <- paste("PASS: Final class is",class)
  }else{
   comment <- paste("PASS: Promoted to the next Year")
  }
  #data table output
  output$year_averages <- DT::renderDataTable({
   datatable(data, escape = FALSE, selection = "none",
             options = list(
              searching = FALSE,         # Hide search box
              paging = FALSE,            # Hide pagination
              ordering = FALSE,          # Disable ordering in all columns
              lengthMenu = list(FALSE),  # Hide entries selection
              language = list(
               info = ""  # Hide the information about entries
              )
             ) 
   )
  })
  #output class
  output$clock <- renderEcharts4r({
   e_charts() |> 
    e_gauge(average, class) |> 
    e_animation(duration = 4000)|>
    e_title('% SCORE',left='center')
  })
  #graph 
  output$graph <- renderEcharts4r({
   data|> 
    e_charts(Year) |>
    e_bar(Average,
          bind = Year,
          emphasis = list(
           focus = "item"),
          itemStyle = list(
           color = htmlwidgets::JS("
          function(params) {
          var colorList = ['#00ff00','#00ff00','#00ff00','#00ff00','#ff0000'];
          return colorList[params.dataIndex]
          } 
          "),
           shadowBlur = 0.5,
           shadowColor = "#ced4da",
           shadowOffsetX = 0.5)) |>
    e_animation(duration = 4000)|>
    e_axis_labels(x = "Year",y = "% Score")|> 
    e_tooltip(backgroundColor = "#e9ecef") |>
    e_toolbox_feature(feature = "saveAsImage") |>
    e_legend(show = FALSE) |>
    e_datazoom(type = "inside") |>
    e_grid(show = TRUE)|>
    e_title(text = student_data$Reg,
            subtext = student_data$Course,
            left = "center", top = 1,
            sublink = "https://github.com/ndeke254",
            textStyle = list(fontWeight = "normal"))|>
    e_x_axis(splitLine=list(
     lineStyle = list(
      type = "dashed"))) |>
    e_y_axis(scale = TRUE,
             splitLine = list(
              lineStyle = list(
               type = "dashed"
              )
             )
    ) |> 
    e_image_g(
     right = 120,
     top = 90,
     z = -999,
     style = list(
      image = "logo.png",
      width = 200,
      height = 200,
      opacity = .1
     )
    )
  }) 
  }else{
   output$year_averages <- DT::renderDataTable({
   data.frame()
   })
   output$clock <- renderEcharts4r({
    e_charts()
   })
   output$graph <- renderEcharts4r({
    e_charts() |> 
     e_title("No Data Available", left = "center")
   })
  }
 })
 #prepare and download transcripts
 #prepare the transcripts
 observeEvent(input$transcripts,{
  #student details
  student_data <- registrationData() |> filter(Reg %in% input$student_reg)
  student_year <- student_data$Year 
  student_course <- student_data$Code 
  #approved marks
  t_b_b <- loadData()[grepl("green", loadData()$actions), ]
  t_b <- t_b_b |> filter(reg %in% input$student_reg) |>
   select(code, course, grade) |>
   arrange(code)
  #first year
  chosen_units1 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 1)
  t_b_1 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units1$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes1 <- t_b_1$code
  cbind_1 <- chosen_units1 |> filter(code %in% rel_codes1)
  finale1 <- cbind(cbind_1, t_b_1) 
  n_total1 <-  nrow(finale1)
  total1 <- sum(finale1$score)
  average1 <- round(total1/n_total1,2)
  #second year
  chosen_units2 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 2)
  t_b_2 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units2$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes2 <- t_b_2$code
  cbind_2 <- chosen_units2 |> filter(code %in% rel_codes2)
  finale2 <- cbind(cbind_2, t_b_2) 
  n_total2 <-  nrow(finale2)
  total2 <- sum(finale2$score)
  average2 <- round(total2/n_total2,2)
  #third year
  chosen_units3 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 3)
  t_b_3 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units3$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes3 <- t_b_3$code
  cbind_3 <- chosen_units3 |> filter(code %in% rel_codes3)
  finale3 <- cbind(cbind_3, t_b_3) 
  n_total3 <-  nrow(finale3)
  total3 <- sum(finale3$score)
  average3 <- round(total3/n_total3,2)
  #fourth year
  chosen_units4 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 4)
  t_b_4 <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(chosen_units4$code)) |>
   select(reg, code, course,grade, score)
  #combine exact rows
  rel_codes4 <- t_b_4$code
  cbind_4 <- chosen_units4 |> filter(code %in% rel_codes4)
  finale4 <- cbind(cbind_4, t_b_4) 
  n_total4 <-  nrow(finale4)
  total4 <- sum(finale4$score)
  average4 <- round(total4/n_total4,2)
  #final average
  student_f_units <- units |>
   filter(course %in% c("BOTH",student_course))
  t_b_f <- t_b_b |> filter(reg == input$student_reg &
                            code %in% c(rel_codes1,rel_codes2,
                                        rel_codes3,rel_codes4)) |>
   select(reg, code, course,grade, score)
  n_total <- nrow(t_b_f)
  total <- sum(t_b_f$score)
  average <- round(total/n_total,2)
  #To find final averages
  if(student_year == 1){
   #finale data
   data <- data.frame(
    Year = "First",
    Units = n_total1,
    Average = average1
   )
  }else if(student_year == 2){
   #finale data
   data <- data.frame(
    Year = c("First","Second"),
    Units = c(n_total1,n_total2),
    Average = c(average1,average2)
   )
  }else if(student_year == 3){
   #finale data
   data <- data.frame(
    Year = c("First","Second","Third"),
    Units = c(n_total1,n_total2,n_total3),
    Average = c(average1,average2,average3)
   )
  }else{
   #finale data
   data <- data.frame(
    Year = c("First","Second","Third","Fourth"),
    Units = c(n_total1,n_total2,n_total3,n_total4),
    Average = c(average1,average2,average3,average4)
   )
  }
  #transcript tables
  table_data1 <- t_b_1 |> select(code, course, grade) |> arrange(code) |>
   add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average1))
  table_data2 <- t_b_2 |> select(code, course, grade) |> arrange(code) |>
   add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average2))
  table_data3 <- t_b_3 |> select(code, course, grade) |> arrange(code) |> 
   add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average3))
  table_data4 <- t_b_4 |> select(code, course, grade) |> arrange(code) |>    add_row(code = "", course = "AVERAGE SCORE", grade = as.character(average4))
  #parameters
  r_name <- student_data$Name
  reg_no <- input$student_reg
  prog <- student_data$Course
  date <- student_data$Date
  id <- student_data$ID
  stamp_datee <- format(Sys.Date(),  format = "%d %b %y")
  comment <- comment
  #comment
  a <- average
  class <- case_when(
   a >= 70 ~ "FIRST CLASS HONOURS",
   a >= 60 ~ "SECOND CLASS-UPPER",
   a >= 50 ~ "SECOND CLASS-LOWER",
   a >= 40 ~ "PASS",
   a >= 1 ~ "NONE",
   TRUE ~ ""
  )
  #saving file
  reg_No <- gsub("/", "", input$student_reg)
  #student_units
  student_units <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% student_year)
  list1 <- as.list(student_units$code)
  #missing units
  r_units <- as.list(t_b_f$code)
  m_units <- setdiff(list1, r_units)
  #failed units
  fileName <- gsub("/", "", input$student_reg)
  fileName <- paste0(fileName,".csv")
  registered_regs <- list.files(registered_unitsDir, full.names = FALSE)
  file <- read.csv(file = paste0(registered_unitsDir,"/",fileName))
  file_2 <- file |>
   filter(Status %in% "FAILED")
  list_2 <- as.list(file_2$Code)
  #print feedback
  if(length(m_units) > 0  & length(list_2) > 0){
   comment <- paste("FAIL: Missing units - ", 
                    paste(unlist(m_units), collapse = ", "),";",
                    "Failed units - ",paste(unlist(list_2), collapse = ", "))
  }else if(length(m_units)> 0){
   comment <- paste("FAIL: Missing units - ",
                    paste(unlist(m_units), collapse = ", "))
  }else if(length(list_2)> 0){
   comment <- paste("FAIL: Failed units - ",
                    paste(unlist(list_2), collapse = ", "))
  }else if(length(m_units)== 0  & length(list_2) == 0 & student_year == 4){
   comment <- paste("PASS: Final class is",class)
  }else{
   comment <- paste("PASS: Promoted to the next Year")
  }
  #prepare templates
  if(student_year == 1){
   # Render the R Markdown template with the data
   render_result <- rmarkdown::render("transcript1.Rmd",
                                      output_file = paste(reg_No,
                                                          "_transcript.html"),
                                      params = list(data1 = table_data1,
                                                    name = r_name,
                                                    reg = reg_no,
                                                    course = prog,
                                                    date = date,
                                                    id = id,
                                                    ddate =  stamp_datee,
                                                    comment = comment
                                      )
   )
  }else if(student_year == 2){
   # Render the R Markdown template with the data
   render_result <- rmarkdown::render("transcript2.Rmd",
                                      output_file = paste(reg_No,
                                                          "_transcript.html"),
                                      params = list(data1 = table_data1,
                                                    data2 = table_data2,
                                                    name = r_name,
                                                    reg = reg_no,
                                                    course = prog,
                                                    date = date,
                                                    id = id,
                                                    ddate =  stamp_datee,
                                                    comment = comment
                                      )
   )
  }else if(student_year == 3){
   # Render the R Markdown template with the data
   render_result <- rmarkdown::render("transcript3.Rmd",
                                      output_file = paste(reg_No,
                                                          "_transcript.html"),
                                      params = list(data1 = table_data1,
                                                    data2 = table_data2,
                                                    data3 = table_data3,
                                                    name = r_name,
                                                    reg = reg_no,
                                                    course = prog,
                                                    date = date,
                                                    id = id,
                                                    ddate =  stamp_datee,
                                                    comment = comment
                                      )
   )
  }else if(student_year == 4){
   # Render the R Markdown template with the data
   render_result <- rmarkdown::render("transcript4.Rmd",
                                      output_file = paste(reg_No,
                                                          "_transcript.html"),
                                      params = list(data1 = table_data1,
                                                    data2 = table_data2,
                                                    data3 = table_data3,
                                                    data4 = table_data4,
                                                    name = r_name,
                                                    reg = reg_no,
                                                    course = prog,
                                                    date = date,
                                                    id = id,
                                                    ddate =  stamp_datee,
                                                    comment = comment
                                      )
   )
  }else{
   return
  }
  #convert HTML to a pdf file
  pdf <- chrome_print(input = render_result, output = paste(reg_no,"_transcript.pdf"))
  #PDF ready
  resetLoadingButton("transcripts")
  showToast("info", "Transcripts ready!",
            .options = myToastOptions )
  # Show the download link
  shinyjs::show("download") 
  #once download is clicked
  output$download <- downloadHandler(
   filename =  paste(reg_no,"_transcript.pdf"),
   content = function(file) {
    file.copy(pdf, file)
   }
  )
 })
 #edit a student registration file
 observeEvent(input$confirm_editreg,{
  shinyjqui::jqui_show("#reset_button", effect = "fade")
  showToast("info", "Edit mode!", keepVisible = TRUE,
            .options = myToastOptions )
  updateTextInput( session = session, inputId = "editmode", value = 1)
  reset("photoInput")
  updateTextInput( session = session, inputId = "toggle", value = 0)
  selectedRow <- as.numeric(strsplit(input$editreg_button, "_")[[1]][2])
  search_string <- paste0("editreg_ ",selectedRow)
  filtered_df <- registrationData()[grepl(search_string,
                                          registrationData()$Buttons), ]
  #update fields
  updateTextInput(session = session, inputId = "Name",
                  value = filtered_df$Name
                  )
  updateSelectizeInput(session = session, inputId = "Gender",
                        selected =  filtered_df$Gender
                        )
  updateTextInput(session = session, inputId = "ID",
                  value = filtered_df$ID
                  )
  updateTextInput(session = session, inputId = "Date",
                  value = filtered_df$Date
                  )
  updateSelectizeInput(session = session, inputId = "Code",
                       selected =  filtered_df$Code
                       )
  updateTextInput(session = session, inputId = "Reg",
                  value = filtered_df$Reg
                  )
  removeModal()
 })
 observeEvent(input$dismiss, {
  removeModal()
 })
 observeEvent(input$approve_button, {
  modal_dialog(approve = TRUE, edit = FALSE, editreg = FALSE,
               delete = FALSE, promote = FALSE, deletereg = FALSE)
 })
 observeEvent(input$delete_button, {
  modal_dialog(approve = FALSE, edit = FALSE, delete = TRUE, 
               editreg = FALSE, promote = FALSE, deletereg = FALSE)
 })
 observeEvent(input$edit_button, {
  modal_dialog(approve = FALSE, edit = TRUE, delete = FALSE,
               editreg = FALSE, promote = FALSE, deletereg = FALSE)
 })
 observeEvent(input$editreg_button, {
  modal_dialog(approve = FALSE, edit = FALSE, delete = FALSE,
               editreg = TRUE, promote = FALSE, deletereg = FALSE)
 })
 observeEvent(input$deletereg_button, {
  modal_dialog(approve = FALSE, edit = FALSE, delete = FALSE, 
               editreg = FALSE, promote = FALSE, deletereg = TRUE)
 })
 observeEvent(input$year_button, {
  modal_dialog(approve = FALSE, edit = FALSE, delete = FALSE,
               editreg = FALSE, promote = TRUE, deletereg = FALSE)
 })
 observeEvent(input$reset,{
  # Clear the input fields
  updateTextInput(session, "Name", value = "")
  updateNumericInput(session, "ID", value = "")
  updateSelectInput(session, "Gender", selected = NULL)
  updateTextInput(session, "toggle", value = "0")
  reset("Date")
  reset("photoInput")
  new_reg()
  updateTextInput(session, "editmode", value = 0)
  hideToast(animate = TRUE, session = session)
  shinyjqui::jqui_hide("#reset_button", effect = "fade")
 })
 observeEvent(input$exit,{
  #reset all fields to blank
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   choices = registrationData()$Reg,
   server = TRUE,
   selected = "",
   options = list(maxOptions = 3)
  )
  updateSelectizeInput(
   session = session,
   inputId = "code",
   choices = NULL,
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
  updateNumericInput(
   session = session,
   inputId = "score",
   value = ""
  )
  updateTextInput(
   session = session,
   inputId = "id",
   value = 0
  )
  shinyjqui::jqui_hide("#exit_button", effect = "fade")
  hideToast(animate = TRUE, session = session)
 })
 
 #output registered units
 output$registered_units <- DT::renderDataTable({
  datatable(reg_units(), escape = FALSE, selection = "none",
            options = list(
             searching = FALSE,         # Hide search box
             paging = FALSE,            # Hide pagination
             ordering = FALSE,          # Disable ordering in all columns
             lengthMenu = list(FALSE),  # Hide entries selection
             language = list(
              info = ""  # Hide the information about entries
             )
            )
  )
 })
 #update on button click
 observeEvent(input$register,{
  req(input$register_unit)
  req(input$register_code)
  reg_no <- input$student_reg
  #student details
  student_data <- registrationData() |> filter(Reg %in% reg_no)
  student_year <- student_data$Year 
  cleaned_reg <- gsub("/", "", reg_no)
  fileName <- paste0(cleaned_reg,".csv")
  registered_students <- list.files(registered_unitsDir, full.names = FALSE)
  if(fileName %in% registered_students){
   file <- read.csv(file = paste0(registered_unitsDir,"/",fileName))
   new <- data.frame(
    Reg = input$student_reg,
    Code = input$register_code,
    Unit = input$register_unit,
    Date = format(Sys.time(), "%d-%m-%Y %H:%M"),
    Status = "REGISTERED",
    Year = student_year
   )
   combined <- rbind.data.frame(file,new)
   # Write the file to the local system
   write.csv(
    x = combined,
    file = file.path(registered_unitsDir, fileName),
    row.names = FALSE, quote = TRUE
   )
  }else{
  student_units <- data.frame(
    Reg = input$student_reg,
    Code = input$register_code,
    Unit = input$register_unit,
    Date = format(Sys.time(), "%d-%m-%Y %H:%M"),
    Status = "REGISTERED",
    Year = 1
   )
  # Write the file to the local system
  write.csv(
   x = student_units,
   file = file.path(registered_unitsDir, fileName),
   row.names = FALSE, quote = TRUE
  )
  }
  #output registered units
  output$registered_units <- DT::renderDataTable({
   datatable(reg_units(), escape = FALSE, selection = "none",
             options = list(
              searching = FALSE,         # Hide search box
              paging = FALSE,            # Hide pagination
              ordering = FALSE,          # Disable ordering in all columns
              lengthMenu = list(FALSE),  # Hide entries selection
              language = list(
               info = ""  # Hide the information about entries
              )
             )
   )
  })
  #student_units
  student_data <- registrationData() |> filter(Reg %in% input$student_reg)
  student_year <- student_data$Year 
  student_course <- student_data$Code
  student_units <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% student_year)
  list1 <- as.list(student_units$code)
  list2 <- as.list(reg_units()$Code)
  list <- setdiff(list1, list2)
  if(length(list) != 0){
  updateSelectizeInput(
   session = session,
   inputId = "register_code",
   choices = list,
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
  }else{
   showToast("info", "All Units registered!",
             .options = myToastOptions )
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = NULL,
    server = TRUE
    )
  }
 })

 }
shinyApp(ui, server)
