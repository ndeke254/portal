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
#import data
units <- read_csv("data/units.csv",show_col_types = FALSE) # nolint
names <- read_csv( "data/names.csv", show_col_types = FALSE) # nolint
# Define the fields we want to save from the form
fields <- c("reg", "name", "code", "course",
            "score", "grade", "time", "lecturer","actions")
users <- c("DR.JOHN", "PROF.MAKORE", "MR.WAMAE", "MS.MUTUA", "DR.OCHIENG")# nolint
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
 windowTitle = "University of Marseille", # nolint
 title = tags$img(src = "logo.png"), # nolint
 theme = bslib::bs_theme(4),
 tabPanel(
  title = "Lecturer",
  value = "lecturer",
  icon = icon("person-chalkboard"),
  useShinyjs(),
  useShinyFeedback(),
  includeCSS("www/styles.css"),
  splitLayout(
   cellWidths = c("10.286%","14.286%","5.286%", # nolint
                  "24.286%","5.286%","4.286%","14.286%"), # nolint
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
    hidden(textInput("lecturer", "Lecturer"))
   ),
   textInput("actions", "Actions"),
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
  title = "Administrator", # nolint
  value = "admin",
  icon = icon("table-cells"),  
  div(class = "input-line",
      div(class = "form-group",
          textInput("nameInput", 
                    "Name", 
                    width = "225px")
      ),
      div(class = "form-group",
          selectInput("genderInput", "Gender", 
                      choices = c("Male", "Female"),
                      width = "100px",
                      selected = NULL)
      ),
      div(class = "form-group id-field",
          numericInput("IdInput", "ID", 
                       value = "", 
                       width = "110px")
      ),
      disabled(
       div(class = "form-group",
           textInput("dateInput", 
                     "Date", 
                     value = format(Sys.time(), "%d-%m-%Y"),
                     width = "120px")
       )
      ),
      div(class = "form-group",
          selectInput("codeInput",
                      "Code",
                      choices = c("X74", "X75"),
                      width = "55px",
                      selected = NULL)
      ),
      disabled(
       div(class = "form-group",
           textInput("regInput",
                     "Registration Number", 
                     value = "", 
                     width = "150px")
       )
      ),
      div(class = "form-group",
          fileInput("photoInput",
                    "Select Photo", 
                    accept = c("image/jpeg", "image/png")
          ),
          hidden(
           textInput('toggle',label = "",value = "0")
          )
      ),
      div(
       style = "padding-top: 25px;",
       loadingButton("registerButton", "Register",
                     style = "width: 110px;margin-top: -34px;",
                     loadingLabel = "Registering",
                     loadingSpinner = "cog")
      )
  ),
  tags$hr(),
  DT::dataTableOutput("registrationTable"),
 ),
 tabPanel(
  title = "Student",
  value = "student",
  icon = icon("children"),
  textOutput("student_name"),
  tags$hr(),
  DT::dataTableOutput("student_marks")
 )
)
server <- function(input, output, session){
 #ADMINISTRTORS
 #create admin icons
 shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
   inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
 }
 # Create reactive data frame for registration data
 registrationData <- function(){
  data <- read.csv("data/names.csv", stringsAsFactors = FALSE)
  data <- data |> arrange(desc(date))
  data
 }
 # Create a vector to store used numbers
 usedNumbers <- function() {
  # Extract numbers between slashes
  numbers <- sub(".*/(\\d+)/.*", "\\1", registrationData()$reg)
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
   code <- isolate(input$codeInput)
   date <-Sys.time()
   year <- format(date, "%Y")
   # Generate a unique 4-digit number
   num <- sample(setdiff(1000:9999, usedNumbers()), 1)
   # Update the generatedNumber reactive value
   generatedNumber(num)
   
   reg <- paste0(code, "/", num, "/", year)
   updateTextInput(session, "regInput", value = reg)
  }
 }
 
 observeEvent(input$codeInput, {
  
  if (!is.null(code) && !is.null(date)) {
   new_reg()
  }
 })
 
 # Event handler for register button click
 observeEvent(input$registerButton, {
  # Check if the ID input has 8 characters
  if (nchar(input$IdInput) != 8 || is.na(input$IdInput)) {
   showToast("error",
             "ID should be 8 characters!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()
  }
  # Check if the name input is empty
  if (is.null(input$nameInput) || input$nameInput == "") {
   showToast("info",
             "Enter Name!",
             .options = myToastOptions ) 
   resetLoadingButton("registerButton")
   return()
  }
  # Check if the reg input is empty
  if (is.null(input$regInput) || input$regInput == "") {
   showToast("infp",
             "Refresh RegInput!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()
  }
  # Retrieve input values
  name <- input$nameInput
  id <- input$IdInput
  reg <- input$regInput
  gender <- input$genderInput
  photo <- input$photoInput
  code <- input$codeInput
  # Check if a photo is uploaded
  if (is.null(photo) || is.na(photo$datapath) || input$toggle == "0"){
   showToast("error",
             "Please appload Photo!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()
  }
  
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
  # Get the file extension of the uploaded photo
  extension <- tools::file_ext(photo$name) 
  
  # Generate a unique filename using the regInput value and file extension
  filename <- paste0(generatedNumber(), ".", extension)
  # Move the uploaded photo to the desired directory and rename it
  file.rename(photo$datapath, paste0("data/images/",filename))
  # Append the data to the registration data frame
  added_row <- data.frame(
   name = name,
   gender = gender,
   ID = id,
   reg = reg,
   falculty = "Falculty of Arts and Social Sciences",	
   department = "Economics and Development Studies",
   course = course(),
   code = code,
   photo = filename,
   date = format(Sys.time(), "%d-%m-%Y %H:%M"),
   Action = paste0(
    shinyInput(
     actionButton, 1, id = "editreg_",
     label = icon("file-pen"),
     style = "color: red;",
     onclick = paste0('Shiny.onInputChange( \"editreg_button\" , this.id, {priority: \"event\"})')
    ),
    shinyInput(
     actionButton, 1, id = "deletereg_",
     label = icon("trash"),
     onclick = paste0('Shiny.onInputChange( \"deletereg_button\" , this.id, {priority: \"event\"})')
    )
   ),
   stringsAsFactors = FALSE
  )
  names <- rbind(registrationData(), added_row)
  write.csv(names,"data/names.csv",row.names = FALSE, quote = TRUE)
  # Clear the input fields
  updateTextInput(session, "nameInput", value = "")
  updateNumericInput(session, "IdInput", value = "")
  updateSelectInput(session, "genderInput", selected = NULL)
  updateTextInput(session, "toggle", value = "0")
  new_reg()
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   choices = registrationData()$reg,
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
  # Render the registration data table
  output$registrationTable <- renderDataTable(
   registrationData(),server = TRUE, escape = FALSE, selection = "none",
   options = list(
    columnDefs = list(
     list(targets = c(5,6,7,8,9), visible = FALSE),# column you want to hide
     list(targets = c(1,2,5,6,7,8,9), orderable = FALSE)#Disable sorting
    )
   )
  )
  showToast("success",
            "New student Registered!",
            .options = myToastOptions )
  resetLoadingButton("registerButton")
  reset("photoInput")
 })
 #reset toggle field
 observeEvent(input$photoInput, {
  updateTextInput(session, "toggle", value = "1")
 })
 # Render the registration data table
 output$registrationTable <- renderDataTable(
  registrationData(),server = TRUE, escape = FALSE, selection = "none",
  options = list(
   columnDefs = list(
    list(targets = c(5,6,7,8,9), visible = FALSE),# column you want to hide
    list(targets = c(1,2,5,6,7,8,9), orderable = FALSE)#Disable sorting
   )
  )
 )
 #delete a response file
 observeEvent(input$deletereg_button, {
  selectedRow <- as.numeric(strsplit(input$deletereg_button, "_")[[1]][2])
  Data <- registrationData()[-selectedRow,]
  # Source directory and file name
  write.csv(Data,"data/names.csv",row.names = FALSE, quote = TRUE)
  #render new data table
  # Render the registration data table
  output$registrationTable <- renderDataTable(
   registrationData(),server = TRUE, escape = FALSE, selection = "none",
   options = list(
    columnDefs = list(
     list(targets = c(5,6,7,8,9), visible = FALSE),# column you want to hide
     list(targets = c(1,2,5,6,7,8,9), orderable = FALSE)#Disable sorting
    )
   )
  )
  showToast("success",
            "Student details Deleted!",
            .options = myToastOptions )
 })
 #LECTURERS
 
 #entered data
 releasedDir <- "released_results"
 #approved_results directory
 approvedDir <- "approved_results"
 #update choices
 updateSelectizeInput(
  session = session,
  inputId = "reg",
  choices = registrationData()$reg,
  selected = "",
  server = TRUE,
  options = list(maxOptions = 3)
 )
 updateSelectizeInput(
  session = session,
  inputId = "code",
  choices = units$code,
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
    value = '<button id="approve_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-check-to-slot" role="presentation" aria-label="check-to-slot icon"></i></button><button id="delete_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button><button id="edit_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>'
   )
   data
  } else if (length(released_files) == 1){
   released_data <- lapply(released_files, read.csv, stringsAsFactors = FALSE)
   #set as a data frame
   released_data <- as.data.frame(released_data)
   #change to 1 decimal place
   released_data$score <- as.numeric(released_data$score) |> round(0)
   released_data$Filename <- released_files
   #data set
   data <- released_data
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "actions",
    value = '<button id="approve_ 2" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-check-to-slot" role="presentation" aria-label="check-to-slot icon"></i></button><button id="delete_ 2" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button><button id="edit_ 2" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>'
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
   released_data$Filename <-  rev(released_files)
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
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "actions",
    value =  new_button
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
    data <- sapply(fields, function(x) input[[x]])
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
   #student name
   output$student_name <- renderText({
    paste(input$reg, input$name)
   })
   #student_tab marks
   output$student_marks <- DT::renderDataTable({
    t_b <- loadData() |> filter(reg %in% input$reg) |>
     select(code, course, grade) |>
     arrange(code)
    t_b
   })
   
   #reset all fields to blank
   #update choices
   updateSelectizeInput(
    session = session,
    inputId = "reg",
    choices = registrationData()$reg,
    server = TRUE,
    selected = "",
    options = list(maxOptions = 3)
   )
   updateSelectizeInput(
    session = session,
    inputId = "code",
    choices = units$code,
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
   originalFile <- row$Filename 
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
   #create new file
   newFile <- data.frame(
    reg = row$reg, name = row$name, code = row$code, course = row$course,
    score = input$score,	grade = input$grade,	time = row$time, 
    actions = row$actions, lecturer = row$lecturer
   )
   fileName <- gsub("released_results/", "", row$Filename)
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
   showToast(
    "success", "Mark Edited!", .options = myToastOptions
   )
   #reset all fields to blank
   #update choices
   updateSelectizeInput(
    session = session,
    inputId = "reg",
    choices = registrationData()$reg,
    server = TRUE,
    selected = "",
    options = list(maxOptions = 3)
   )
   updateSelectizeInput(
    session = session,
    inputId = "code",
    choices = units$code,
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
  names <- read_csv( "data/names.csv", show_col_types = FALSE) # nolint
  name_df <- names |> filter(reg %in% x) %>%  select(name)
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
  updateTextInput(
   session = session,
   inputId = "grade",
   value = grade
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
 observeEvent(input$delete_button, {
  selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
  search_string <- paste0("delete_ ",selectedRow)
  filtered_df <- loadData()[grepl(search_string,
                                  loadData()$actions), ]
  filename_delete <- filtered_df$Filename
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
 })
 #approve a response file
 observeEvent(input$approve_button, {
  selectedRow <- as.numeric(strsplit(input$approve_button, "_")[[1]][2])
  search_string <- paste0("approve_ ",selectedRow)
  filtered_df <- loadData()[grepl(search_string,
                                  loadData()$actions), ]
  filename_approve <- filtered_df$Filename
  file <- read.csv(filename_approve, stringsAsFactors = FALSE)
  # change buttons
  file$actions <- paste0(
   shinyInput(
    actionButton, 1, id = "approve1_",
    label = icon("check-to-slot"),
    style = "color: green;",
    onclick = paste0('Shiny.onInputChange( \"approve1_button\" , this.id, {priority: \"event\"})')
   ),
   shinyInput(
    actionButton, 1, id = "delete1_",
    label = icon("trash"),
    onclick = paste0('Shiny.onInputChange( \"delete1_button\" , this.id, {priority: \"event\"})')
   ),
   shinyInput(
    actionButton, 1, id = "edit1_",
    label = icon("file-pen"),
    onclick = paste0('Shiny.onInputChange( \"edit1_button\" , this.id, {priority: \"event\"})')
   )
  ) 
  #write the file
  write.csv(file,filename_approve,row.names = FALSE, quote = TRUE)
  # Source directory and file name
  source_dir <- "released_results"
  fileName <- gsub("released_results/", "", filename_approve)
  file_name <- fileName 
  # Destination directory
  dest_dir <- "approved_results"
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
  showToast("success", "Mark Approved!",
            .options = myToastOptions )
 })
 #edit a response file
 observeEvent(input$edit_button,{
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
 })
 #control approved files
 observeEvent(input$delete1_button, {
  showToast(
   "error", "Approved Mark: Action Not allowed!",
   .options = myToastOptions
  )
 })
 observeEvent(input$approve1_button, {
  showToast(
   "error", "Approved Mark: Action Not allowed!",
   .options = myToastOptions
  )
 })
 observeEvent(input$edit1_button, {
  showToast(
   "error", "Approved Mark: Action Not allowed!",
   .options = myToastOptions
  )
 })
}

shinyApp(ui, server)
