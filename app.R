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
  splitLayout(
   cellWidths = c("18.286%","8.286%","9.286%", # nolint
                  "5.286%","12.286%","24.286%","0%","8.286%"
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
   condition = "input.editreg_button", 
   tags$div( id="reset_button",
   actionButton(inputId = "reset",
                label = "",
                icon = icon("refresh"),
                title = "Reset fields")
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
  title = "Student",
  value = "student",
  icon = icon("children"),
  splitLayout(
   cellWidths = c("10.286%","14.286%","5.286%", # nolint
                  "24.286%","5.286%","4.286%","14.286%"),
 selectizeInput(
  inputId = "student_reg",
  label = "Registration number", # nolint
  multiple = FALSE,
  choices = NULL
  ),
 selectizeInput(
  inputId = "register_unit",
  label = "Register Unit", # nolint
  multiple = FALSE,
  choices = NULL
 ),
 ),
 tags$hr(),
  textOutput("student_name"),
  textOutput("student_course"),
  DT::dataTableOutput("student_marks")
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
    Filename = character(0),
    Year = character(0),
    stringsAsFactors = FALSE
    )
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value = '<button id="deletereg_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;deletereg_button&quot; , this.id, {priority: &quot;event&quot;})" title="Delete Registration"> <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="editreg_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;editreg_button&quot; , this.id, {priority: &quot;event&quot;})" title="Edit Registration"> <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
    <button id="year_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;year_button&quot; , this.id, {priority: &quot;event&quot;})" title="Promote Year"> <i class="fa fa-plus" role="presentation" aria-label="file-pen icon"></i></button>'
    )
   data
  } else if (length(registered_files) == 1){
   registered_data <- lapply(registered_files, read.csv, stringsAsFactors = FALSE)
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
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value =  new_button
   )
   data
  }else {
   registered_data <- lapply(registered_files, read.csv, stringsAsFactors = FALSE)
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
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value =  new_button
   )
  data
}
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
     list(targets = c(1,2,5,6,7), orderable = FALSE)#Disable sorting
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
   #create new file
   newFile <- data.frame(
    Name = input$Name, 
    Gender = input$Gender,
    ID = input$ID, 
    Reg = input$Reg,	
     Code = input$Code, 
    Course = input$Course,	
    Date = row$Date, 
    Year = row$Year,
    Buttons = row$Buttons
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
     list(targets = c(1,2,5,6,7), orderable = FALSE)#Disable sorting
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
 })
 # Render the first registration data table
 output$registrationTable <- renderDataTable(
  registrationData(),server = TRUE, escape = FALSE, selection = "none",
  options = list(
   columnDefs = list(
    list(targets = c(1,2,5,6,7), orderable = FALSE)#Disable sorting
   )
  )
 )

 #LECTURERS
 #entered data
 releasedDir <- "released_results"
 #approved_results directory
 approvedDir <- "approved_results"
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
    value = '<button id="approve_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})" title="Approve Mark">  <i class="fas fa-check-to-slot" role="presentation" aria-label="check-to-slot icon"></i></button>
    <button id="delete_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})" title="Delete Mark">  <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="edit_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})" title="Edit Mark">  <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>'
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
   button <- data[!grepl("approve1_1", data$actions), ]  
   if(nrow(button) > 0){
    button <- data[!grepl("approve1_1", data$actions), ]  |> select(actions)
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
   }else{
    #update the buttons
    updateTextInput(
     session = session,
     inputId = "actions",
     value = '<button id="approve_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-check-to-slot" role="presentation" aria-label="check-to-slot icon"></i></button><button id="delete_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button><button id="edit_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>'
    )
   }
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
   button <- data[!grepl("approve1_1", data$actions), ]  
   if(nrow(button) > 0){
   button <- data[!grepl("approve1_1", data$actions), ]  |> select(actions)
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
   } else{
    #update the buttons
    updateTextInput(
     session = session,
     inputId = "actions",
     value = '<button id="approve_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-check-to-slot" role="presentation" aria-label="check-to-slot icon"></i></button><button id="delete_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button><button id="edit_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})">  <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>'
    )
    }
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
   #create new file
   newFile <- data.frame(
    reg = row$reg, name = row$name, code = row$code, course = row$course,
    score = input$score,	grade = input$grade,	time = row$time, 
    actions = row$actions, lecturer = row$lecturer
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
  observeEvent(input$deletereg_button, {
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
      list(targets = c(1,2,5,6,7), orderable = FALSE)#Disable sorting
     )
    )
   )
   updateTextInput( session = session, inputId = "editmode", value = 0)
   hideToast(animate = TRUE, session = session)
   new_reg()
   showToast("success",
             "Student details Deleted!",
             .options = myToastOptions )
 })
 #approve a response file
 observeEvent(input$approve_button, {
  selectedRow <- as.numeric(strsplit(input$approve_button, "_")[[1]][2])
  search_string <- paste0("approve_ ",selectedRow)
  filtered_df <- loadData()[grepl(search_string,
                                  loadData()$actions), ]
  filename_approve <- filtered_df$filename
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
 #promote a student
 observeEvent(input$year_button, {
  selectedRow <- as.numeric(strsplit(input$year_button, "_")[[1]][2])
  search_string <- paste0("year_ ",selectedRow)
  filtered_df <- registrationData()[grepl(search_string,
                                          registrationData()$Buttons), ]
  current_year <- as.numeric(filtered_df$Year)
  if(current_year < 4) {
   new_year <- current_year + 1
   filtered_df$Year <- new_year
   originalFile <- filtered_df$Filename 
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
      list(targets = c(1,2,5,6,7), orderable = FALSE)#Disable sorting
     )
    )
   )
  showToast(
   "success", "Student promoted!",
   .options = myToastOptions
  )
  }else{
   showToast(
    "error", "Student:Final year!",
    .options = myToastOptions
   )
  }
 })
 #delete a response file
 observeEvent(input$delete_button, {
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
 #STUDENT TAB
 observeEvent(input$student_reg, {
  student_data <- registrationData() |> filter(Reg %in% input$student_reg)
  t_b <- loadData()[grepl("approve1_1", loadData()$actions), ]
  t_b <- t_b |> filter(reg %in% input$student_reg) |>
   select(code, course, grade) |>
   arrange(code)
  #student name
  output$student_name <- renderText({
   paste(student_data$reg, student_data$name)
  })
 output$student_course <- renderText({
  paste(student_data$code, student_data$course)
 })
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
 })
 #edit a student registration file
 observeEvent(input$editreg_button,{
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
  
 })
 }
shinyApp(ui, server)
