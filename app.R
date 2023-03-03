library(shiny)
library(rdrop2)
library(shinyFeedback)
library(shinyjs)
library(readr)
library(shinyvalidate)
library(DT)
library(tidyverse)
library(prodlim)
#import data
units <- read_csv("data/units.csv",show_col_types = FALSE)
names <- read_csv("data/names.csv",show_col_types = FALSE)
# Define the fields we want to save from the form
fields <- c("reg", "name","code","score","grade",'time','lecturer')
users <- c('DR.JOHN','PROF.MAKORE','MR.WAMAE','MS.MUTUA','DR.OCHIENG')
# use this list for all your toasts
myToastOptions <- list(
 positionClass = "toast-top-right",
 progressBar = FALSE,
 timeOut = 3000,
 closeButton = TRUE,
 
 # same as defaults
 newestOnTop = TRUE,
 preventDuplicates = TRUE,
 showDuration = 300,
 hideDuration = 1000,
 extendedTimeOut = 1000,
 showEasing = "linear",
 hideEasing = "linear",
 showMethod = "fadeIn",
 hideMethod = "fadeOut"
)

ui = fluidPage(
  useShinyjs(),
  tags$br(),
  tags$hr(),
  useShinyFeedback(),
  includeCSS('www/styles.css'),
  splitLayout(
    selectizeInput(
    inputId = "reg", 
    label = "Registration number",
    multiple = FALSE,
    choices = NULL
  ),
  disabled(
    textInput('name','Student name',placeholder ='name')
  ),
  selectizeInput(
    inputId = "code", 
    label = "Code",
    multiple = FALSE,
    choices = NULL,
    width = '70px'
    ),
  disabled(
    textInput('course','Course',placeholder ='course')
  ),
  numericInput('score','Score',value =NULL,min = 1,max = 99,width = '60px' ),
  disabled(
    textInput('grade','Grade',width = '40px'),
   hidden(textInput('time','Time')),
   hidden(textInput('lecturer','Lecturer'))
    ),
  div(
    style="padding-top: 25px;",
  loadingButton("submit", "Submit",loadingLabel = 'Entering',
                loadingSpinner = 'cog',loadingStyle = 'color:green'),
  )
  ),
  tags$hr(),
  DT::dataTableOutput("marks")
  
)
server = function(input, output, session) {
  # entered data
  outputDir <- "responses"
  # update choices
  updateSelectizeInput(
    session = session,
    inputId = 'reg',
    choices = names$reg,
    server = TRUE,
    options = list(maxOptions = 3)
  )
  updateSelectizeInput(
    session = session,
    inputId = 'code',
    choices = units$code,
    server = TRUE,
    options = list(maxOptions = 3)
  )
 
  #update uneditable fields from database
  observe({
    x <-input$reg
    name_df <- names |> filter(reg %in% x)%>%  select(name)
    name <- name_df[[1]]
    updateTextInput(
      session = session,
      inputId = 'name',
      value = name
    )
    y <-input$code
    course_df <- units |> filter( code %in% y)%>%  select(title)
    course <- course_df[[1]]
    updateTextInput(
      session = session,
      inputId = 'course',
      value = course
    )
    #set a grade on scale
    z <- input$score
    grade <- case_when(
      z >= 70 ~'A',
      z >= 60 ~'B',
      z >= 50 ~'C',
      z >= 40 ~'D',
      z >=1 ~ 'E',
      TRUE ~ ''
    )
    updateTextInput(
      session = session,
      inputId = 'grade',
      value = grade
    )
    
    #check already available results
    #load available data
    loadData <- function() {
     # Read all the files into a list
     files <- list.files(outputDir, full.names = TRUE)
     data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
     # Concatenate all data together into one data.frame
     data <- do.call(rbind, data) 
     data 
    }
    data_entered <- c(input$reg,input$code) 
    data_saved <- loadData() |> select(reg,code)
    #check for a row match
    if(is.na(row.match(data_entered,data_saved))) {
     enable('submit')
    } else {
     showToast('warning','Mark already exists!',.options = myToastOptions)
     disable('submit')
    }
})
  #resetting fields
  observeEvent(input$reg, {
    updateTextInput(
      session = session,
      inputId = 'grade',
      value = ''
    )
    updateNumericInput(
      session = session,
      inputId = 'score',
      value = ''
    )
  })
  observeEvent(input$code, {
    updateTextInput(
      session = session,
      inputId = 'grade',
      value = ''
    )
    updateNumericInput(
      session = session,
      inputId = 'score',
      value = ''
    )
    updateTextInput(
      session = session,
      inputId = 'time',
      value = format(Sys.time(), "%a %e %b %Y %H:%M:%S ")
    )
    updateTextInput(
      session = session,
      inputId = 'lecturer',
      value = sample(users,1)
    )
  })
 observeEvent(input$score,{
   x <- input$score
   if(x>99 |is.na(x)| x<1) {
     updateNumericInput(
       session = session,
       inputId = 'score',
       value = ''
     )
   }else {
    return()
   }
 } )
 
 # When the Submit button is clicked, save the form data
 observeEvent(input$submit, {
   #put control to remove NA values
  if(input$reg=='' | input$code=='' | is.na(input$score)
     |input$grade=='') {
   resetLoadingButton('submit')
showToast(
 'error','Fill all fields!',.options = myToastOptions
 )   
  } else {
   # Whenever a field is filled, aggregate all form data
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
     fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
     # Write the file to the local system
     write.csv(
       x = data,
       file = file.path(outputDir, fileName), 
       row.names = FALSE, quote = TRUE
     )
   }
   saveData(formData())
   resetLoadingButton('submit')
   loadData <- function() {
     # Read all the files into a list
     files <- list.files(outputDir, full.names = TRUE)
     data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
     # Concatenate all data together into one data.frame
     data <- do.call(rbind, data) 
     data 
   }
   # Show the previous responses
   # (update with current response when Submit is clicked)
   output$marks <- DT::renderDataTable({
    loadData() 
   }) 
   showToast(
    'success','Mark entered.',.options = myToastOptions
    ) 
   #reset all fields to blank
   updateTextInput(
    session = session,
    inputId = 'reg',
    value = ''
   )
   updateNumericInput(
    session = session,
    inputId = 'code',
    value = ''
   )
   updateNumericInput(
    session = session,
    inputId = 'score',
    value = ''
   )

  }
 })

}

shinyApp(ui, server)

