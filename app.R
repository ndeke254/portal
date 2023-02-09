#import libraries
library(shiny)
library(readr)
library(tidyverse)

#Import data
results <- read_csv("results.csv",show_col_types = FALSE)

#App User Interface

ui <- fluidPage(
  tags$h2(
    'Admin View'
  ),
  tableOutput('admin_results'),
tags$h2(
  'Students View'
),
selectizeInput(
  inputId = "reg", 
  label = "Registration number",
  multiple = FALSE,
  choices =''
),
textOutput('name'),
tableOutput('student_results')
)
#App server
server <- function(input, output, session) {
  output$admin_results <- renderTable(
    results
  )
  updateSelectizeInput(
    inputId = 'reg',
    choices = results$Reg.No
  )
  
  output$student_results <- renderTable({
    x <- input$reg
    data <- results |> filter(Reg.No %in% x)
    data <- data |> select(`XET201 MICROECONOMICS`)
    r_score <- as.character(data[1,])
    name <- data |> colnames()
    grade_summary <- data.frame(Unit= name,
                                Grade = r_score
                                )
    final_grade <- str_sub(grade_summary$Grade,-1)
    show_table <- data.frame(Unit= name,
                             Grade = final_grade
                             )
    show_table
})
  output$name <- renderText({
    x <- input$reg
    data <- results |> filter(Reg.No %in% x)
    paste(data[[1]],
    data[[2]])
  })
}

shinyApp(ui, server)



