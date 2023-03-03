#import libraries
library(shiny)
library(readr)
library(tidyverse)

#Import data
results <- read_csv("data/results.csv",show_col_types = FALSE)

#App User Interface
ui <- navbarPage(
  title = " PORTAL",
  theme = bslib::bs_theme(4),
  tabPanel(
    title = "Overall",
    value = "overall",
    icon = icon("table-cells"),
    selectizeInput(
      inputId = "reg", 
      label = "Registration number",
      multiple = FALSE,
      choices = NULL
    ),
    tableOutput('admin_results')
  ),
  tabPanel(
    title = "Student",
    value = "student",
    icon = icon("children"),
    textOutput('name'),
    tableOutput('student_results')
  ),
  tabPanel(
    title = "Lecturer",
    value = "lecturer",
    icon = icon("person-chalkboard")
  ),
)
server <- function(input, output, session) {
  output$admin_results <- renderTable(
    results
  )
  updateSelectizeInput(
    session = session,
    inputId = 'reg',
    choices = results$Reg.No,
    server = TRUE
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

