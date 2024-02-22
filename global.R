#import libraries and packages
library(shiny)
library(dplyr)
library(shinyFeedback)
library(shinyjs)
library(shinyBS)
library(shinyvalidate)
library(shinyWidgets)
library(stringr)
library(echarts4r)
library(DT)
library(pagedown)
library(shinydashboard)
library(shinyTime)
library(shinybusy)
library(lubridate)
library(RMySQL)
library(dotenv)
library(DBI)
library(data.table)
library(shinydashboardPlus)
library(waiter)

# Retrieve database credentials
dotenv::load_dot_env()
host <- Sys.getenv("DB_HOST")
port <- as.numeric(Sys.getenv("DB_PORT"))
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_PASSWORD")

#import data
source("confirm_modal_dialog.R")
source("edit_modal_dialog.R")
source("user_timeline.R")

# Define the fields we want to save from the form
marks_fields <- c("reg", "name", "code", "course", "score", "grade",
                  "time", "lecturer", "actions", "student_year")
users <- c("Dr. John", "Prof. Makore", "Mr. Wamae", "Ms. Mutua", "Dr. Ochieng")
reg_fields <- c("Name",	"Gender",	"ID", "Reg", "Code", "Course", "Date",
                "Buttons", "Year")

# use this list for all your toasts
myToastOptions <- list(
  positionClass = "toast-top-right",
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

my_valuebox <- function(value, title, subtitle, icon = NULL,
                        color = NULL, width = 4, href = NULL) {
  shinydashboard:::validateColor(color)
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  if (!is.null(href))
    boxContent <- a(href = href, boxContent)
  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

my_color <- function(x) {
  case_when(
    x > 0 ~ "green",
    x == 0 ~ "teal",
    x < 0 ~ "red",
    TRUE ~ "yellow"
  )
}

my_symbol <- function(x) {
  case_when(
    x > 0 ~  "&uarr;",
    x == 0 ~ "&rarr;",
    x < 0 ~ "&darr;",
    TRUE ~ "&ndash;"
  )
}
