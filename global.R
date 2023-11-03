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
library(shinydashboard)
library(shinyTime)
#import data
units <- read_csv("data/units.csv",show_col_types = FALSE) # nolint
admin <- read_csv("data/admin.csv",show_col_types = FALSE) # nolint

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
my_valuebox <- function(value, title, subtitle, icon = NULL, 
                        color = NULL, width = 4, href = NULL){
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
my_color <- function(x){
 case_when(
  x > 0 ~"green",
  x == 0 ~ "teal",
  x < 0 ~ "red",
  TRUE ~ "yellow"
 )
}
my_symbol <- function(x){
 case_when(
  x > 0 ~  "&uarr;",
  x == 0 ~ "&rarr;",
  x < 0 ~ "&darr;",
  TRUE ~ "&ndash;"
 )
}