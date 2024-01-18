server <- function(input, output, session){
 # Create a connection to the MySQL database
 con <- dbConnect(RMySQL::MySQL(),
                  dbname = dbname,
                  host = host,
                  port = port,
                  user = user,
                  password = password)
 
 # populate required value entries
 observeEvent(input$Code, {
  code <- input$Code
  # define course
  course <- reactive({
   if(code == "X74") {
    course <- "Economics"
   } else if (code == "X75") {
    course <- "Economics and Statistics"
   } else (
    return()
   )
  })
  updateTextInput("Course", session = session,  value = course())
 })
 
 # Create a vector to store used numbers
 usedNumbers <- function() {
  # Extract numbers between slashes
  numbers <- sub(".*/(\\d+)/.*", "\\1", data$table_data$Reg)
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
 # Load data from MySQL into a reactiveValues object
 data <- reactiveValues(table_data = NULL)
 # Load data from MySQL table into a data.table
 data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
 
 observe({
  # Load data from MySQL table into a data.table
  data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  if(nrow(data$table_data)== 0){
   #create an ideal button
   ideal <- '<button id="deletereg_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;deletereg_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Delete"> <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="editreg_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;editreg_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Edit"> <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
    <button id="year_ 1" type="button" class="btn btn-default action-button" style="color: black;"   onclick="Shiny.onInputChange( &quot;year_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Promote"> <i class="fas fa-ranking-star" role="presentation" aria-label="ranking-star icon"></i></button>
    <button id="timeline_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;timeline_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Timeline"> <i class="fas fa-clock-rotate-left" role="presentation" aria-label="clock-rotate-left icon"></i></button>
    <button id="Edited" type="button" class="btn btn-default action-button" style="background-color: #e9ecef; " disabled data-title="Edited">NO</button>'
    
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value = ideal
   )
  }else{
   button <- data$table_data |> arrange(desc(Date)) |> select(Actions) 
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
               "color: black;" , new_button) 
   new1 <- gsub("background-color: #0025ff8f;", 
                "background-color: #e9ecef;", new)
   new2 <- gsub("YES","NO",new1)  
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value =  new2
   )
   }
 })
 # create a unique registration number
 observeEvent(input$Code, {
 if (!is.null(code) && !is.null(date)) {
  new_reg()
 } else if (!is.null(code) && !is.null(date)){
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

 # Render the DataTable
 output$registrationTable <- renderDataTable({
   data <- data$table_data |> arrange(desc(Date))
   datatable(data,
             escape = FALSE,
             selection = "none",
             options = list(
              columnDefs = list(
               list(targets = c(5,6), visible = FALSE),# column you want to hide
               list(targets = c(1,3,4,5,6,9), orderable = FALSE)#Disable sorting
              )
             )
   )
 })
 #preview uploaded photo
 observe({
  req(input$photoInput)
  #Update the preview output
  output$previewImage <- renderImage({
   list(src = input$photoInput$datapath)
  }, deleteFile = FALSE)
 })
 # Add a new row
 observeEvent(input$registerButton, {
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
  #ensure no duplicates for ID
  # Check if the entered ID already exists in the column
  if(input$ID %in%data$table_data$ID) {
   showToast("error",
             "ID Number Exists!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()  # Stop execution if duplicate ID
  }
  
  #entered inputs data
  name <- input$Name
  ID <- input$ID
  gender <- input$Gender
  Reg_No <- input$Reg
  Code <- input$Code
  Course <- input$Course
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Years <- input$Year
  Buttons <- input$Buttons
  Users <- "Administrator"
  Description <- "Registered as a new Student"
  # Insert data into the database
  insert_query_1 <- paste0("INSERT INTO student_details
                  VALUES('",name,"',",ID,",'",gender,"','",Reg_No,"','",Code,"',
                                    '",Course,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),
                                    ",Years,",'",Buttons,"')")
  insert_query_2 <- paste0("INSERT INTO student_timeline
                  VALUES('",Reg_No,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Description,"')")
  DBI::dbSendQuery(con,insert_query_1)
  DBI::dbSendQuery(con,insert_query_2)
  # Reload data
  data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  resetLoadingButton("registerButton")
  showToast("success",
            "New student Registered!",
            .options = myToastOptions )
  resetLoadingButton("registerButton")
  # Clear the input fields
  updateTextInput(session, "Name", value = "")
  updateNumericInput(session, "ID", value = "")
  updateSelectInput(session, "Gender", selected = NULL)
  reset("photoInput")
  #Update the preview output
  output$previewImage <- renderImage({
   list(src = "")
  }, deleteFile = FALSE)
  new_reg()
 })
 
 # Edit new row
 observeEvent(input$editreg_button, {
 confirm_modal_dialog(approve = FALSE, edit = FALSE, delete = FALSE,
               editreg = TRUE, promote = FALSE, deletereg = FALSE)
 })
 observeEvent(input$dismiss, {
  removeModal()
 })
 #preview uploaded photo
 observe({
  req(input$edit_photoInput)
  #Update the preview output
  output$edit_previewImage <- renderImage({
   list(src = input$edit_photoInput$datapath)
  }, deleteFile = FALSE)
 })
 #edit a student registration file
 observeEvent(input$confirm_editreg,{
  # Load data from MySQL table into a data.table
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  edit_modal_dialog()
  selectedRow <- as.numeric(strsplit(input$editreg_button, "_")[[1]][2])
  search_string <- paste0("editreg_ ",selectedRow)
  row <- data[grepl(search_string,
                               data$Actions), ]
  updateSelectInput(session, "edit_code", selected = row$Code )
  updateTextInput(session, "edit_name", value = row$Name)
  updateSelectInput(session, "edit_gender", selected = row$Gender)
  updateNumericInput(session, "edit_ID", value = row$ID)
  updateTextInput(session, "edit_course", value = row$Course)
  updateTextInput(session, "edit_buttons", value = row$Actions)
  # Get ID to prevent empty execution
  entered_ID <- row$ID
  string <- data |> filter(grepl(entered_ID, ID))
  string_selected <- string |> select(Serial)
  string_selected <- string_selected[[1]]
  #update the regInput 
  updateTextInput(session = session, inputId = "edit_reg",
                  value = string_selected )
  
 })
 observeEvent(input$cancel, {
  removeModal()
 })
 #on edit mode
 observeEvent(input$edit_code, {
  req(input$edit_reg != "")
  string <- input$edit_reg
  #replacing string
  code <- input$edit_code
  # Replace characters before the first slash with the new value
  result <- sub("^[^/]+", code, string)
  #update the regInput 
  updateTextInput(session = session, inputId = "edit_reg",
                  value = result
  )
  # define course
  course <- reactive({
   if(code == "X74") {
    course <- "Economics"
   } else if (code == "X75") {
    course <- "Economics and Statistics"
   } else (
    return()
   )
  })
  updateTextInput("edit_course", session = session,  value = course())
  
 })
 # save edits of a row
 observeEvent(input$save, {
  # validate inputs
  # Check if the ID input has 8 characters
  if (nchar(input$edit_ID) != 8 || is.na(input$edit_ID)) {
   showToast("error",
             "ID should be 8 characters!",
             .options = myToastOptions )
   return()
  }
  # Check if the name input is empty
  if (is.null(input$edit_name) || input$edit_name == "") {
   showToast("info",
             "Enter Name!",
             .options = myToastOptions ) 
   return()
  }
  # change color for edited rows
  button <- input$edit_buttons
  new_button <- gsub("background-color: #e9ecef;", 
                     "background-color: #0025ff8f;", button)
  edited_buttons <- gsub("NO","YES",new_button)
  # collect all entries
  name <- input$edit_name
  id <- input$edit_ID
  gender <- input$edit_gender
  reg_no <- input$edit_reg
  code <- input$edit_code
  course <- input$edit_course
  buttons <- edited_buttons
  # extract serial number string
  string <- input$edit_reg
  # Extract the unique number between slashes
  number <- str_extract(string, "(?<=/)[0-9]+(?=/)")
  # create the query
   update_query <- sprintf("UPDATE student_details SET
                           Name = '%s',
                           ID = %s,
                           Gender = '%s',
                           Serial = '%s',
                           Code = '%s',
                           Course = '%s',
                           Actions = '%s'
                           WHERE Serial LIKE '%%%s%%'", 
                           name, id, gender, reg_no, code, course, buttons, 
                           number)
   
  DBI::dbSendQuery(con,update_query)
  # Reload data
  data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  showToast("success",
            "Student Data Edited!",
            .options = myToastOptions )
  removeModal()
  #update timeline
  Users <- "Administrator"
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Description <- "Edited Student Data"
  timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Description,"')")
  DBI::dbSendQuery(con,timeline_query)
 })
 observeEvent(input$deletereg_button, {
  confirm_modal_dialog(approve = FALSE, edit = FALSE, delete = FALSE,
                       editreg = FALSE, promote = FALSE, deletereg = TRUE)
 })
 observeEvent(input$confirm_deletereg, {
  selectedRow <- as.numeric(strsplit(input$deletereg_button, "_")[[1]][2])
  search_string <- paste0("deletereg_ ",selectedRow)
  search_string <- paste0("'%", search_string, "%'")
  # Query to remove a row
  delete_query <- paste0("DELETE FROM student_details WHERE
                           Actions LIKE", search_string 
                          )
  DBI::dbSendQuery(con,delete_query)
  # Reload data from MySQL after deleting the row
  updated_data <- dbGetQuery(con, "SELECT * FROM student_details")
  # Update the Shiny reactiveValues with the updated data
  data$table_data <- as.data.table(updated_data)
  showToast("success",
            "Student Data Deleted!",
            .options = myToastOptions )
  removeModal()
 })
 # promote a student to the next year
 observeEvent(input$year_button, {
  confirm_modal_dialog(approve = FALSE, edit = FALSE, delete = FALSE,
               editreg = FALSE, promote = TRUE, deletereg = FALSE)
 })
 observeEvent(input$confirm_promote, {
 selectedRow <- as.numeric(strsplit(input$year_button, "_")[[1]][2])
 search_string <- paste0("year_ ",selectedRow)
 # import data
 promote_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
 filtered_df <- promote_data[grepl(search_string, promote_data$Actions), ]
 current_year <- as.numeric(filtered_df$Year)
 # add a plus one
 new_year <- current_year + 1
 # update the row
 update_year_query <- sprintf("UPDATE student_details SET Year = %s WHERE Actions LIKE '%%%s%%'", new_year, search_string) 
 DBI::dbExecute(con,update_year_query)
 # Reload data from MySQL after deleting the row
 updated_data <- dbGetQuery(con, "SELECT * FROM student_details")
 # Update the Shiny reactiveValues with the updated data
 data$table_data  <- as.data.table(updated_data)                           
 showToast("success",
           "Student Promoted!",
           .options = myToastOptions )
 removeModal()
 })
 # view a student timeline
 observeEvent(input$timeline_button, {
  selectedRow <- as.numeric(strsplit(input$timeline_button, "_")[[1]][2])
  search_string <- paste0("timeline_ ",selectedRow)
  # import data
  timeline_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_timeline"))
  details_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  filtered_df <- details_data[grepl(search_string, details_data$Actions), ]
  # Get the student in review
  Serial_No <- filtered_df$Serial
  Name <- filtered_df$Name
  student_timeline_data <- timeline_data |> filter(Serial %in% Serial_No) 
  # Convert Date column to datetime
  student_timeline_data$Date <- as.POSIXct(student_timeline_data$Date, tz = "UTC")
  # Separate Date column into Date and Time columns
  student_timeline_data <- student_timeline_data %>%
   mutate(DateOnly = as.Date(Date),
          TimeOnly = format(Date, "%H:%M:%S")) |>
   arrange(desc(TimeOnly))
  # open the Modal Dialog first
  user_timeline_modal(reg_no = paste(Serial_No,"",Name))
  # Render UI for timeline
  output$timeline_block <- renderUI({
   unique_dates <- unique(student_timeline_data$DateOnly)
   lapply(seq_along(unique_dates), function(i) {
    current_date <- unique_dates[i]
    current_data <- filter(student_timeline_data, DateOnly == current_date)
    timelineBlock(
     width = 24,
     timelineLabel(format(current_date, "%Y-%m-%d"),
                   color = "teal"),
     lapply(seq_len(nrow(current_data)), function(j) {
      timelineItem(
       current_data$Description[j],
       title = current_data$Users[j],
       time = current_data$TimeOnly[j],
       footer = icon(name = "file-pen")
      )
     })
    )
   })
  })
 })
 observeEvent(input$ex, {
  removeModal()
  
 })
 session$onSessionEnded(function() {
  dbDisconnect(con, add = TRUE)  # Disconnect when the session ends
   })
}
