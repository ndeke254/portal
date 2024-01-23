server <- function(input, output, session){
 # Create a connection to the MySQL database
 con <- dbConnect(RMySQL::MySQL(),
                  dbname = dbname,
                  host = host,
                  port = port,
                  user = user,
                  password = password)
 
 ### ADMINISTRATOR REGISTRATION
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
 register_units <- reactiveValues(data_table = NULL)
 # Load data from MySQL table into a data.table
 data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
 register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
 observe({
  # Load data from MySQL table into a data.table
  data_table_1 <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  marks_data <- register_units$data_table |> filter(!is.na(Score))
  if(nrow(data$table_data) == 0 || nrow(marks_data) == 0 ){
   #create an ideal button
   ideal <- '<button id="deletereg_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;deletereg_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Delete"> <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="editreg_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;editreg_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Edit"> <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
    <button id="year_ 1" type="button" class="btn btn-default action-button" style="color: black;"   onclick="Shiny.onInputChange( &quot;year_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Promote"> <i class="fas fa-ranking-star" role="presentation" aria-label="ranking-star icon"></i></button>
    <button id="timeline_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;timeline_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Timeline"> <i class="fas fa-clock-rotate-left" role="presentation" aria-label="clock-rotate-left icon"></i></button>
    <button id="Edited" type="button" class="btn btn-default action-button" style="background-color: #e9ecef; " disabled data-title="Edited">NO</button>'
  #marks table buttons
   ideal_marks <- '<button id="delete_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Delete"> <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="edit_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Edit"> <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
    <button id="approve_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Approve"> <i class="fas fa-circle-check" role="presentation" aria-label="circle-check icon"></i></button>
    <button id="reject_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;reject_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Reject"> <i class="fas fa-circle-xmark" role="presentation" aria-label="circle-xmark icon"></i></button>
    <button id="Status" type="button" class="btn btn-default action-button" style="background-color: #e9ecef; " disabled data-title="Status">ENTERED</button>'
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value = ideal
   )
   updateTextInput(
    session = session,
    inputId = "actions",
    value = ideal_marks
   )
  }else{
   button <- data$table_data |>
    arrange(desc(Date)) |>
    select(Actions) 
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
  
  #marks table update
  button_marks <- register_units$data_table |> 
   arrange(desc(Date)) |> 
   select(Actions) 
  button_marks <- button_marks[[1,1]]
  # Find all IDs containing a number
  ids_marks <- str_extract_all(button_marks, "(?<=id=\")\\w+\\s\\d+(?=\")")[[1]]
  # Replace IDs with the next number
  new_button_marks <- button_marks
  for (id in ids_marks) {
   number <- str_extract(id, "\\d+")
   next_number <- as.numeric(number) + 1
   new_id <- gsub("\\d+", next_number, id)
   new_button_marks <- str_replace(new_button_marks, id, new_id)
  }
  new_marks <- gsub("color: green;", 
              "color: black;" , new_button_marks) 
  new1_marks <- gsub("background-color: #0025ff8f;", 
               "background-color: #e9ecef;", new_marks)
  new2_marks <- gsub("YES","NO",new1_marks)  
  #update the buttons
  updateTextInput(
   session = session,
   inputId = "actions",
   value =  new2_marks
  )
  }
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "student_reg",
   choices = data_table_1$Serial,
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   choices =  unique(register_units$data_table$Serial),
   selected = unique(register_units$data_table$Serial[1]),
   server = TRUE,
   options = list(maxOptions = 3)
  )
  
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
  Actions <- "INSERT NEW"
  Description <- paste("Added ",Reg_No, " : ", name, " as a new Student", sep = "")
  # Insert data into the database
  insert_query_1 <- paste0("INSERT INTO student_details
                  VALUES('",name,"',",ID,",'",gender,"','",Reg_No,"','",Code,"',
                                    '",Course,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),
                                    ",Years,",'",Buttons,"')")
  insert_query_2 <- paste0("INSERT INTO student_timeline
                  VALUES('",Reg_No,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
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
  # Update dependencies
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "student_reg",
   choices = data$table_data$Serial,
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
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
  row <- data[grepl(search_string, data$Actions), ]
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
  # Extract the unique number between slashes
  number <- str_extract(reg_no, "(?<=/)[0-9]+(?=/)")
  # Load data from MySQL table into a data.table
  selected_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  selected_data <- selected_data[grepl(number, selected_data$Serial), ]
  Name <- selected_data$Name
  ID <- selected_data$ID
  Gender <- selected_data$Gender
  Serial <- selected_data$Serial
  Code <- selected_data$Code
  Course <- selected_data$Course
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
                           name, id, gender, reg_no, code, course, buttons,number)
   
  DBI::dbSendQuery(con,update_query)
  # Reload data
  data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  showToast("success",
            "Student Data Edited!",
            .options = myToastOptions )
  removeModal()
  # Compare values and identify changes
  changes <- data.frame(
   Name = ifelse(Name != name, paste(Name, "  →  ", name), NA),
   ID = ifelse(ID != id, paste(ID, "  →  ", id), NA),
   Gender = ifelse(Gender != gender, paste(Gender, "  →  ", gender), NA),
   Serial = ifelse(Serial != reg_no, paste(Serial, " →  ", reg_no), NA),
   Code = ifelse(Code != code, paste(Code, "  →  ", code), NA),
   Course = ifelse(Course != course, paste(Course, "  →  ", course), NA)
  )
  # Filter out rows where no changes occurred
   filtered_dataframe <- changes[, !is.na(changes[1, ])]
   values <- unname(unlist(filtered_dataframe))
 #update timeline
  Users <- "Administrator"
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Actions <- "UPDATE USER"
  Description <- paste("Edited data:", paste(values, collapse=", "))
  #write changes
  timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
  DBI::dbSendQuery(con,timeline_query)
 })
 observeEvent(input$deletereg_button, {
  confirm_modal_dialog(approve = FALSE, edit = FALSE, delete = FALSE,
                       editreg = FALSE, promote = FALSE, deletereg = TRUE)
 })
 #confirm deletion of a row record
 observeEvent(input$confirm_deletereg, {
  selectedRow <- as.numeric(strsplit(input$deletereg_button, "_")[[1]][2])
  search_string_1 <- paste0("deletereg_ ",selectedRow)
  search_string <- paste0("'%", search_string_1, "%'")
  # Load data from MySQL table into a data.table
  delete_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  # Get the serial number
  row <- delete_data[grepl(search_string_1, delete_data$Actions), ]
  Serial <- row$Serial
  # Extract the unique number between slashes
  number <- str_extract(Serial, "(?<=/)[0-9]+(?=/)")
  # Query to remove a row
  delete_query <- paste0("DELETE FROM student_details WHERE
                           Actions LIKE", search_string 
                          )
  timeline_delete_query <- paste0("DELETE FROM student_timeline 
                                   WHERE Serial LIKE", paste0("'%", number, "%'"))
  DBI::dbSendQuery(con, delete_query)
  DBI::dbSendQuery(con, timeline_delete_query)
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
 # update timeline after action
 reg_no <- filtered_df$Serial
 Users <- "Prof. Maclain"
 Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
 Actions <- "PROMOTE STUDENT"
 Description <- paste("Promoted ",reg_no, " from Year ", current_year, " to Year ", new_year, sep = "")
 timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
 DBI::dbSendQuery(con,timeline_query)
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
  # Extract numbers between slashes
  numbers <- sub(".*/(\\d+)/.*", "\\1", Serial_No)
  numbers <- as.numeric(numbers)
  Name <- filtered_df$Name
  student_timeline_data <- timeline_data[grepl(numbers, timeline_data$Serial), ]
  # Convert Date column to datetime
  student_timeline_data$Date <- as.POSIXct(student_timeline_data$Date, tz = "UTC")
  # Separate Date column into Date and Time columns
  student_timeline_data <- student_timeline_data %>%
   mutate(DateOnly = as.Date(Date),
          TimeOnly = format(Date, "%H:%M:%S")) |>
   arrange(desc(Date))
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
      # set icons to show in chats
      action_icon <- current_data$Actions[j]
      if(action_icon %in% "INSERT NEW"){
       set_icon <- "address-card"
      } else if(action_icon %in% "UPDATE USER"){
       set_icon <- "user-pen"
      }else if(action_icon %in% "PROMOTE STUDENT"){
       set_icon <- "turn-up"
      }else if(action_icon %in% "REGISTER UNIT"){
       set_icon <- "marker"
      }else if(action_icon %in% "SUCCESS REGISTRATION"){
       set_icon <- "thumbs-up"
      }else if(action_icon %in% "UPDATE MARK"){
       set_icon <- "user-graduate"
      }else if(action_icon %in% "PAID FEES"){
       set_icon <- "money-check-dollar"
      }else{
       return()
      }
      timelineItem(
       action_icon,
       title = current_data$Users[j],
       time = current_data$TimeOnly[j],
       footer = current_data$Description[j],
       icon = icon(name = set_icon)
      )
     })
    )
   })
  })
 })
 observeEvent(input$ex, {
  removeModal()
 })
 
 ### STUDENT REGISTRATION
 observeEvent(input$student_reg, {
  req(input$student_reg)
  student_reg <- input$student_reg
  #student details
  # Load data from MySQL table into a data.table
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  student_data <- data |> filter(Serial %in%student_reg)
  
  student_year <- student_data$Year 
  student_course <- student_data$Code 
  register_units$data_table  <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  # create a table for a specific student
  student_reg_units <- register_units$data_table |> 
   filter(Serial %in% student_reg) |>
   filter(Year %in% student_year) |>
   arrange(Code)
  # Render the DataTable
  output$registered_units <- renderDataTable({
   # create a table for a specific student
   student_reg_units <- register_units$data_table |> 
    filter(Serial %in% student_reg) |>
    filter(Year %in% student_year) |>
    arrange(Code)
   datatable(student_reg_units,
             escape = FALSE,
             selection = "none",
             options = list(
              columnDefs = list(
               list(targets = c(1,2,5,6,7,10,11,12), visible = FALSE),# column you want to show
               list(targets = c(1,2,3,4,5,6,7,8,9,10,11,12), orderable = FALSE)#Disable sorting
              )
             )
   )
  })

  #student name
  output$student_name <- renderText({
   paste(student_reg, student_data$Name)
  })
  #student_course
  output$student_course <- renderText({
   paste(student_data$Code, student_data$Course,"Year",student_year)
  })
  #student_units
  units <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units"))
  student_units <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% student_year)
  #student timetable
  datetime <- c(
   "Mon 08:00", "Tue 08:00", "Wed 08:00", "Thu 08:00", "Fri 08:00",
   "Mon 11:00", "Tue 11:00", "Wed 11:00", "Thu 11:00", "Fri 11:00",
   "Mon 14:00", "Tue 14:00", "Wed 14:00", "Thu 14:00", "Fri 14:00"
  )
  sample1 <- sample(datetime, size = 15, replace = TRUE)
  sample2 <- sample(users, size = 5, replace = TRUE)
  num_rows <- nrow(student_units)
  # create a specific student timetable
  student_units_table <- student_units |> 
   select(code, title) |>
   mutate(lecturer = rep(sample2, length.out = num_rows)) |>
   mutate(time = rep(sample1, length.out = num_rows)) |>
   mutate(venue = rep("", length.out = num_rows))
  output$timetable <- DT::renderDataTable({
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
  # update field with unregistered units only
  list1 <- as.list(student_units_table$code)
  list2 <- as.list(student_reg_units$Code)
  list <- setdiff(list1, list2)
  if(length(list)>0) {
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = list,
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3)
   )
  }else{
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = NULL,
    selected = NULL,
    server = TRUE,
    options = list(maxOptions = 3) 
   )
  }
 })
  
  # create registration deadline
  observe({
  admin_file <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
  invalidateLater(1000, session)
  time1 <- as.POSIXct(admin_file[[1,2]], tz = "Africa/Nairobi")
  time2 <- as.POSIXct(Sys.time(), tz = "Africa/Nairobi")
  time_difference <- as.numeric(difftime(time1, time2, units = "secs"))
  if(time_difference > 0){
   updateSwitchInput(session = session,
                     inputId = "switch",
                     value = TRUE)
   output$notice <- renderText({
    paste("Course Registration closes in:")
   })
   output$flip_time <- renderText({
    date <- format(input$target_date, "%d-%m-%Y")
    time <- strftime(input$select_time, "%T")
    day <- weekdays(input$target_date)
    date_time <- paste("On", day, date,time)
   })
   enable("register_code")
   # Calculate days, hours, minutes, and seconds
   days <- floor(time_difference / (24 * 3600))
   time_difference_1 <- time_difference %% (24 * 3600)
   hours <- floor(time_difference_1 / 3600)
   time_difference_2 <- time_difference_1 %% 3600
   minutes <- floor(time_difference_2 / 60)
   seconds <- round(time_difference_2 %% 60, 0)
   #output flip time countdown
   output$days <- renderText(paste0(days, " days"))
   output$hours <- renderText(paste0(hours, " hours"))
   output$minutes <- renderText(paste0(minutes, " minutes"))
   output$seconds <- renderText(paste0(seconds, " seconds"))
   # update progress bar
   progress_value <- (time_difference/as.numeric(admin_file[[1,3]]))* 100
   updateProgressBar(session, "progress_bar", value = progress_value)
   # show in numericals
   output$remaining_time <- renderText({
   paste(days,":", hours, ":", minutes, ":", seconds)
   })
  }else{
   output$notice <- renderText({
    paste("Course Registration has been closed. Kindly contact your Department.")
   })
   # update switch input
   updateSwitchInput(session = session,
                     inputId = "switch",
                     value = FALSE)
   disable("register_code")
   shinyjqui::jqui_hide("#flip", effect = "fade")
   output$flip_time <- renderText({
    paste("Please set a later date and time!")
   })
  }
 })
 # close course registration
  observeEvent(input$close, {
   admin_file <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
   # Update the MySQL table
    query <- sprintf("UPDATE administrator_file SET close = '%s' WHERE id = 1",
                     "2000-01-01 00:00:00 EAT")
    dbExecute(con, query)
    # reload table
    # Reload data from MySQL after deleting the row
    updated_data <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
    # Update the Shiny reactiveValues with the updated data
    data$table_data  <- as.data.table(updated_data) 
    # update switch input
    updateSwitchInput(session = session,
                      inputId = "switch",
                      value = FALSE)
    # update fields
    shinyjqui::jqui_hide("#flip", effect = "fade")
    shinyjqui::jqui_hide("#table", effect = "fade")
    disable("register_code")
    output$notice <- renderText({
     paste("Course Registration has been closed. Kindly contact your Department.")
    })
  })
  ####admin control of dates
  observeEvent(input$open, {
   shinyjqui::jqui_show("#flip", effect = "fade")
   shinyjqui::jqui_show("#table", effect = "fade")
   date <- format(input$target_date, "%Y-%m-%d")
   time <- strftime(input$select_time, "%T")
   day <- weekdays(input$target_date)
   date_time <- paste(date,time,"EAT")
   # save the initial difference recorded for progress check
   progress_time <- difftime(date_time, Sys.time(), units = "secs")
   # update switch input
   updateSwitchInput(session = session,
                     inputId = "switch",
                     value = TRUE)
   # Update the MySQL table
   query <- sprintf("UPDATE administrator_file SET close = '%s', seconds = '%s'
                    WHERE id = 1", date_time, progress_time)
   dbExecute(con, query)
   # reload table
   updated_data <- as.data.table(dbGetQuery(con, "SELECT close FROM administrator_file"))
   output$set_time <- renderDataTable({
    datatable(updated_data, escape = FALSE, selection = "none", rownames = FALSE,
              options = list(
               columnDefs = list(
                list(targets = "_all", className = "dt-center")
               ),
               searching = FALSE,         # Hide search box
               paging = FALSE,            # Hide pagination
               ordering = FALSE,          # Disable ordering in all columns
               lengthMenu = list(FALSE),  # Hide entries selection
               language = list(
                info = ""  # Hide the information about entries
               )
              )
    ) |>
     formatStyle(
      columns = names(updated_data),  # Apply to all columns
      textAlign = "center"   # Center alignment
     )
   })
  })
observeEvent(input$register_code, {
 units <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units"))
 #update course titles
 t <- input$register_code
 course_df <- units |> filter(code %in% t) %>%  select(title)
 course_title <- course_df[[1]]
 updateTextInput(
  session = session,
  inputId = "register_unit",
  value = course_title
 )
})

# register a unit
observeEvent(input$register, {
  reg_no <- input$student_reg
  if(input$register_code == ""){
   showToast("info", "Select a Unit!",
             .options = myToastOptions )
  }else if(input$register_code != "" && reg_no != ""){
   req(input$register_unit)
   req(input$register_code)
   #student details
   student_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
   student_data <- student_data |> filter(Serial %in% reg_no)
   student_reg <- student_data$Serial
   student_name <- student_data$Name
   Code <- input$register_code
   Course <- input$register_unit
   Type <- input$type
   student_year <- student_data$Year
   Date <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
   # Insert data into the database
   insert_query <- paste0("INSERT INTO registered_units (Serial, Name, Code,
                           Course, Status, Type,Year)
                           VALUES('",student_reg,"','",student_name,"','",Code,"',
                           '",Course,"','REGISTERED','FIRST ATTEMPT','",student_year,"')")
   DBI::dbExecute(con,insert_query)
   register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))   
   # update field with unregistered units only
   units <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units"))
   student_units <- units |>
    filter(course %in% c("BOTH",student_data$Code)) |>
    filter(year %in% student_year)
   # create a table for a specific student
   student_reg_units <- register_units$data_table |> 
    filter(Serial %in% student_reg) |>
    filter(Year %in% student_year) |>
    arrange(Code)
   list1 <- as.list(student_units$code)
   list2 <- as.list(student_reg_units$Code)
   list <- setdiff(list1, list2)
   if(length(list)>0) {
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = list,
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3)
   )
   }else{
    updateSelectizeInput(
     session = session,
     inputId = "register_code",
     choices = NULL,
     selected = NULL,
     server = TRUE,
     options = list(maxOptions = 3) 
    )
    # update timeline after action
    Date <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
    reg_no <- student_reg
    Users <- student_name
    Actions <- "SUCCESS REGISTRATION"
    Description <- paste("Completed registering year ",student_year," units",sep = "")
    timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Date,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
    DBI::dbSendQuery(con,timeline_query)
   }
   # update timeline after action
   reg_no <- student_reg
   Users <- student_name
   Actions <- "REGISTER UNIT"
   Description <- paste("Registered ",Code," : ", Course," -"," as a unit this Semester",sep = "")
   timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Date,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
   DBI::dbSendQuery(con,timeline_query)
  }else{
   return()
  }
  })
 observeEvent(input$reg, {
  register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  # Registered but with no marks units
  registered_units <- register_units$data_table |> 
   filter(Serial %in% input$reg) |>
   filter(is.na(Score))
  # update student name
  x <- input$reg
  name_df <- registered_units |> 
   filter(Serial %in% x) |>
   select(Name)
  name <- name_df[[1]]
  updateTextInput(
   session = session,
   inputId = "name",
   value = unique(name)
  )
  
  # update code input
  updateSelectizeInput(
   session = session,
   inputId = "code",
   choices = registered_units$Code,
   selected =  registered_units$Code[1],
   server = TRUE,
   options = list(maxOptions = 3) 
  )
 })
  observeEvent(input$code, {
   units <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units"))
   #update course titles
   t <- input$code
   course_df <- units |> filter(code %in% t) %>%  select(title)
   course_title <- course_df[[1]]
   # update course title
   updateTextInput(
    session = session,
    inputId = "course",
    value = course_title
   )
  })
  observeEvent(input$score,{
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
  })
  # Render the DataTable
  output$marks <- renderDataTable({
   # create a table for a specific student
   student_marks <- register_units$data_table |> 
    select(Serial, Name, Code, Course, Score, Grade, Lecturer, Date, Actions)|>
    filter(!is.na(Score))|>
    arrange(desc(Date))
   datatable(student_marks,
             escape = FALSE,
             selection = "none",
             options = list(
              columnDefs = list(
               list(targets = c(2,4,5,6,7,9), orderable = FALSE)#Disable sorting
              )
             )
   )
  })
  # When the Submit button is clicked
  observeEvent(input$submit, {
   #put control to remove NA values
   if (input$reg == "" | input$code == "" | is.na(input$score)
       | input$grade == "") {
    resetLoadingButton("submit")
    showToast(
     "error", "Fill all fields!", .options = myToastOptions
    )
   } else if(input$reg != "" && input$code != "" && !is.na(input$score)
             && input$grade != "") {
    # Extract the unique number between slashes
    number <- str_extract(input$reg, "(?<=/)[0-9]+(?=/)")
    # Get inputs for the fields
    Score <- input$score
    Grade <- input$grade
    Date <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
    Code <- input$code
    Actions <- input$actions
    Lecturer <- sample(users, 1)
    # create the query
    update_query <- sprintf("UPDATE registered_units
    SET Score = %s, Grade = '%s', Date = '%s', Actions = '%s', Lecturer = '%s'
    WHERE Serial LIKE '%%%s%%' AND Code = '%s'",
                            Score, Grade, Date, Actions, Lecturer, number, Code)
    # send query for execution
    DBI::dbSendQuery(con,update_query)
    register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
    #reset all fields to blank
    updateNumericInput(
     session = session,
     inputId = "score",
     value = ""
    )
    # reset loading submit button
    resetLoadingButton("submit")
    showToast(
     "success", "Mark entered", .options = myToastOptions
    )
    # update timeline on mark submission
    reg_no <- input$reg
    Users <- Lecturer
    Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
    Actions <- "UPDATE MARK"
    Description <- paste("Updated student mark for ",Code," : ", input$course, sep = "")
    timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
    DBI::dbSendQuery(con,timeline_query)
   }
  })
  observeEvent(input$approve_button, {
   selectedRow <- as.numeric(strsplit(input$approve_button, "_")[[1]][2])
   search_string <- paste0("approve_ ",selectedRow)
   # Load data from MySQL table into a data.table
   mark_data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
   # Get the serial number
   row <- mark_data[grepl(search_string, mark_data$Actions), ]
   print(row)
   # update timeline on mark submission
   reg_no <- row$Serial
   Users <- row$Name
   Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
   Actions <- "PAID FEES"
   Amount <- sample(seq(1000, 10000, by = 5)*2, 1)
   Description <- paste("Paid $", Amount, " Tuition fee for SEM 1 2024", sep = "")
   timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
   DBI::dbSendQuery(con,timeline_query)
  })
 session$onSessionEnded(function() {
  dbDisconnect(con, add = TRUE)  # Disconnect when the session ends
   })
}
