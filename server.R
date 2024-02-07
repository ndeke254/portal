server <- function(input, output, session){
 shinyjs::runjs('
    var container = $(".moving-sentence");
    container.css("left", "100%");

    function moveSentence() {
      container.animate({left: "-100%"}, 30000, "linear", function() {
        container.css("left", "100%");
        moveSentence();
      });
    }

    moveSentence();
  ')
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
  if(nrow(data$table_data) == 0){
   #create an ideal button
   ideal <- '<button id="deletereg_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;deletereg_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Delete"> <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="editreg_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;editreg_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Edit"> <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
    <button id="year_ 1" type="button" class="btn btn-default action-button" style="color: black;"   onclick="Shiny.onInputChange( &quot;year_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Promote"> <i class="fas fa-ranking-star" role="presentation" aria-label="ranking-star icon"></i></button>
    <button id="timeline_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;timeline_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Timeline"> <i class="fas fa-clock-rotate-left" role="presentation" aria-label="clock-rotate-left icon"></i></button>
    <button id="cash_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;cash_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Payments"> <i class="fas fa-sack-dollar" role="presentation" aria-label="sack-dollar- icon"></i></button>
    <button id="Edited" type="button" class="btn btn-default action-button" style="background-color: #e9ecef; " disabled data-title="Edited">NO</button>'
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "Buttons",
    value = ideal
   )
  
  }else{
   button <- data$table_data |>
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
  }
  if (nrow(marks_data) == 0 ){
   #marks table buttons
   ideal_marks <- '<button id="delete_ 1" type="button" class="btn btn-default action-button" style="color: red;" onclick="Shiny.onInputChange( &quot;delete_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Delete"> <i class="fas fa-trash" role="presentation" aria-label="trash icon"></i></button>
    <button id="edit_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;edit_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Edit"> <i class="fas fa-file-pen" role="presentation" aria-label="file-pen icon"></i></button>
    <button id="approve_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;approve_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Approve"> <i class="fas fa-circle-check" role="presentation" aria-label="circle-check icon"></i></button>
    <button id="reject_ 1" type="button" class="btn btn-default action-button" onclick="Shiny.onInputChange( &quot;reject_button&quot; , this.id, {priority: &quot;event&quot;})" data-title="Reject"> <i class="fas fa-circle-xmark" role="presentation" aria-label="circle-xmark icon"></i></button>
    <button id="Status" type="button" class="btn btn-default action-button" style="background-color: #e9ecef; " disabled data-title="Status">RELEASED</button>'
    updateTextInput(
    session = session,
    inputId = "actions",
    value = ideal_marks
   )
  }else{
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
   new2_marks <- gsub("PASSED","RELEASED",new1_marks)  
   new3_marks <- gsub("FAILED","RELEASED",new2_marks) 
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "actions",
    value =  new3_marks
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
   choices =  unique(data$table_data$Serial),
   selected = "",
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
   selected = data$table_data$Serial[1],
   server = TRUE,
   options = list(maxOptions = 3)
  )
  #update choices
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   choices =  unique(data$table_data$Serial),
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
 # set condition
 units <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units"))
 student_units <- units |>
  filter(course %in% c("BOTH",filtered_df$Code)) |>
  filter(year %in% current_year)  
 student_reg_units <- register_units$data_table |> 
  filter(Serial %in% filtered_df$Serial) |>
  filter(Year %in% filtered_df$Year) |>
  arrange(Code)
 list1 <- as.list(student_units$code)
 list2 <- as.list(student_reg_units$Code)
 list <- setdiff(list1, list2)
 if(current_year < 4 & all(student_reg_units$Status == "PASSED") & length(list)== 0) {
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
 }else if(current_year == 4 & all(student_reg_units$Status == "PASSED")& length(list)== 0){
  showToast(
   "error", "Horray! Waiting Graduation!",
   .options = myToastOptions
  )
  removeModal()
 }else if(current_year < 4 & all(student_reg_units$Status != "PASSED") | length(list)!= 0){
  showToast("error",
            "All Year Units NOT passed!",
            .options = myToastOptions)
  removeModal()
  
 }else{
  removeModal()
  showToast("error",
            "Not Qualified!",
            .options = myToastOptions )
 }
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
      }else if(action_icon %in% "DELETE MARK"){
       set_icon <- "ban"
      }else if(action_icon %in% "EDIT MARK"){
       set_icon <- "pen-to-square"
      }else if(action_icon %in% "APPROVE MARK"){
       set_icon <- "check"
      }else if(action_icon %in% "PRINT TRANSCRIPT"){
       set_icon <- "file-pdf"
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
  shinyjs::hide("download") 
  #student details
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  student_data <- data |>
   filter(Serial %in%student_reg)
  student_year <- student_data$Year 
  student_course <- student_data$Code 
  #student name
  output$student_name <- renderText({
   paste(student_data$Serial, student_data$Name)
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
  list1 <- as.list(student_units$code)
  #already registered units
  register_units$data_table  <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  # create a table for a specific student
  student_reg_units <- register_units$data_table |> 
   filter(Serial %in% student_reg) |>
   filter(Year %in% student_year) |>
   arrange(Code)
  # registered current year units
  output$registered_units <- DT::renderDataTable(
   datatable(student_reg_units, escape = FALSE, selection = "none",
             options = list(
               columnDefs = list(
                list(targets = c(1,2,5,6,7,10,11,12), visible = FALSE)),# column you want to hide
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
  list2 <- as.list(student_reg_units$Code)
  #Remaining units
  list <- setdiff(list1, list2)
  # Get the students registered for a single unit
  registered_students <- as.list(register_units$data_table$Serial)
  if(student_reg %in% registered_students){
   #already passed units
   n_passed <- student_reg_units |> 
    filter(Status %in% "PASSED") |>
    nrow()
   per <- n_passed/length(list1)
   update_progress("bar",per)
  }
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
   showToast("info", "All Units Registered!",
             .options = myToastOptions)
   supp_code <- student_reg_units |> filter(Status %in% "FAILED")
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = supp_code$Code,
    server = TRUE
   )
   
  }
  updateSelectizeInput(
   session = session,
   inputId = "type",
   selected = ""
  )
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
  t_b_b <- register_units$data_table |> 
   filter(Status == "PASSED" | Status == "FAILED")
  #first year
  chosen_units1 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 1)
  t_b_1 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units1$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total1 <-  nrow(t_b_1)
  total1 <- sum(t_b_1$Score)
  average1 <- round(total1/n_total1,2)
  #second year
  chosen_units2 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 2)
  t_b_2 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units2$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total2 <-  nrow(t_b_2)
  total2 <- sum(t_b_2$Score)
  average2 <- round(total2/n_total2,2)
  #third year
  chosen_units3 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 3)
  t_b_3 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units3$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total3 <-  nrow(t_b_3)
  total3 <- sum(t_b_3$Score)
  average3 <- round(total3/n_total3,2)
  #fourth year
  chosen_units4 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 4)
  t_b_4 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units4$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total4 <-  nrow(t_b_4)
  total4 <- sum(t_b_4$Score)
  average4 <- round(total4/n_total4,2)
  #final average
  student_f_units <- units |>
   filter(course %in% c("BOTH",student_course))
  t_b_f <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(t_b_1$Code, t_b_2$Code, t_b_3$Code, t_b_4$Code)) |>
   select(Serial, Code, Course, Grade, Score)
  n_total <- nrow(t_b_f)
  total <- sum(t_b_f$Score)
  average <- round(total/n_total,2)
  
  #To find final averages
  if(student_year == 1){
   #finale data
   data <- data.frame(
    Year = "First",
    Units = n_total1,
    Average = average1
   )
   t_b_1 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 1) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_1_marks <- DT::renderDataTable(
    datatable(t_b_1, escape = FALSE, selection = "none",
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
   hide("year_2_marks")
   hide("year_3_marks") 
   hide("year_4_marks")
   
  }else if(student_year == 2){
   #finale data
   data <- data.frame(
    Year = c("First","Second"),
    Units = c(n_total1,n_total2),
    Average = c(average1,average2)
   )
   t_b_1 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 1) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_1_marks <- DT::renderDataTable(
    datatable(t_b_1, escape = FALSE, selection = "none",
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
   t_b_2 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 2) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_2_marks <- DT::renderDataTable(
    datatable(t_b_2, escape = FALSE, selection = "none",
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
   hide("year_3_marks") 
   hide("year_4_marks")
  }else if(student_year == 3){
   #finale data
   data <- data.frame(
    Year = c("First","Second","Third"),
    Units = c(n_total1,n_total2,n_total3),
    Average = c(average1,average2,average3)
   )
   t_b_1 <- t_b_b |> filter(Seial %in% input$student_reg & Year %in% 1) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_1_marks <- DT::renderDataTable(
    datatable(t_b_1, escape = FALSE, selection = "none",
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
   t_b_2 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 2) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_2_marks <- DT::renderDataTable(
    datatable(t_b_2, escape = FALSE, selection = "none",
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
   t_b_3 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 3) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_3_marks <- DT::renderDataTable(
    datatable(t_b_3, escape = FALSE, selection = "none",
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
   hide("year_4_marks") 
  }else{
   #finale data
   data <- data.frame(
    Year = c("First","Second","Third","Fourth"),
    Units = c(n_total1,n_total2,n_total3,n_total4),
    Average = c(average1,average2,average3,average4)
   )
   t_b_1 <- t_b_b |> filter(Seial %in% input$student_reg & Year %in% 1) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_1_marks <- DT::renderDataTable(
    datatable(t_b_1, escape = FALSE, selection = "none",
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
   t_b_2 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 2) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_2_marks <- DT::renderDataTable(
    datatable(t_b_2, escape = FALSE, selection = "none",
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
   t_b_3 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 3) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_3_marks <- DT::renderDataTable(
    datatable(t_b_3, escape = FALSE, selection = "none",
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
   t_b_4 <- t_b_b |> filter(Serial %in% input$student_reg & Year %in% 4) |>
    select(Code, Course, Grade) |>
    arrange(Code)
   output$year_4_marks <- DT::renderDataTable(
    datatable(t_b_4, escape = FALSE, selection = "none",
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
  }
  #transcript tables
  table_data1 <- t_b_1 |> select(Code, Course, Grade) |> arrange(Code) |>
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average1))
  table_data2 <- t_b_2 |> select(Code, Course, Grade) |> arrange(Code) |>
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average2))
  table_data3 <- t_b_3 |> select(Code, Course, Grade) |> arrange(Code) |> 
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average3))
  table_data4 <- t_b_4 |> select(Code, Course, Grade) |> arrange(Code) |>
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average4))
  #allow transcripts download
  if(nrow(t_b_1) > 0){
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
   a >= 1 ~ "PASS",
   TRUE ~ ""
  )
  #availability conditions
  if(student_reg %in% registered_students){
   # missing units
   r_units <- as.list(t_b_f$Code)
   m_units <- setdiff(list1, r_units)
   # failed units
   file_2 <- register_units$data_table |> 
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
    data |> 
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
  student_reg <- input$student_reg
  #student details
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  student_data <- data |>
   filter(Serial %in%student_reg)
  student_year <- student_data$Year 
  student_course <- student_data$Code 
  #approved marks
  t_b_b <- register_units$data_table |> 
   filter(Status == "PASSED" | Status == "FAILED")
  #first year
  chosen_units1 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 1)
  t_b_1 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units1$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total1 <-  nrow(t_b_1)
  total1 <- sum(t_b_1$Score)
  average1 <- round(total1/n_total1,2)
  #second year
  chosen_units2 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 2)
  t_b_2 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units2$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total2 <-  nrow(t_b_2)
  total2 <- sum(t_b_2$Score)
  average2 <- round(total2/n_total2,2)
  #third year
  chosen_units3 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 3)
  t_b_3 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units3$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total3 <-  nrow(t_b_3)
  total3 <- sum(t_b_3$Score)
  average3 <- round(total3/n_total3,2)
  #fourth year
  chosen_units4 <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% 4)
  t_b_4 <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(chosen_units4$code)) |>
   select(Serial, Code, Course, Grade, Score)
  # Calculate stats
  n_total4 <-  nrow(t_b_4)
  total4 <- sum(t_b_4$Score)
  average4 <- round(total4/n_total4,2)
  #final average
  student_f_units <- units |>
   filter(course %in% c("BOTH",student_course))
  t_b_f <- t_b_b |> filter(Serial == input$student_reg & Code %in% c(t_b_1$Code, t_b_2$Code, t_b_3$Code, t_b_4$Code)) |>
   select(Serial, Code, Course, Grade, Score)
  n_total <- nrow(t_b_f)
  total <- sum(t_b_f$Score)
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
  table_data1 <- t_b_1 |> select(Code, Course, Grade) |> arrange(Code) |>
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average1))
  table_data2 <- t_b_2 |> select(Code, Course, Grade) |> arrange(Code) |>
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average2))
  table_data3 <- t_b_3 |> select(Code, Course, Grade) |> arrange(Code) |> 
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average3))
  table_data4 <- t_b_4 |> select(Code, Course, Grade) |> arrange(Code) |>
   add_row(Code = "", Course = "AVERAGE SCORE", Grade = as.character(average4))
  #parameters
  r_name <- student_data$Name
  reg_no <- input$student_reg
  prog <- student_data$Course
  date <- student_data$Date
  id <- student_data$ID
  stamp_datee <- format(Sys.Date(),  format = "%d %b %y")
  comment <- comment
  reg_No <- gsub("/", "", input$student_reg)
  #comment
  a <- average
  class <- case_when(
   a >= 70 ~ "FIRST CLASS HONOURS",
   a >= 60 ~ "SECOND CLASS-UPPER",
   a >= 50 ~ "SECOND CLASS-LOWER",
   a >= 1 ~ "PASS",
   TRUE ~ ""
  )
  units <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units"))
  student_units <- units |>
   filter(course %in% c("BOTH",student_course)) |>
   filter(year %in% student_year)
  list1 <- as.list(student_units$code)
  # missing units
  r_units <- as.list(t_b_f$Code)
  m_units <- setdiff(list1, r_units)
  # failed units
  file_2 <- register_units$data_table |> 
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
  #update timeline
  Users <- r_name
  reg_no <- reg_no
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Actions <- "PRINT TRANSCRIPT"
  Description <- paste("Printed transcript: ",reg_no,"_transcript.pdf") 
  #write changes
  timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
  DBI::dbSendQuery(con,timeline_query)
  #convert HTML to a pdf file
  pdf <- chrome_print(input = render_result,
                      output = paste(reg_no,"_transcript.pdf"))
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
 # create a supplementry registration
 observeEvent(input$type, {
  student_reg <- input$student_reg 
  #student details
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  student_data <- data |>
   filter(Serial %in%student_reg)
  student_year <- student_data$Year 
  student_course <- student_data$Code 
  if(input$type %in% "SUPPLEMENTARY"){
   #already registered units
   register_units$data_table  <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
   # create a table for a specific student
   student_reg_units <- register_units$data_table |> 
    filter(Serial %in% student_reg) |>
    filter(Year %in% student_year) |>
    arrange(Code)
   # failed units
   failed_units <- student_reg_units |> 
    filter(Status %in% "FAILED" & Type != "SUPPLEMENTARY")
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = failed_units$Code,
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3)
   )
  }else{
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
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = list,
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3) 
   )
  }
 })
  # create registration deadline
  observe({
  admin_file <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
  status <- admin_file[[2,3]]
  get <- admin_file[[2,2]]
  if(status == 1){
   updateTextAreaInput(
    session = session,
    inputId = "post_writeup",
    value = get
   )
   output$announcement <- renderText({
   paste0("NOTICE: ", get)
   })
   disable("post_writeup")
  }else{
   hide("announcement")
   enable("post_writeup")
  }
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
   output$days <- renderText(days)
   output$hours <- renderText(hours)
   output$minutes <- renderText(minutes)
   output$seconds <- renderText(seconds)
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
   enable("select_time")
   enable("target_date")
   admin_file <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
   # Update the MySQL table
    query <- sprintf("UPDATE administrator_file SET close = '%s' WHERE id = 1",
                     "2000-01-01 00:00:00 EAT")
    dbExecute(con, query)
    # reload table
    # Reload data from MySQL after deleting the row
    updated_data <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
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
   disable("select_time")
   disable("target_date")
   
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
  }else if(input$register_code != "" && reg_no != "" ){
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
   if(input$type != "SUPPLEMENTARY"){
   # Insert data into the database
   insert_query <- paste0("INSERT INTO registered_units (Serial, Name, Code,
                           Course, Status, Type,Year)
                           VALUES('",student_reg,"','",student_name,"','",Code,"',
                           '",Course,"','REGISTERED','FIRST ATTEMPT','",student_year,"')")
   DBI::dbExecute(con,insert_query)
   }else{
    Status = "REGISTERED"
    insert_query <- sprintf("UPDATE registered_units 
                            SET Type = '%s', Status = '%s'
                            WHERE Serial = '%s' AND Code = '%s'",
                            Type, Status, student_reg, Code )
    DBI::dbSendQuery(con,insert_query)
    updateSelectizeInput(
     session = session,
     inputId = "type",
     selected = ""
    )
   }
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
  resetLoadingButton("register")
  
  })
 observeEvent(input$reg, {
  updateTextInput(
   session = session,
   inputId = "grade",
   value = ""
  )
  if(input$id == "0"){
  register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  # Registered but with no marks units
  registered_units <- register_units$data_table |> 
   filter(Serial %in% input$reg) |>
   filter(is.na(Score) | Status == "REGISTERED")
  # update student name
  x <- input$reg
  name_df <- registered_units |> 
   filter(Serial %in% x) |>
   select(c("Name","Code"))
  updateTextInput(
   session = session,
   inputId = "name",
   value = unique(name_df$Name)
  )
  
  # update code input
  updateSelectizeInput(
   session = session,
   inputId = "code",
   choices = name_df$Code,
   selected =  name_df$Code[1],
   server = TRUE,
   options = list(maxOptions = 3) 
  )
  }
 })
  observeEvent(input$code, {
   if(input$id == "0"){
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
   }
   data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
   # check exam type
   exam_type <- data_table |> 
    filter(Serial %in% input$reg) |>
    filter(Code %in% input$code)|>
    select(Type)
   if(exam_type %in% "FIRST ATTEMPT" | input$code == ""){
    updateTextInput(session, "grade", label = "Grade")
    updateNumericInput(session, "score", "Score")
   }else{
    updateTextInput(session, "grade", label = "Grade *")
    updateNumericInput(session, "score", "Score *")
   }
  })
  observeEvent(input$score,{
   data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
   # check exam type
   exam_type <- data_table |> 
    filter(Serial %in% input$reg) |>
    filter(Code %in% input$code)|>
    select(Type)
   x <- input$score
   if(exam_type %in% "FIRST ATTEMPT" | input$code == ""){
    #set a grade on scale
    grade <- case_when(
     x >= 70 ~ "A",
     x >= 60 ~ "B",
     x >= 50 ~ "C",
     x >= 40 ~ " D",
     x >= 1 ~ "E",
     TRUE ~ ""
    )
    updateTextInput(
     session = session,
     inputId = "grade",
     value = grade
    )
    if(x > 99 |is.na(x)| x < 1 | nchar(x) > 2) {
     updateNumericInput(
      session = session,
      inputId = "score",
      value = ""
     )
    }else {
     return()
    }
   }else{
    if(x > 50 | is.na(x)| x < 40 | nchar(x) > 2) {
     updateNumericInput(
      session = session,
      inputId = "score",
      value = ""
     )
     #set a grade on scale
     z <- input$score
     updateTextInput(
      session = session,
      inputId = "grade",
      value = "D*"
     )
    }else{
     return()
    }
  }
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
   if(input$id == "0"){
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
    Status <- "RELEASED"
    # create the query
    update_query <- sprintf("UPDATE registered_units
    SET Score = %s, Grade = '%s', Date = '%s', Actions = '%s', Lecturer = '%s', Status = '%s'
    WHERE Serial LIKE '%%%s%%' AND Code = '%s'",
                            Score, Grade, Date, Actions, Lecturer, Status, number, Code)
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
   }
  })
  observeEvent(input$cash_button, {
   selectedRow <- as.numeric(strsplit(input$cash_button, "_")[[1]][2])
   search_string <- paste0("cash_ ",selectedRow)
   # Load data from MySQL table into a data.table
   details_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
   # Get the serial number
   row <- details_data[grepl(search_string, details_data$Actions), ]
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
   showToast(
    "success", "Payment Accepted", .options = myToastOptions
   )
  })
  
  # customize mark buttons functionality
  #Delete Button
  observeEvent(input$delete_button, {
   confirm_modal_dialog(approve = FALSE, edit = FALSE, delete = TRUE,
                        editreg = FALSE, promote = FALSE, deletereg = FALSE)
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
 #confirm deletion of a row record
 observeEvent(input$confirm_delete, {
  selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
  search_string_1 <- paste0("delete_ ",selectedRow)
  search_string <- paste0("'%", search_string_1, "%'")
  # Load data from MySQL table into a data.table
  marks_data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  # Get the serial number
  row <- marks_data[grepl(search_string_1, marks_data$Actions), ]
  # update timeline on deletion
  reg_no <- row$Serial
  Code <- row$Code
  Course <- row$Course
  Users <- sample(users, 1)
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Actions <- "DELETE MARK"
  Description <- paste("Deleted marks for ", Code, " : ", Course, sep = "")
  timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
  DBI::dbSendQuery(con,timeline_query)
  # Query to remove a row
  marks_delete_query <- paste0("DELETE FROM registered_units WHERE
                           Actions LIKE", search_string 
                               )
 
  DBI::dbSendQuery(con, marks_delete_query)
  # Reload data from MySQL after deleting the row
  updated_data <- dbGetQuery(con, "SELECT * FROM registered_units")
  # Update the Shiny reactiveValues with the updated data
  register_units$data_table <- as.data.table(updated_data)
  showToast("success",
            "Mark Deleted!",
            .options = myToastOptions )
  removeModal()
 })
 
# edit Button on marks
 observeEvent(input$edit_button, {
  confirm_modal_dialog(approve = FALSE, edit = TRUE, delete = FALSE,
                       editreg = FALSE, promote = FALSE, deletereg = FALSE)
 })
 observeEvent(input$confirm_edit,{
  updateTextInput(session, "id", value = "1")
  # Load data from MySQL table into a data.table
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  units_table <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units"))
  selectedRow <- as.numeric(strsplit(input$edit_button, "_")[[1]][2])
  search_string <- paste0("edit_ ",selectedRow)
  row <- data[grepl(search_string, data$Actions), ]
  updateSelectInput(session, "code", choices = units_table$code,
                    selected = row$Code)
  updateSelectInput(session, "reg", selected = row$Serial)
  updateTextInput(session, "name", value = row$Name)
  updateNumericInput(session, "course", value = row$Course)
  updateTextInput(session, "score", value = row$Score)
  updateTextInput(session, "grade", value = row$Grade)
  updateTextInput(session, "actions", value = row$Actions)
  disable("code")
  disable("reg")
  showNotification("Edit the score only!", duration = 10, type = "message")
  removeModal()
 })
 # confirm edit now
 observeEvent(input$submit, {
  if(input$id == "1"){
 # collect all entries
  new_score <- input$score
  new_grade <- input$grade
  action <- input$actions
 # Load data from MySQL table into a data.table
 selected_data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
 selected_data <- selected_data |>
  filter(Actions %in% action)
 old_mark <- selected_data$Score
 old_grade <- selected_data$Grade
 # create the query
 update_query <- sprintf("UPDATE registered_units SET
                           Score = %s,
                           Grade = '%s' 
                           WHERE Actions = '%s'", 
                           new_score, new_grade, action)
 DBI::dbSendQuery(con,update_query)
 # Reload data
 register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
 resetLoadingButton("submit")
 showToast("success",
           "Student Marks Edited!",
           .options = myToastOptions )
 removeModal()
 # create a timeline for the marks changes
  marks_change = paste0(old_mark,"% (",old_grade,") → ", new_score, "% (", new_grade,")")
 #update timeline
 Users <- sample(users, 1)
 reg_no <- selected_data$Serial
 Code <- selected_data$Code
 Course <- selected_data$Course
 Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
 Actions <- "EDIT MARK"
 Description <- paste("Edited Marks ",Code," : ", Course,"i.e.",marks_change )
 #write changes
 timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
 DBI::dbSendQuery(con,timeline_query)
 #reset inputs
 enable("code")
 enable("reg")
 updateTextInput(session, "id", value = "0")
 updateTextInput(session, "score", value = "")
 updateSelectizeInput(session, "code", choices = "", selected ="")
  }
})
 # admin approve marks for the student 
 observeEvent(input$approve_button, {
  # Load data from MySQL table into a data.table
  selected_data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  selectedRow <- as.numeric(strsplit(input$approve_button, "_")[[1]][2])
  search_string <- paste0("approve_ ",selectedRow)
  row <- selected_data[grepl(search_string, selected_data$Actions), ]
  action <- row$Actions
  score <- row$Score
  if(score > 40){
   Status <- "PASSED"
  }else if(score < 40){
   Status <- "FAILED"
  }else{
   return()
  }
  # change color for edited rows
  button <- action
  new_button <- gsub("background-color: #e9ecef;", 
                     "background-color: #0025ff8f;", button)
  edited_buttons_1 <- gsub("RELEASED",Status, new_button)
  edited_buttons <- gsub("FAILED",Status, edited_buttons_1)
  # create the query
  update_query <- sprintf("UPDATE registered_units SET
                           Status = '%s',
                           Actions = '%s' 
                           WHERE Actions = '%s'", 
                          Status, edited_buttons, action)
  DBI::dbSendQuery(con,update_query)
  # Reload data
  register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  showToast("success",
            "Student Marks Approved!",
            .options = myToastOptions )
  #update timeline
  Users <- "Administrator"
  reg_no <- row$Serial
  Code <- row$Code
  Course <- row$Course
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Actions <- "APPROVE MARK"
  Description <- paste("Approved Marks ",Code," : ", Course)
  #write changes
  timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
  DBI::dbSendQuery(con,timeline_query)
 })
 observe({
 #output value boxes
 current_year <- lubridate::year(Sys.Date())
 current_date <- format(Sys.Date(), "%d-%m-%Y")
 previous_year <- current_year - 1
 previous_day1 <- Sys.Date() - 1
 previous_day <- format(previous_day1, "%d-%m-%Y")
 #year
 n_c <- data$table_data [grepl(current_year, data$table_data$Date), ] |>
  nrow()
 n_p <- data$table_data[grepl(previous_year, data$table_data$Date), ] |>
  nrow()
 per_n <- (((n_c-n_p)/n_p)*100) |> round(2)
 #day
 c_d <- register_units$data_table[grepl(current_date, register_units$data_table$Date), ] |>
  nrow()
 p_d <- register_units$data_table [grepl(previous_day, register_units$data_table$Date), ] |>
  nrow()
 per_d <- (((c_d-p_d)/p_d)*100) |> round(2)
 ####
 
 output$registered_no <- renderValueBox({
  value <- n_c |>
   prettyNum(big.mark =',', scientific = FALSE)
  color <- my_color(per_n)
  arrow <- my_symbol(per_n)
  my_valuebox(value,  
              title = "NO.OF STUDENTS",
              subtitle = tagList(HTML(arrow), paste0(abs(per_n),"% since last year")),
              icon = icon("pencil"),
              color = color
  )
 })
 ###
 output$available_marks <- renderValueBox({
  value <- c_d |>
   prettyNum(big.mark =',', scientific = FALSE)
  color <- my_color(per_d)
  arrow <- my_symbol(per_d)
  my_valuebox(value,  
              title = "RELEASED RESULTS",
              subtitle = tagList(HTML(arrow), paste0(abs(per_d),"% since yesterday")),
              icon = icon("pencil"),
              color = color
  )
 })
 miss_today <- register_units$data_table [grepl("REGISTERED", register_units$data_table$Status), ] |>
  filter(Date %in% current_date) |> nrow()
 miss_yester <- register_units$data_table[grepl("REGISTERED", register_units$data_table $Status), ] |>
  filter(Date %in% previous_day) |> nrow()
 miss_per <- (((miss_today-miss_yester)/miss_yester)*100) |> round(2)
 output$missing_marks <- renderValueBox({
  value = miss_today |>
   prettyNum(big.mark =',', scientific = FALSE)
  color <- my_color(miss_per)
  arrow <- my_symbol(miss_per)
  my_valuebox(value,  
              title = "MISSING MARKS",
              subtitle = tagList(HTML(arrow), paste0(abs(miss_per),"% since yesterday")),
              icon = icon("pencil"),
              color = color
  )
 })
 ####
 fail_today <- register_units$data_table [grepl("FAILED", register_units$data_table $Status), ] |>
  filter(Date %in% current_date) |> nrow()
 fail_yester <- register_units$data_table [grepl("FAILED", register_units$data_table $Status), ] |>
  filter(Date %in% previous_day) |> nrow()
 fail_per <- (((fail_today-fail_yester)/fail_yester)*100) |> round(2)
 output$failed_marks <- renderValueBox({
  value = fail_today |>
   prettyNum(big.mark =',', scientific = FALSE)
  color <- my_color(fail_per)
  arrow <- my_symbol(fail_per)
  my_valuebox(value,  
              title = "FAILED MARKS",
              subtitle = tagList(HTML(arrow), paste0(abs(fail_per),"% since yesterday")),
              icon = icon("pencil"),
              color = color
  )
 })
 ####
 prom_ready <- data$table_data |> 
  filter(Year %in% c(1,2,3))
 prom_current <-   prom_ready [grepl(current_date,   prom_ready $Date), ] |> nrow()
 prom_previous <-   prom_ready [grepl(previous_day,   prom_ready $Date), ] |> nrow()
 prom_per <- (((prom_current-prom_previous)/prom_previous)*100) |> round(2)
 output$waiting_promotion <- renderValueBox({
  value = prom_current |>
   prettyNum(big.mark =',', scientific = FALSE)
  color <- my_color(prom_per)
  arrow <- my_symbol(prom_per)
  my_valuebox(value,  
              title = "PENDING PROMOTIONS",
              subtitle = tagList(HTML(arrow), paste0(abs(prom_per),"% since yesterday")),
              icon = icon("pencil"),
              color = color
  )
 })
 ####
 ready_grad <- data$table_data |> 
  filter(Year %in% 4)
 grad_current1 <- ready_grad |> 
  filter(Year %in% 4) 
 grad_current <- grad_current1[grepl(current_year, grad_current1$Date), ] |> nrow()
 grad_previous <- grad_current1[grepl(previous_year, grad_current1$Date), ] |> nrow()
 grad_per <- (((grad_current-grad_previous)/grad_previous)*100) |> round(2)
 output$grad_students <- renderValueBox({
  value = grad_current |>
   prettyNum(big.mark =',', scientific = FALSE)
  color <- my_color(grad_per)
  arrow <- my_symbol(grad_per)
  my_valuebox(value,  
              title = "WAITING GRADUATION",
              subtitle = tagList(HTML(arrow), paste0(abs(grad_per),"% since last year")),
              icon = icon("pencil"),
              color = color
  )
 })
 observeEvent(input$post, {
  disable("post_writeup")
  shinyjs::show("announcement")
  # Update the MySQL table
  query <- sprintf("UPDATE administrator_file SET close = '%s', seconds = '%s'
                    WHERE id = 2", input$post_writeup, 1)
  dbExecute(con, query)
  admin_file <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
  get <- admin_file[[2,2]]
  output$announcement <- renderText({
   paste0("NOTICE: ", get)
  })
 })
 observeEvent(input$unpost, {
  enable("post_writeup")
  hide("announcement")
  updateTextAreaInput(
   session = session,
   inputId = "post_writeup",
   value = ""
   )
  # Update the MySQL table
  query <- sprintf("UPDATE administrator_file SET close = '%s', seconds = '%s'
                    WHERE id = 2", "", 0)
  dbExecute(con, query)
 })
 })
 session$onSessionEnded(function() {
  dbDisconnect(con, add = TRUE)  # Disconnect when the session ends
   })
}
