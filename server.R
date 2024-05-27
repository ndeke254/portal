server <- function(input, output, session) {

# customize moving sentence with js to show notice/announcement
 shinyjs::runjs('
var container = $(".moving-sentence");
container.css("left", "100%");
function moveSentence() {
container.animate({left: "-100%"}, 30000, "linear", function() {
container.css("left", "100%");
moveSentence();
});
}
moveSentence();'
 )

 #import data
 
 # Create a connection to the MySQL database
 con <- dbConnect(
  RMySQL::MySQL(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
 )
 
 # Load data from MySQL into a reactiveValues object
 data <- reactiveValues(table_data = NULL)
 register_units <- reactiveValues(data_table = NULL)
 token_data <- reactiveValues(data_table = NULL)
 
 # Load data from MySQL table into a data.table
 data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
 register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
 units <- as.data.table(dbGetQuery(con, "SELECT * FROM course_units")) 

  ### ADMINISTRATOR REGISTRATION
 
 # populate required value entries
 observeEvent(input$Code, {
  data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  code <- input$Code
  
  course_lookup <- c("X74" = "Economics",
                     "X75" = "Economics and Statistics")
  
  course <- reactive({
   course_lookup <- as.list(course_lookup)
   course_name <- course_lookup[[code]]
   return(course_name)
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
  
   num <- sample(setdiff(1000:9999, usedNumbers()), 1)
   
   # Update the generatedNumber reactive value
   generatedNumber(num)
   reg <- paste0(code, "/", num, "/", year)
   updateTextInput(session, "Reg", value = reg)
  }
 }
 
 # Load data from MySQL table into a data.table
 observe({
  marks_data <- register_units$data_table[!is.na(Score), ]
  #create an ideal button
  if(nrow(data$table_data) == 0){
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
   button <- data$table_data[, .(Date, Actions)][order(-Date)]
   button  <- button[[1,2]]

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
   new2 <- gsub("YES", "NO", new1) 
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
   
   # marks table update
   button_marks <- register_units$data_table[, .(Date, Actions)][order(-Date)]
   button_marks <- button_marks[[1,2]]
   
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
   new4_marks <- gsub("REJECTED","RELEASED",new3_marks)
   
   #update the buttons
   updateTextInput(
    session = session,
    inputId = "actions",
    value =  new4_marks
   )
  }

  # update choices
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   choices =  data$table_data[, Serial],
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  )
    # update choices
  updateSelectizeInput(
   session = session,
   inputId = "student_reg",
   choices = data$table_data[, Serial],
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
  data <- data$table_data[order(-Date)]
  datatable(data,
            escape = FALSE,
            selection = "none",
            options = list(
             columnDefs = list(
              list(targets = c(5,6), visible = FALSE),# column you want to hide
              list(targets = c(1, 3, 4, 5, 6, 9), orderable = FALSE)#Disable sorting
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
  
  # Check if the entered ID already exists in the 
  if(input$ID %in%data$table_data$ID) {
   showToast("error",
             "ID Number Exists!",
             .options = myToastOptions )
   resetLoadingButton("registerButton")
   return()  # Stop execution if duplicate ID
  }
  
  # entered inputs data
  name <- input$Name
  ID <- input$ID
  gender <- input$Gender
  Reg_No <- input$Reg
  Code <- input$Code
  Course <- input$Course
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Years <- input$Year
  Complete <- "0%"
  Buttons <- input$Buttons
  Users <- "Administrator"
  Actions <- "INSERT NEW"
  Description <- paste("Added ",Reg_No, " : ", name, " as a new Student", sep = "")
     # Insert data into the database
  insert_query_1 <- paste0("INSERT INTO student_details
                  VALUES('",name,"',",ID,",'",gender,"','",Reg_No,"','",Code,"',
                                    '",Course,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),
                                    ",Years,",'",Complete,"','",Buttons,"')")
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
  
  # Update the preview output
  output$previewImage <- renderImage({
   list(src = "")
  }, deleteFile = FALSE)
  new_reg()
  
  # update choices
  updateSelectizeInput(
   session = session,
   inputId = "student_reg",
   choices = data$table_data[, Serial],
   selected = "",
   server = TRUE,
   options = list(maxOptions = 3)
  ) 
  # update choices
  updateSelectizeInput(
   session = session,
   inputId = "reg",
   choices =  data$table_data[, Serial],
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
 
 # edit a student registration file
 observeEvent(input$confirm_editreg,{ 
  
  # Load data from MySQL table into a data.table
  selectedRow <- as.numeric(strsplit(input$editreg_button, "_")[[1]][2])
  search_string <- paste0("editreg_ ",selectedRow)
  search_string <- paste0("\\b", search_string, "\\b")
  
  row <-  data$table_data[Actions %like% search_string, ]
  updateSelectInput(session, "edit_code", selected = row[, Code])
  updateTextInput(session, "edit_name", value = row[, Name])
  updateSelectInput(session, "edit_gender", selected = row[, Gender])
  updateNumericInput(session, "edit_ID", value = row[, ID])
  updateTextInput(session, "edit_course", value = row[, Course])
  updateTextInput(session, "edit_buttons", value = row[, Actions])
  
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
  course_lookup <- c("X74" = "Economics",
                     "X75" = "Economics and Statistics")
  
  course <- reactive({
   course_name <- course_lookup[[code]]
   return(course_name)
  })
  updateTextInput("edit_course", session = session,  value = course()
  )
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
  edited_buttons <- gsub("NO","YES", new_button)
  
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
  selected_data <- selected_data[Serial %like% number, ]
  Name <- selected_data[, Name]
  ID <- selected_data[, ID]
  Gender <- selected_data[, Gender]
  Serial <- selected_data[, Serial]
  Code <- selected_data[, Code]
  Course <- selected_data[, Course]
  
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
  
  DBI::dbSendQuery(con, update_query)
  
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
  row <- delete_data[Actions %like% search_string_1, ]
  Serial <- row[, Serial]
  
  # Extract the unique number between slashes
  number <- str_extract(Serial, "(?<=/)[0-9]+(?=/)")
  
  # Query to remove a row
  delete_query <- paste0("DELETE FROM student_details WHERE
                           Actions LIKE", search_string 
  )
  timeline_delete_query <- paste0("DELETE FROM student_timeline 
                                   WHERE Serial LIKE", paste0("'%", number, "%'"))
  units_delete_query <- paste0("DELETE FROM registered_units 
                                   WHERE Serial LIKE", paste0("'%", number, "%'"))
  DBI::dbSendQuery(con, timeline_delete_query)
  DBI::dbSendQuery(con, units_delete_query)
  DBI::dbSendQuery(con, delete_query)
  
  # Reload data from MySQL after deleting the row
  updated_data <- dbGetQuery(con, "SELECT * FROM student_details")
  register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
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
 filtered_df <- promote_data[Actions %like% search_string, ]
  current_year <- filtered_df[, Year]

    # add a plus one
  new_year <- as.numeric(current_year) + 1
  
  # set condition
  code <- filtered_df$Code
  serial <- filtered_df$Serial
  year <- filtered_df$Year
  student_units <- units[year == current_year &
                          (course == "BOTH" | course == code), ]  
  student_reg_units <- register_units$data_table[Serial == serial & 
                                                  Year == year, 
                                                 ][order(Code)]
  list1 <- student_units[, code]
  list2 <-student_reg_units[, Code]
  list <- setdiff(list1, list2)
  if(current_year < 4 & all(student_reg_units$Status == "PASSED") & 
     length(list)== 0) {
   
   # update the row
   per <- "0%"
   update_year_query <- sprintf("UPDATE student_details 
                                SET Year = %s,
                                Complete = '%s'
                                WHERE Actions LIKE '%%%s%%'", new_year, per, search_string) 
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
   DBI::dbSendQuery(con, timeline_query)
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
 
 # Keep track of current page
 currentPage <- reactiveVal(0)
 
 # Function to reset the current page number to 0
 reset_page <- function() {
  currentPage(0)
 }
 
  # all my pages are 10 rows each
  pageSize <- 10
  
  # Render UI for timeline
  timeline_block <- function(data) {
   unique_dates <- unique(data$DateOnly)
   lapply(seq_along(unique_dates), function(i) {
    current_date <- unique_dates[i]
    current_data <- filter(data, DateOnly == current_date)
    timelineBlock(
     width = 24,
     timelineLabel(format(current_date, "%Y-%m-%d"),
                   color = "teal"),
     lapply(seq_len(nrow(current_data)), function(j) {
      
     action_icon <- current_data$Actions[j]
      
      # Create a lookup table for action icons and corresponding set icons
      icon_lookup <- c("INSERT NEW" = "user-plus",
                       "UPDATE USER" = "user-pen",
                       "PROMOTE STUDENT" = "turn-up",
                       "REGISTER UNIT" = "marker",
                       "SUCCESS REGISTRATION" = "list",
                       "UPDATE MARK" = "user-graduate",
                       "PAID FEES" = "money-check-dollar",
                       "DELETE MARK" = "ban",
                       "EDIT MARK" = "pencil",
                       "APPROVE MARK" = "check",
                       "PRINT TRANSCRIPT" = "download",
                       "UPDATE VERSION" = "code-fork",
                       "REJECT MARK" = "x")
      
      # Vectorized approach to set icons
      set_icon <- icon_lookup[[current_data$Actions[j]]]
      
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
  }
  
  # loading spinner
  loader <- Waiter$new(id = "end", 
                       html = spin_loaders(
                        id = 3, 
                        color = "#696f72",
                        style = NULL
                        ),
                       color = "#ffffff1f"
                       )
  
 # Function to load data from database
 loadData <- function(numbers, currentPage, pageSize) {
  offset <- (currentPage - 1) * pageSize
  
  # Construct the SQL query with placeholders for Serial, pageSize, and offset
  query <- sprintf("SELECT * FROM student_timeline 
                   WHERE Serial LIKE '%%%s%%'
                   ORDER BY Date DESC
                   LIMIT %d OFFSET %d", numbers, pageSize, offset)
  
  data <- as.data.table(dbGetQuery(con, query))
  
  #If no rows are returned, return an empty data frame
  if (nrow(data) == 0) {
   return(data.frame())
  }

   # Convert Date column to datetime
   data$Date <- as.POSIXct(data$Date, tz = "UTC")
   # Separate Date column into Date and Time columns
   data <- data %>%
    mutate(DateOnly = as.Date(Date),
           TimeOnly = format(Date, "%H:%M:%S")) 
   return(data)

 }
 
 # Function to load next page data
 load_next_page <- function() {
   # Increment the page number
   currentPage(currentPage() + 1)
   # Fetch data for the next page
   next_page_data <- loadData(numbers, currentPage(), 10)
   # Check if the returned data frame is empty
   if (nrow(next_page_data) == 0) {
    shinyjs::show("empty")
    output$empty <- renderUI(
     img(
      src = "empty.gif",
      align = "center",
      height = "100px",
      width = "500px",
      style = "margin-top: -260px; margin-left: 170px;"
     )
    )
   } else {
    shinyjs::hide("empty")
    loader$show()
    on.exit({
     loader$hide() 
    })
    Sys.sleep(0.5)
    
    # Render the next page data
    data_ui <- timeline_block(next_page_data)
    insertUI(selector = "#end", where = "beforeBegin", ui = data_ui)
   }
  }
 
 # view a student timeline
 observeEvent(input$timeline_button, {
  shinyjs::hide("empty")
  selectedRow <- as.numeric(strsplit(input$timeline_button, "_")[[1]][2])
  search_string <- paste0("timeline_ ",selectedRow)
  
  # import data
  details_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
 
  filtered_df <- details_data[Actions %like% search_string, ]
  
  # Get the student in review
  Serial_No <- filtered_df[, Serial]
  
  # Extract numbers between slashes
  numbers <<- sub(".*/(\\d+)/.*", "\\1", Serial_No)
  numbers <<- as.numeric(numbers)
  Name <- filtered_df[, Name]
  
  # open the Modal Dialog first
  user_timeline_modal(reg_no = paste(Serial_No,"",Name))
 })
 
 # Event handler for when screen end is reached
 observeEvent(input$screen_end_reached, {
 # Call load_next_page function
 load_next_page()
 })
 
 observeEvent(input$ex, {
  reset_page()
  removeModal()
  shinyjs::hide("empty")
  })
 
 ### STUDENT REGISTRATION
 observeEvent(input$student_reg, {
  req(input$student_reg != "")
  student_reg <- input$student_reg
  shinyjs::hide("download") 
  shinyjs::hide("preview_pdf") 

  # student details
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  student_data <- data[Serial == student_reg, ]
  data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  token_query <- sprintf("SELECT * FROM student_tokens WHERE
 Serial LIKE '%s' ORDER BY Sequence DESC", student_reg)
  token_table <- as.data.table(dbGetQuery(con, token_query))[,.(`Amount Ksh`, Date, Token)]
  
  student_year <- student_data[, Year]
  student_course <- student_data[, Course]
  student_serial <- student_data[, Serial]
  student_name <- student_data[, Name]
  student_code <- student_data[, Code]
  
  # student name
  updateTextInput(
   session = session,
   inputId = "student_name",
   value = student_name
  )
  # student_course
  updateTextInput(
   session = session,
   inputId = "student_course",
   value = paste(student_code," - ", student_course)
  )
  updateTextInput(
   session = session,
   inputId = "current_year",
   value = student_year
  )
  # render tokens table
  output$student_tokens <- renderDataTable({
   datatable(token_table,
             escape = FALSE,
             selection = "none"
             )
   })
  
  # student_units
  student_units <- units[year == student_year &
                          (course == "BOTH" | course == student_code), ] 
  
  list1 <- student_units[, code]
  
  # create a table for a specific student
  student_reg_units <- data_table[Serial == student_reg &
                                   Year == student_year,
                                  ][order(Code)]
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
  list2 <- student_reg_units[, Code]
  
  # Remaining units
  list <- setdiff(list1, list2)
  # Get the students registered for a single unit
  registered_students <- data_table[, Serial]
  if(student_reg %in% registered_students){
   
   # already passed units
   n_passed <- student_reg_units[Status == "PASSED", .N]
   per <- n_passed/length(list1)
   update_progress("bar", per)
   
  }
  
  # list update
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
   supp_code <- student_reg_units[Status == "FAILED", Code]
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = supp_code,
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
  num_rows <- student_units[, .N]
  
  output$timetable <- DT::renderDataTable({
   student_units_table <- student_units[, c("lecturer", "time") := 
                                         list(rep(sample2, length.out = num_rows),
                                              rep(sample1, length.out = num_rows))
   ][, .(code, title, lecturer, time)]
   
   datatable(student_units_table, 
             escape = FALSE,
             selection = "none",
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
  
  # approved marks
  t_b_b <- data_table[Status == "PASSED" | Status == "FAILED", ]
  
  # first year
  chosen_units1 <- units[year == 1 &
                          (course == "BOTH" | course == student_code), ]
  
  codes1 <-  chosen_units1[, code]
  t_b_1 <- t_b_b[Serial == input$student_reg & Code %in% codes1,
                 .(Serial, Code, Course, Grade, Score)]
  
  # Calculate stats
  n_total1 <- t_b_1[, .N]
  total1 <- t_b_1[, sum(Score)]
  average1 <- round(total1/n_total1, 2)
  
  # second year
  chosen_units2 <- units[year == 2 & 
                          (course == "BOTH" | course == student_code), ]
  
  codes2 <-  chosen_units2[, code]
  t_b_2 <- t_b_b[Serial == input$student_reg & Code %in% codes2,
                 .(Serial, Code, Course, Grade, Score)]
  # Calculate stats
  n_total2 <- t_b_2[, .N]
  total2 <- t_b_2[, sum(Score)]
  average2 <- round(total2/n_total2, 2)
  
  # third year
  chosen_units3 <- units[year == 3 &
                          (course == "BOTH" | course == student_code), ]
                          
  
  codes3 <-  chosen_units3[, code]
  t_b_3 <- t_b_b[Serial == input$student_reg & Code %in% codes3,
                 .(Serial, Code, Course, Grade, Score)]
  # Calculate stats
  n_total3 <- t_b_3[, .N]
  total3 <- t_b_3[, sum(Score)]
  average3 <- round(total3/n_total3, 2)
  
  # fourth year
  chosen_units4 <- units[year == 4 &
                          (course == "BOTH" | course == student_code), ]
  
  codes4 <-  chosen_units4[, code]
  t_b_4 <- t_b_b[Serial == input$student_reg & Code %in% codes4,
                 .(Serial, Code, Course, Grade, Score)]
  # Calculate stats
  n_total4 <- t_b_4[, .N]
  total4 <- t_b_4[, sum(Score)]
  average4 <- round(total4/n_total4,2)
  
  # final average
  student_f_units <- units[course == "BOTH" | course == student_code, ]
  
  codes_all <- c(t_b_1[, Code], t_b_2[, Code], t_b_3[, Code], t_b_4[, Code])
  t_b_f <- t_b_b[Serial == input$student_reg & 
                  Code %in% codes_all,
                 .(Serial, Code, Course, Grade, Score)]
  n_total <- t_b_f[, .N]
  total <- t_b_f[, sum(Score)]
  average <- round(total/n_total, 2)
  
  # To find final averages
  if(student_year == 1){
   
   #finale data
   data <- data.frame(
    Year = "First",
    Units = n_total1,
    Average = average1
   )
   t_b_1 <- t_b_b[Serial == input$student_reg & Year == 1,
                  .(Code, Course, Grade)
                  ][order(Code)]
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
    Units = c(n_total1, n_total2),
    Average = c(average1, average2)
   )
   t_b_1 <- t_b_b[Serial == input$student_reg & Year == 1,
                  .(Code, Course, Grade)
                  ][order(Code)]
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
   
   t_b_2 <- t_b_b[Serial == input$student_reg & Year == 2,
                  .(Code, Course, Grade)
                  ][order(Code)]
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
    Year = c("First", "Second", "Third"),
    Units = c(n_total1, n_total2, n_total3),
    Average = c(average1, average2, average3)
   )
   t_b_1 <- t_b_b[Serial == input$student_reg & Year == 1,
                  .(Code, Course, Grade)
   ][order(Code)]
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
   t_b_2 <- t_b_b[Serial == input$student_reg & Year == 2,
                  .(Code, Course, Grade)
   ][order(Code)]
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
   t_b_3 <- t_b_b[Serial == input$student_reg & Year == 3,
                  .(Code, Course, Grade)
   ][order(Code)]
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
    Year = c("First", "Second", "Third", "Fourth"),
    Units = c(n_total1, n_total2, n_total3, n_total4),
    Average = c(average1, average2, average3, average4)
   )
   t_b_1 <- t_b_b[Serial == input$student_reg & Year == 1,
                  .(Code, Course, Grade)
   ][order(Code)]
   
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
   t_b_2 <- t_b_b[Serial == input$student_reg & Year == 2,
                  .(Code, Course, Grade)
   ][order(Code)]
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
   
   t_b_3 <- t_b_b[Serial == input$student_reg & Year == 3,
                  .(Code, Course, Grade)
   ][order(Code)]
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
   
   t_b_4 <- t_b_b[Serial == input$student_reg & Year == 4,
                  .(Code, Course, Grade)
                  ][order(Code)]
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
  table_1 <- t_b_1[order(Code), .(Code, Course, Grade)]
  new_row_1 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average1)
  )
  table_data1 <- rbind(table_1, new_row_1)
  
  table_2 <- t_b_2[order(Code), .(Code, Course, Grade)]
  new_row_2 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average2)
  )
  table_data2 <- rbind(table_2, new_row_2)
  
  table_3 <- t_b_3[order(Code), .(Code, Course, Grade)]
  new_row_3 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average3)
  )
  table_data3 <- rbind(table_3, new_row_3)
  
  table_4 <- t_b_4[order(Code), .(Code, Course, Grade)]
  new_row_4 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average4)
  )
  table_data4 <- rbind(table_4, new_row_4)
  
  # allow transcripts download
  if(nrow(t_b_1) > 0){
   enable("transcripts")
  }else{
   disable("transcripts")
  }
  # comment
  a <- average
  class <- case_when(
   a >= 70 ~ "First Class Honours",
   a >= 60 ~ "Second Class-Upper",
   a >= 50 ~ "Second Class-Lower",
   a >= 1 ~ "Pass",
   TRUE ~ ""
  )
  # availability conditions
  if(student_reg %in% registered_students){
   
   # missing units
   r_units <- t_b_f[, Code]
   m_units <- setdiff(list1, r_units)
   
   # failed units
   file_2 <- data_table[Status == "FAILED", ]
   list_2 <- file_2[, Code]
   
   # print feedback
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
   
   # data table output
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
   
   # output class
   output$clock <- renderEcharts4r({
    e_charts() |> 
     e_gauge(average, class) |> 
     e_animation(duration = 4000)|>
     e_title("Average",
             left='center')
   })
   
   # graph 
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
          var colorList = ['#808080','#606060','#404040','#282828'];
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
     e_title(text = student_data[, Serial],
             subtext = student_data[, Course],
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
      right = 100,
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
  # value boxes for fees
  fees_lookup <- c("1" = "28500",
                   "2" = "26000",
                   "3" = "26000",
                   "4" = "26000"
                   )
  
  # Vectorized approach to fees look-up
  fees <- fees_lookup[[student_year]]
  #######
  output$total_fees <- renderValueBox({
   value <- paste("Ksh.", prettyNum(fees, big.mark =',', scientific = FALSE))
   my_valuebox(value,  
               title = "Tuition Fees Invoice",
               subtitle = paste("Fees payable for year", student_year),
               icon = icon("file-invoice"),
               color = "bg-info"
   )
  })
  ########
  output$paid_fees <-renderValueBox({
   value <- token_table[, sum(`Amount Ksh`)] 
   value <- paste("Ksh.", prettyNum(value, big.mark =',', scientific = FALSE))
   my_valuebox(value,  
                   title = "Total Paid",
                   subtitle = paste("You have paid"),
                   icon = icon("money-check"),
                   color = "bg-info"
   )
  })
  #######
  output$balance_fees <- renderValueBox({
   paid <- token_table[, sum(`Amount Ksh`)] 
   value <- as.numeric(fees) - paid
   value <- paste("Ksh.", prettyNum(value, big.mark =',', scientific = FALSE))
   my_valuebox(value,  
               title = "Outstanding Balance",
               subtitle = paste("Amount to clear"),
               icon = icon("sack-xmark"),
               color = "bg-info"
   )
  })
  ######
  output$last_token <- renderValueBox({
   n <- token_table[, .N]
   my_valuebox(n,  
               title = "Payments Instalments",
               subtitle = paste("Number of times student has paid"),
               icon = icon("hashtag"),
               color = "bg-info"
   )
  })
 })
 
 # prepare and download transcripts
 
 # prepare the transcripts
 observeEvent(input$transcripts,{
  
  # show system busy on download initiation
  show_modal_spinner(text = "Please Wait...")
  
  # logged in student
  student_reg <- input$student_reg
  
  #student details
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  
  student_data <- data[Serial == student_reg, ]
  student_year <- student_data[, Year]
  student_course <- student_data[, Code]
  
  #approved marks
  t_b_b <- data_table[Status == "PASSED" | Status == "FAILED", ]
  
  # first year
  chosen_units1 <- units[year == 1 & 
                          (course == "BOTH" | course == student_course), ]
  
  codes1 <-  chosen_units1[, code]
  t_b_1 <- t_b_b[Serial == input$student_reg & Code %in% codes1,
                 .(Serial, Code, Course, Grade, Score)]
  
  # Calculate stats
  n_total1 <- t_b_1[, .N]
  total1 <- t_b_1[, sum(Score)]
  average1 <- round(total1/n_total1, 2)
  
  # second year
  chosen_units2 <- units[year == 2 & 
                          (course == "BOTH" | course == student_course), ]
  
  codes2 <-  chosen_units2[, code]
  t_b_2 <- t_b_b[Serial == input$student_reg & Code %in% codes2,
                 .(Serial, Code, Course, Grade, Score)]
  # Calculate stats
  n_total2 <- t_b_2[, .N]
  total2 <- t_b_2[, sum(Score)]
  average2 <- round(total2/n_total2, 2)
  
  # third year
  chosen_units3 <- units[year == 3 & 
                          (course == "BOTH" | course == student_course), ]
  
  codes3 <-  chosen_units3[, code]
  t_b_3 <- t_b_b[Serial == input$student_reg & Code %in% codes3,
                 .(Serial, Code, Course, Grade, Score)]
  # Calculate stats
  n_total3 <- t_b_3[, .N]
  total3 <- t_b_3[, sum(Score)]
  average3 <- round(total3/n_total3, 2)
  
  # fourth year
  chosen_units4 <- units[year == 4 & 
                          (course == "BOTH" | course == student_course), ]
  
  codes4 <-  chosen_units4[, code]
  t_b_4 <- t_b_b[Serial == input$student_reg & Code %in% codes4,
                 .(Serial, Code, Course, Grade, Score)]
  # Calculate stats
  n_total4 <- t_b_4[, .N]
  total4 <- t_b_4[, sum(Score)]
  average4 <- round(total4/n_total4, 2)
  
  # final average
  student_f_units <- units[course == "BOTH" | course == student_course, ]
  
  codes_all <- c(t_b_1[, Code], t_b_2[, Code], t_b_3[, Code], t_b_4[, Code])
  t_b_f <- t_b_b[Serial == input$student_reg & 
                  Code %in% codes_all,
                 .(Serial, Code, Course, Grade, Score)]
  n_total <- t_b_f[, .N]
  total <- t_b_f[, sum(Score)]
  average <- round(total/n_total, 2)
  
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
    Year = c("First", "Second"),
    Units = c(n_total1, n_total2),
    Average = c(average1, average2)
   )
  }else if(student_year == 3){
   
   #finale data
   data <- data.frame(
    Year = c("First", "Second", "Third"),
    Units = c(n_total1, n_total2, n_total3),
    Average = c(average1, average2, average3)
   )
  }else{
   
   #finale data
   data <- data.frame(
    Year = c("First", "Second", "Third", "Fourth"),
    Units = c(n_total1, n_total2, n_total3, n_total4),
    Average = c(average1, average2, average3, average4)
   )
  }
  
  #transcript tables
  table_1 <- t_b_1[order(Code), .(Code, Course, Grade)]
  new_row_1 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average1)
  )
  table_data1 <- rbind(table_1, new_row_1)
  
  table_2 <- t_b_2[order(Code), .(Code, Course, Grade)]
  new_row_2 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average2)
  )
  table_data2 <- rbind(table_2, new_row_2)
  
  table_3 <- t_b_3[order(Code), .(Code, Course, Grade)]
  new_row_3 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average3)
  )
  table_data3 <- rbind(table_3, new_row_3)
  
  table_4 <- t_b_4[order(Code), .(Code, Course, Grade)]
  new_row_4 <- data.table(
   Code = "",
   Course = "AVERAGE SCORE",
   Grade = as.character(average4)
  )
  table_data4 <- rbind(table_4, new_row_4)
  
  #parameters
  r_name <- student_data[, Name]
  reg_no <- input$student_reg
  prog <- student_data[, Course]
  date <- student_data[, Date]
  id <- student_data[, ID]
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

   
 student_units <- units[year == student_year & 
                         (course == "BOTH" | course == student_course), ]
  list1 <- student_units[, code]
  
  # missing units
  r_units <- t_b_f[, Code]
  m_units <- setdiff(list1, r_units)
  # failed units
  file_2 <- data_table[Status == "FAILED", ]
  list_2 <- file_2[, Code]
  
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
                                      output_file = paste0(reg_No,
                                                          "_transcript.html"),
                                                                output_dir = "www",
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
                                      output_file = paste0(reg_No,
                                                          "_transcript.html"),
                                                              output_dir = "www",

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
                                      output_file = paste0(reg_No,
                                                          "_transcript.html"),
                                                                                               output_dir = "www",

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
   render_result <- rmarkdown::render(
    "transcript4.Rmd",
     output_file = paste0(reg_No, "_transcript.html"),
                output_dir = "www",

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
  
    # create path for the html file
  path <- gsub("/","", paste0(reg_no,"_transcript.html"))
  path <- paste0("www/", path)

  #update timeline
  Users <- r_name
  reg_no <- reg_no
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Actions <- "PRINT TRANSCRIPT"
  Description <- paste0("Printed transcript: ",reg_no,"_transcript.pdf") 
  
  #write changes
  timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
  DBI::dbSendQuery(con,timeline_query)
  
  #convert HTML to a pdf file
  
  # create extra chrome arguments to take in print
  chrome_extra_args <- function(default_args = c("--disable-gpu")) {
   args <- default_args
   # Test whether we are in a shinyapps container
   if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    args <- c(args,
              "--no-sandbox", # required because we are in a container
              "--disable-dev-shm-usage") # in case of low available memory
   }
   args
  }
  

  # print the html to pdf
  pagedown::chrome_print(
   input = path,
   output = gsub("html", "pdf", path),
   extra_args = chrome_extra_args(),
   verbose = 1,
   async = TRUE # returns a promise
   )$then(
    onFulfilled = function(value) {
     #PDF ready
     resetLoadingButton("transcripts")
     showToast("info", "Transcripts ready!",
               .options = myToastOptions)
     
     #once download button is clicked
     output$download <- downloadHandler(
      filename =  paste0(reg_no,"_transcript.pdf"),
      content = function(file) {
       file.copy(value, file)
      },
      contentType = "application/pdf"
      )
     # Show the download and preview buttons
     shinyjs::show("download") 
     shinyjs::show("preview_pdf") 

    },
    onRejected = function(error) {
     showToast("error", "Transcripts printing failed!",
               .options = myToastOptions
               )
     HTML("")
    }
   )$finally(remove_modal_spinner) 

 })
 # preview the transcripts before download
 observeEvent(input$preview_pdf, {
   reg <- gsub("/","", input$student_reg)
  path <- paste0(reg, "_transcript.html")
  shiny::showModal(
  shiny::modalDialog(
    title = "Transcript Preview",
    size = "l",
    easyClose = TRUE,
    tags$iframe(
      src = path
      )
  )
           )
       })
 # create a supplementry registration
 observeEvent(input$type, {
  req(input$student_reg != "")
  student_reg <- input$student_reg
  #student details
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details")) 
  data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  
  student_data <- data[Serial == student_reg, ]
  student_year <- student_data[, Year]
  student_course <- student_data[, Code]
  
  if(input$type %in% "SUPPLEMENTARY"){
   
   # create a table for a specific student
   student_reg_units <- data_table[Serial == student_reg &
                                    Year == student_year
                                   ][order(Code)]
   
   
   # failed units
   failed_units <- student_reg_units[Status == "FAILED" &
                                      Type != "SUPPLEMENTARY", ]
   updateSelectizeInput(
    session = session,
    inputId = "register_code",
    choices = failed_units[, Code],
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3)
   )
  }else{
   
   # update field with unregistered units only
   student_units <- units[year == student_year &
                          (course == "BOTH" | course == student_course), ]
   # create a table for a specific student
   student_reg_units <- data_table[Serial == student_reg &
                                    Year == student_year]
   list1 <- student_units[, code]
   list2 <- student_reg_units[, Code]
   list <- setdiff(list1, list2)
   
   req(length(list)>0)
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
  status <- admin_file[[2,"seconds"]]
  get <- admin_file[[2,"close"]]
  app_version <<- admin_file[[3, "close"]]
  
  # update App version
  output$version_text <- renderText(
   paste0(paste0("V", gsub("^(\\d+)", "\\1", app_version)))
  )
  if(status == 1){
   updateTextAreaInput(
    session = session,
    inputId = "post_writeup",
    value = get
   )
   output$announcement <- renderText({
    paste0("NOTE: ", get)
   })
   disable("post_writeup")
  }else{
   hide("announcement")
   enable("post_writeup")
  }
  invalidateLater(1000, session)
  time1 <- as.POSIXct(admin_file[[1,"close"]], tz = "Africa/Nairobi")
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
   days <- sprintf("%02d", floor(time_difference / (24 * 3600)))
   time_difference_1 <- time_difference %% (24 * 3600)
   hours <- sprintf("%02d", floor(time_difference_1 / 3600))
   time_difference_2 <- time_difference_1 %% 3600
   minutes <- sprintf("%02d", floor(time_difference_2 / 60))
   seconds <- sprintf("%02d", round(time_difference_2 %% 60, 0))
   
   #output flip time countdown
   output$days <- renderText(days)
   output$hours <- renderText(hours)
   output$minutes <- renderText(minutes)
   output$seconds <- renderText(seconds)
   
   # update progress bar
   progress_value <- (time_difference/as.numeric(admin_file[[1,"seconds"]]))* 100
   updateProgressBar(session, "progress_bar", value = progress_value)
   
   # show in numericals
   output$remaining_time <- renderText({
    paste(days,":", hours, ":", minutes, ":", seconds)
   })
  }else{
   # Update remaining time on close
   updateProgressBar(session, "progress_bar", value = 0)
   output$remaining_time <- renderText({
    paste("00:00:00:00")
   })
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
   datatable(updated_data[1,1],
             escape = FALSE,
             selection = "none",
             rownames = FALSE,
             options = list(
              columnDefs = list(
               list(targets = "_all", 
                    className = "dt-center")
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

    #update course titles
  t <- input$register_code
  course_title <- units[code == t, title][1]
  updateTextInput(
   session = session,
   inputId = "register_unit",
   value = course_title
  )
 })
 
 # register a unit
 observeEvent(input$register, {
  student_reg <- input$student_reg
  if(input$register_code == ""){
   showToast("info", "Select a Unit!",
             .options = myToastOptions )
  }else if(input$register_code != "" && student_reg != "" ){
   req(input$register_unit)
   req(input$register_code)
   
   #student details
   data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
   student_data <- data[Serial == student_reg, ]
   student_reg <- student_data[, Serial]
   student_year <- student_data[, Year]
   student_course <- student_data[, Code]
   student_name <- student_data[, Name]
   Code <- input$register_code
   Course <- input$register_unit
   Type <- input$type
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
   # Reload data from MySQL
  data_table  <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units")) 

   # update field with unregistered units only
   student_units <- units[year == student_year &
                           (course == "BOTH" | course == student_course), ]
   # create a table for a specific student
   student_reg_units <- data_table[Serial == student_reg &
                                    Year == student_year
                                   ][order(Code)]
   list1 <- student_units[, code]
   list2 <- student_reg_units[, Code]
   list <- setdiff(list1, list2)
   
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
   showToast("success", "SUCCESS: REGISTERED",
             .options = myToastOptions )
  }else{
   resetLoadingButton("register")
   showToast("error", "Registration Number required!",
             .options = myToastOptions )  
  }
  resetLoadingButton("register")
 })
 
 observeEvent(input$reg, {
  if(input$id == "0"){
   updateTextInput(
    session = session,
    inputId = "grade",
    value = ""
   )
   updateNumericInput(
    session = session,
    inputId = "score",
    value = ""
   )
   data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
   # Registered but with no marks units
   registered_units <- data_table[Serial == input$reg &
                                   (is.na(Score) | Status == "REGISTERED"), ]
   
   # update student name
   x <- input$reg
   name_df <- registered_units[Serial == x, .(Name, Code), ]
   updateTextInput(
    session = session,
    inputId = "name",
    value = name_df[, unique(Name)]
   )
   # update code input
   updateSelectizeInput(
    session = session,
    inputId = "code",
    choices = name_df[, unique(Code)],
    selected = "",
    server = TRUE,
    options = list(maxOptions = 3) 
   )
  }
 })
 
 observeEvent(input$code, {
  if(input$id == "0"){
   #update course titles
   t <- input$code
   course_title <- units[code == t, title][1]

      # update course title
   updateTextInput(
    session = session,
    inputId = "course",
    value = course_title
   )
  }
  data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  
  if(input$code != "" & input$reg != ""){
  # check exam type
  exam_type <- data_table[Serial == input$reg &
                           Code == input$code, Type]
  if(exam_type == "FIRST ATTEMPT"){
   updateTextInput(session, "grade", label = "Grade")
   updateNumericInput(session, "score", "Score")
  }else{
   showToast(
    "info", "Mark should be < 50%", .options = myToastOptions
   )
   updateTextInput(session, "grade", label = "Grade *")
   updateNumericInput(session, "score", "Score *")
  }
  }
 })
 
 observeEvent(input$score,{
  data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  
  if(input$code != "" & input$reg != ""){
  # check exam type
  exam_type <- data_table[Serial == input$reg &
                           Code == input$code, Type]
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
  }
 })
 
 
 # Render the DataTable
 output$marks <- renderDataTable({
  
  # create a table for a specific student
  student_marks <- register_units$data_table[!is.na(Score),
                                             .(Serial, Name, Code, 
                                               Course = str_to_title(Course), 
                                               Score, Grade, Lecturer, Date,
                                               Actions)
                                             ][order(-Date)]
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
  row <- details_data[Actions %like% search_string, ]
  
  # update timeline on mark submission
  reg_no <- row[, Serial]
  Users <- row[, Name]
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Actions <- "PAID FEES"
  Amount <- sample(seq(1000, 10000, by = 5)*2, 1)
  Description <- paste("Paid Ksh.", Amount, " Tuition fee for SEM 1 2024", sep = "")
  timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
  DBI::dbSendQuery(con,timeline_query)
  showToast(
   "success", "Payment Accepted", .options = myToastOptions
  )
  # update tokens table with next number
  token_query <- sprintf("SELECT * FROM student_tokens WHERE
 Serial LIKE '%s' ORDER BY Sequence DESC", reg_no)
  token_table <- as.data.table(dbGetQuery(con, token_query))
  N <- token_table[,.N]
  Sequence <- as.numeric(N)+1
  
  Token <- paste(gsub("(.{4})", "\\1 ", 
                      paste(sample(0:9, 12, replace = TRUE), collapse = "")), collapse = "")
  tokens_query <- paste0("INSERT INTO student_tokens
                       VALUES (", Sequence, ",'", reg_no, "', ", Amount, ", STR_TO_DATE('", Dates, "', '%d/%m/%Y %H:%i:%s'), '", Token, "')")
  
  DBI::dbSendQuery(con,tokens_query)
  # Reload data from DB
  token_data$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM student_tokens"))
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
  search_string <- paste0("\\b", search_string, "\\b")
  
  row <- data[Actions %like% search_string, ]
  updateSelectInput(session, "edit_code", selected = row[, Code])
  updateTextInput(session, "edit_name", value = row[, Name])
  updateSelectInput(session, "edit_gender", selected = row[, Gender])
  updateNumericInput(session, "edit_ID", value = row[, ID])
  updateTextInput(session, "edit_course", value = row[, Course])
  updateTextInput(session, "edit_buttons", value = row[, Actions])
  
  # Get ID to prevent empty execution
  entered_ID <- row$ID
  string_selected <- data[ID == entered_ID, Serial][1]
  
  #update the regInput 
  updateTextInput(session = session, inputId = "edit_reg",
                  value = string_selected )
  
 })
 
 # confirm deletion of a row record
 observeEvent(input$confirm_delete, {
  selectedRow <- as.numeric(strsplit(input$delete_button, "_")[[1]][2])
  search_string_1 <- paste0("delete_ ",selectedRow)
  
  search_string <- paste0("'%", search_string_1, "%'")
  
  # Load data from MySQL table into a data.table
  marks_data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
   
  # Get the serial number
  row <- marks_data[Actions %like% search_string_1, ]
  status <- row[, Status]
  if(status %in% "RELEASED"){
   # update timeline on deletion
   reg_no <- row[, Serial]
   Code <- row[, Code]
   Course <- row[, Course]
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
   
  }else {
   showToast("error",
             "Not Allowed: Approved!",
             .options = myToastOptions)
   removeModal()
  }
 })
 
 # edit Button on marks
 observeEvent(input$edit_button, {
  confirm_modal_dialog(approve = FALSE, edit = TRUE, delete = FALSE,
                       editreg = FALSE, promote = FALSE, deletereg = FALSE)
 })
 
 observeEvent(input$confirm_edit,{
  
  # Load data from MySQL table into a data.table
  data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  selectedRow <- as.numeric(strsplit(input$edit_button, "_")[[1]][2])
  search_string <- paste0("edit_ ",selectedRow)
  search_string <- paste0("\\b", search_string, "\\b")
  
  row <- data[Actions %like% search_string, ]
  status <- row[, Status]

    if(status %in% "RELEASED") {
   updateSelectizeInput(session, "reg", choices = row[, Serial], selected = row[, Serial])
   updateTextInput(session, "name", value = row[, Name])
   updateSelectizeInput(session, "code", choices = units[, code], selected = row[, Code])
   updateNumericInput(session, "course", value = row[, Course])
   updateTextInput(session, "score", value = row[, Score])
   updateTextInput(session, "grade", value = row[, Grade])
   updateTextInput(session, "actions", value = row[, Actions])
   updateTextInput(session, "id", value = "1")
   
   disable("code")
   disable("reg")
   
   showNotification("Edit the score only!", duration = 10, type = "message")
   removeModal()
   
  }else{
   showToast("error",
             "Not Allowed: Approved!",
             .options = myToastOptions)
   removeModal()
  }
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
   selected_data <- selected_data[Actions == action, ]
   old_mark <- selected_data[, Score]
   old_grade <- selected_data[, Grade]
   
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
   
   marks_change = paste0(old_mark, "% (", old_grade, ") \u2192 ", new_score, "% (", new_grade, ")")
   #update timeline
   Users <- sample(users, 1)
   reg_no <- selected_data[, Serial]
   Code <- selected_data[, Code]
   Course <- selected_data[, Course]
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
   updateTextInput(session, "name", value = "")
   updateSelectizeInput(session, "code", choices = "", selected ="")
   updateTextInput(session, "score", value = "")
   updateTextInput(session, "grade", value = "")
   
  }
 })
 
 # admin approve marks for the student 
 observeEvent(input$approve_button, {
  # Load data from MySQL table into a data.table
  selected_data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  selectedRow <- as.numeric(strsplit(input$approve_button, "_")[[1]][2])
  search_string <- paste0("approve_ ",selectedRow)
  search_string <- paste0("\\b", search_string, "\\b")
  
  row <- selected_data[Actions %like% search_string, ]
  action <- row[, Actions]
  score <- row[, Score]
  status <- row[, Status]

    if(status %in% "RELEASED") {
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
   reg_no <- row[, Serial]
   Code <- row[, Code]
   Course <- row[, Course]
   Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
   Actions <- "APPROVE MARK"
   Description <- paste("Approved Marks ",Code," : ", Course)
   # write changes
   timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
   DBI::dbSendQuery(con,timeline_query)
   
   # update % of completion for current year
   # student passed units
   student_year <- data$table_data[Serial == reg_no, Year]
   student_course <- data$table_data[Serial == reg_no, Code]
   n_passed <- register_units$data_table[Serial == reg_no & 
                                          Year == student_year &
                                          Status == "PASSED",
                                         .N]
   # overall year student_units
   n_units <- units[year == student_year &
                     (course == "BOTH" | course == student_course), .N]
   #% of completion
    per <- (n_passed/n_units)*100
    per <- paste0(round(per, 2), "%")
   # create the query
   update_query <- sprintf("UPDATE student_details SET
                           Complete = '%s'
                           WHERE Serial = '%s'",
                           per, reg_no)
   DBI::dbSendQuery(con,update_query)
   # Reload data
   data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  }else{
   showToast("error",
             "Not Allowed: Approved!",
             .options = myToastOptions)
  }
 })
 
 observe({
  data$table_data <- as.data.table(dbGetQuery(con, "SELECT * FROM student_details"))
  # output value boxes
  current_year <- lubridate::year(Sys.Date())
  current_date <- format(Sys.Date(), "%d-%m-%Y")
  previous_year <- current_year - 1
  previous_day1 <- Sys.Date() - 1
  previous_day <- format(previous_day1, "%d-%m-%Y")
  
  # year
  n_c <- data$table_data[Date == current_year, .N]
  n_p <- data$table_data[Date == previous_year, .N]
  per_n <- (((n_c-n_p)/n_p)*100) |> round(2)
  
  # day
  c_d <- register_units$data_table[Date == current_date, .N]
  p_d <- register_units$data_table[Date == previous_day, .N] 
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
  miss_today <- register_units$data_table[Status == "REGISTERED" & 
                                           Date == current_date, .N]
  miss_yester <- register_units$data_table[Status == "REGISTERED" &
                                            Date == previous_day, .N]
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
  fail_today <- register_units$data_table[Status == "FAILED" &
                                           Date == current_date, .N]
  fail_yester <- register_units$data_table[Status == "FAILED" &
                                            Date == previous_day, .N]
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
  prom_ready <- data$table_data[Year %in% c(1,2,3), ]
  prom_current <- prom_ready[Date == current_date, .N]
  prom_previous <- prom_ready[Date == previous_day, .N]
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
  ready_grad <- data$table_data[Year == 4, ]
  grad_current1 <- ready_grad[Year == 4, ]
  grad_current <- grad_current1[Date %like% current_year, .N] 
  grad_previous <- grad_current1[Date %like% previous_year, .N]
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
  admin_file <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
  output$set_time <- renderDataTable({
   datatable(admin_file[1,"close"],
             escape = FALSE,
             selection = "none",
             rownames = FALSE,
             options = list(
              columnDefs = list(
               list(targets = "_all", 
                    className = "dt-center")
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
     columns = "close",  # Apply to all columns
     textAlign = "center"   # Center alignment
    )
  })
  disable("target_date")
  disable("select_time")
 })
 observeEvent(input$post, {
  disable("post_writeup")
  shinyjs::show("announcement")
  
  # Update the MySQL table
  query <- sprintf("UPDATE administrator_file SET close = '%s', seconds = '%s'
                    WHERE id = 2", input$post_writeup, 1)
  dbExecute(con, query)
  admin_file <- as.data.table(dbGetQuery(con, "SELECT * FROM administrator_file"))
  get <- admin_file[[2,"close"]]
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
 # admin approve marks for the student 
 observeEvent(input$reject_button, {
  # Load data from MySQL table into a data.table
  selected_data <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
  selectedRow <- as.numeric(strsplit(input$reject_button, "_")[[1]][2])
  search_string <- paste0("reject_ ",selectedRow)
  
  row <- selected_data[Actions %like% search_string, ]
  status <- row[, Status]
  action <- row[, Actions]
  
  if(status %in% "RELEASED") {
   revert_status <- "REGISTERED"
   
   # change color for edited rows
   button <- action
   new_button <- gsub("background-color: #e9ecef;", 
                      "background-color: #0025ff8f;", button)
   edited_buttons <- gsub("RELEASED","REJECTED", new_button)
   
   # create the query
   update_query <- sprintf("UPDATE registered_units SET
                           Status = '%s',
                           Actions = '%s',
                           Grade = '%s',
                           Score = %s
                           WHERE Actions = '%s'", 
                           revert_status, edited_buttons,
                           "", 0, action)
   DBI::dbSendQuery(con,update_query)
   
   # Reload data
   register_units$data_table <- as.data.table(dbGetQuery(con, "SELECT * FROM registered_units"))
   showToast("success",
             "Student Marks Rejected!",
             .options = myToastOptions )
   
   #update timeline
   Users <- "Administrator"
   reg_no <- row[, Serial]
   Code <- row[, Code]
   Course <- row[, Course]
   Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
   Actions <- "REJECT MARK"
   Description <- paste("Rejected Marks ",Code," : ", Course)
   # write changes
   timeline_query <- paste0("INSERT INTO student_timeline
                  VALUES('",reg_no,"',STR_TO_DATE('",Dates,"','%d/%m/%Y %H:%i:%s'),'",Users,"','",Actions,"','",Description,"')")
   DBI::dbSendQuery(con,timeline_query)
   
  }else{
   showToast("error",
             "Not Allowed: Approved!",
             .options = myToastOptions)
  }
 })
 
 observe({
  
  # ensure all previous fields are filled before summary generation
  req(
   isTruthy(
    input$change_extent != "" & 
    input$change_type != "" &
    input$affected_area != "" &
    input$admin_thought != "" 
    )
  )
  
  # reference inputs
  extent <- input$change_extent 
  type <- input$change_type 
  area <- input$affected_area
  admin_thought <- input$admin_thought
  
  # get the app version from global environment
  app_version <- strsplit(app_version, "\\.")[[1]]
  
  major <- as.numeric(app_version[1])
  minor <- as.numeric(app_version[2])
  patch <- as.numeric(app_version[3])
  
  # customize value according to Extent of changes
  next_version <- c("Major" = glue::glue('V{major + 1}.{minor}.{patch}'),
                     "Minor" = glue::glue('V{major}.{minor + 1}.{patch}'),
                     "Patch" = glue::glue('V{major}.{minor}.{patch + 1}') 
                     )
  next_version <- as.list(next_version)[[input$change_extent]]

  # preview a summary of version update
  updateTextAreaInput(
   session = session,
   inputId = "summary_changes",
   value = glue::glue(
    'New Version: {next_version}
    A {extent} change to {type} a {area} with {admin_thought}
    '
   )
  )
 })
  # Allow user description of new version updates
  observeEvent(input$describe_changes, {
    str <- input$describe_changes
    words <- str_split(str, "\\s+")[[1]]
    words <- words[words != ""]
    word_count <- length(words)
    
    if (word_count > 50) {
      words <- words[1:50]
      updateTextAreaInput(session, "describe_changes", value = paste(words, collapse = " "))
      word_count <- 50
    
    }

    output$word_count <- renderText({
      glue("{word_count}/50 Words")
    })
  })
  
  # save changes to database
  observeEvent(input$updateVersion, {
  #update timeline
  Users <- "Administrator"
  Dates <- format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  Actions <- "UPDATE VERSION"
  Description <- "here"
  #write changes
  timeline_query <- "INSERT INTO student_timeline
  values('X74/0000/2024', 'Dates', 'Users', 'Actions', 'Description');" |>
      stringr::str_replace_all(pattern = "Dates", replacement = Dates) |>
    stringr::str_replace_all(pattern = "Users", replacement = Users)  |>
    stringr::str_replace_all(pattern = "Actions", replacement = Actions)  |>
    stringr::str_replace_all(pattern = "Description", replacement = Description)  
# Execute query
  DBI::dbSendQuery(con, timeline_query)
     showToast("success",
             "App Version Updated",
             .options = myToastOptions )
      resetLoadingButton("updateVersion")
  })
  # a timeline output for version updates
 output$version_timeline <- renderUI({
 # Call load_next_page function
 load_next_page()
})
 session$onSessionEnded(function() {
  dbDisconnect(con, add = TRUE)  # Disconnect when the session ends
 })
}

