releasedDir <- "released_results"
registered_unitsDir <- "registered_units"
registered_students <- list.files(registered_unitsDir, full.names = FALSE)
#conditions to proceed
registered_students <- list.files(registered_unitsDir, full.names = FALSE)
released_marks <- list.files(releasedDir, full.names = TRUE) 
req(length(registered_students) != 0)
req(length(released_marks) != 0 ) 

list3 == failed units

chosen_units <- units |>
 filter(course %in% c("BOTH",student_course)) |>
 filter(year %in% chosen_year)
t_b_a <- t_b_b |> filter(reg %in% input$student_reg) |>
 select(reg, code, score)
finale <- cbind(chosen_units, t_b_a) 


#Extract failed units
list3 <- t_b |> filter(grepl("E", grade))
------------------------------------------------------------
 
 

#saving file
fileName <- gsub("/", "", input$student_reg)
file <- read.csv(file = paste0(registered_unitsDir,"/",fileName,".csv"))
#missing units
r_units <- as.list(t_b_f$code)
m_units <- setdif#To find final averages
if(student_year == 1){
 chosen_year <- 1
}else if(student_year == 2){
 chosen_year <- c(1,2)
}else if(student_year == 3){
 chosen_year <- c(1,2,3)
}else{
 chosen_year <- c(1,2,3,4)
}f(list1, r_units)
#failed units
file_2 <- file |>
 filter(Status %in% "FAILED")
list_2 <- as.list(file_2$Code)
#print feedback
if(length(m_units) > 0  & length(list_2) > 0){
 comment <- paste("FAIL: Missing units - ", m_units,";",
                  "Failed units - ", list_2)
}else if(length(m_units)> 0){
 comment <- paste("FAIL: Missing units - ", m_units,)
}else if(length(list_2)> 0){
 comment <- paste("FAIL: Failed units - ",list_2)
}else if(length(m_units)== 0  & length(list_2) == 0 & student_year == 4){
 comment <- paste("PASS: Final class is",class)
}else{
 comment <- paste("PASS: Promoted to the next Year")
}
#parameters
r_name <- student_data$Name
reg_no <- input$student_reg
prog <- student_data$Course
date <- student_data$Date
id <- student_data$ID
stamp_datee <- format(Sys.Date(),  format = "%d %b %y")
comment <- comment
#print the transcripts
observeEvent(input$transcripts,{
 if(nrow(table_data1) > 0){
  enable("transcripts")
 }else{
  disable("transcripts")
 }
 #saving file
 reg_No <- gsub("/", "", input$student_reg)
 if(student_year == 1){
  # Render the R Markdown template with the data
  render_result <- rmarkdown::render("transcripts/transcript1.Rmd",
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
  render_result <- rmarkdown::render("transcripts/transcript2.Rmd",
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
  render_result <- rmarkdown::render("transcripts/transcript3.Rmd",
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
  return()
 }
 #convert HTML to a pdf file
 pdf <- chrome_print(input = render_result, output = paste(reg_no,"_transcript.pdf"))
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

#output class
output$clock <- renderEcharts4r({
 e_charts() |> 
  e_gauge(average, class) |> 
  e_animation(duration = 4000)|>
  e_title('% SCORE',left='center')
})
#graph 
output$graph <- renderEcharts4r({
 data|> 
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




