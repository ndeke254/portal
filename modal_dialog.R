modal_dialog <- function(approve,delete,edit,promote,editreg,deletereg) {
 if(approve){
  x <- "Approve Mark"
 }else if(delete){
  x <- "Delete Mark" 
 }else if (edit){
  x <- "Edit Mark"
 }else if (editreg){
  x <- "Edit Student Details"
 }else if (deletereg){
  x <- "Delete Registration"
 }else{
  x <- "Promote Student"
 }
 if(approve){
  id <- "confirm_approve"
 }else if(delete){
  id <- "confirm_delete" 
 }else if(edit){
  id <- "confirm_edit"
 }else if(editreg){
  id <- "confirm_editreg"
 }else if(deletereg){
  id <- "confirm_deletereg"
 }else{
  id <- "confirm_promote"
 }
 shiny::modalDialog(
  title = x,
  div(
   paste(paste("Are you sure want to",str_to_lower(x),"?"))
   ),
  size = 's',
  easyClose = TRUE,
  footer = div(
   shiny::actionButton(inputId = id,
                       label   = "YES"
                       ),
   shiny::actionButton(inputId = "dismiss",
                       label   = "NO",
                       )
  )
 ) |>
  shiny::showModal()
}
