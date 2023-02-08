library(tidyverse)
library(stringr)
library(rvest)
library(datapasta)
#import
names <- readxl::read_excel('user_results.xlsx')
names$Name <- paste(names$Name,names$Name1)
names <- names[,-3]
fix(names)
names <- names |> mutate (Reg.No= sample(1435:9714,1002,rep=FALSE))
unique(names$Reg.No)
department <- c('X74/','X75/')
dep <- sample(department,1002,rep=TRUE)
names <- names[,-3]
names$year <- rep('/2018',1002)
names$Reg.No <- paste(names$Reg.No,names$year)
nchar(names$Reg.No)
grepl('',names$Reg.No)
names$Reg.No <- gsub(' ','',names$Reg.No)


 #Grade allocation
x <- user_results$Science
user_results<- user_results |> 
  mutate( Maths_grade = 
            case_when(
              x >= 70 ~'A',
              x >= 60 ~'B',
              x >= 50 ~'C',
              x >= 40 ~'D',
              TRUE ~ 'E'
            )
          )
#combine columns
user_results$Science <- paste(user_results$Science,
                              user_results$Maths_grade)
user_results <- user_results[,-7]

#show grade alone
grade <- user_results |> subset(user=='Jefferson') 
grade <- grade[,-1]
r_score <- as.character(grade[1,])
name <- grade |> colnames()
grade_summary <- data.frame(subject= name,
                             score = r_score
                            )
final_grade <- str_sub(grade_summary$score,-1)
show_table <- data.frame(subject= name,
           score = final_grade
           )














