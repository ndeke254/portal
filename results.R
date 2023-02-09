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
x <- results$`XET201 MICROECONOMICS`
results <- results |> 
  mutate( `XET201 MICROECONOMICS GRADE` = 
            case_when(
              x >= 70 ~'A',
              x >= 60 ~'B',
              x >= 50 ~'C',
              x >= 40 ~'D',
              TRUE ~ 'E'
            )
          )
#combine columns
results$`XET201 MICROECONOMICS` <- paste(results$`XET201 MICROECONOMICS`,
                              results$`XET201 MICROECONOMICS GRADE`)
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

results$`XET201 MICROECONOMICS` <- runif(1002,23,99) |> round(0)



results <- results[,-4]
library(openxlsx)

write.xlsx(names,'names.xlsx')
write.xlsx(results,'results.xlsx')


x <- 'X75/7666/2018' 
grade <- results |> filter(Reg.No %in% x)
grade <- grade |> select(`XET201 MICROECONOMICS`)
r_score <- as.character(grade[1,])
name <- grade |> colnames()
grade_summary <- data.frame(Unit= name,
                            Grade = r_score
)
final_grade <- str_sub(grade_summary$Grade,-1)
show_table <- data.frame(subject= name,
                         score = final_grade
)


grade[[1]]

