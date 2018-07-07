#setwd("D:/School/BISS/MSc Thesis/Data")
setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
rm(list=ls())

#install.packages("dplyr")
#install.packages("tidyr")

library("dplyr")
library("tidyr")

studentInfo <- read.csv("studentInfo_v3.csv")
studentInfo$X <- NULL
studentInfo$id_student <- as.character(studentInfo$id_student)
studentInfo <- studentInfo[studentInfo$final_result != "Withdrawn",]
#studentInfo[studentInfo == "Distinction"] <- "Pass"
studentInfo <- droplevels(studentInfo)


studentInfo_new <- studentInfo[,c(1:3,13:45, 12)]
Auxiliary_input <- studentInfo[,c(1:12,46,47)]

names(studentInfo_new)[4:14] <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
Gathered_Clicks <- gather(studentInfo_new, key = "Decile", value = "Avg_Clicks", 4:14, convert = TRUE)
Gathered_Clicks <- Gathered_Clicks[,-c(4:25)]
studentInfo_new <- studentInfo_new[,-c(4:14)]

names(studentInfo_new)[4:14] <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
Gathered_Asgmts <- gather(studentInfo_new, key = "Decile", value = "Asgmts", 4:14, convert = TRUE)
studentInfo_new <- studentInfo_new[,-c(4:14)]
Gathered_Asgmts <- Gathered_Asgmts[,-c(4:14)]

names(studentInfo_new)[4:14] <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
Gathered_Scores <- gather(studentInfo_new, key = "Decile", value = "Avg_Asgmt_Score", 4:14, convert = TRUE)

df_list <- list(Gathered_Clicks, Gathered_Asgmts, Gathered_Scores)
studentInfo_LSTM <- Reduce(function(x, y) merge(x, y, by = c("code_module", "code_presentation", "id_student","Decile","final_result"), all=TRUE), df_list, accumulate=FALSE)
studentInfo_LSTM <- studentInfo_LSTM[order(studentInfo_LSTM$code_module, studentInfo_LSTM$code_presentation, studentInfo_LSTM$id_student,studentInfo_LSTM$Decile),c(3,1,2,4,6,7,8,5)]
str(studentInfo_LSTM)

BBB_LSTM_Input <- studentInfo_LSTM[studentInfo_LSTM$code_module == "BBB",-c(2,3)]
DDD_LSTM_Input <- studentInfo_LSTM[studentInfo_LSTM$code_module == "DDD",-c(2,3)]
FFF_LSTM_Input <- studentInfo_LSTM[studentInfo_LSTM$code_module == "FFF",-c(2,3)]

write.csv(BBB_LSTM_Input, "BBB_LSTM_Input.csv")
write.csv(DDD_LSTM_Input, "DDD_LSTM_Input.csv")
write.csv(FFF_LSTM_Input, "FFF_LSTM_Input.csv")
