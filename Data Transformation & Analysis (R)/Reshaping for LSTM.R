#setwd("D:/School/BISS/MSc Thesis/Data")
setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
rm(list=ls())

#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("fastDummies")

library("dplyr")
library("tidyr")
library("fastDummies")

studentInfo <- read.csv("studentInfo_v3.csv")
studentInfo$X <- NULL
studentInfo$id_student <- as.character(studentInfo$id_student)
studentInfo <- studentInfo[studentInfo$code_module == "BBB" | studentInfo$code_module == "DDD" | studentInfo$code_module == "FFF",]
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


BBB_LSTM_Input_Train <- studentInfo_LSTM[studentInfo_LSTM$code_module == "BBB" & studentInfo_LSTM$code_presentation != "2014J",
                                         -c(2)]
BBB_LSTM_Input_Test <- studentInfo_LSTM[studentInfo_LSTM$code_module == "BBB" & studentInfo_LSTM$code_presentation == "2014J",
                                        -c(2)]

DDD_LSTM_Input_Train <- studentInfo_LSTM[studentInfo_LSTM$code_module == "DDD" & studentInfo_LSTM$code_presentation != "2014J",
                                         -c(2)]
DDD_LSTM_Input_Test <- studentInfo_LSTM[studentInfo_LSTM$code_module == "DDD" & studentInfo_LSTM$code_presentation == "2014J",
                                        -c(2)]

FFF_LSTM_Input_Train <- studentInfo_LSTM[studentInfo_LSTM$code_module == "FFF" & studentInfo_LSTM$code_presentation != "2014J",
                                         -c(2)]
FFF_LSTM_Input_Test <- studentInfo_LSTM[studentInfo_LSTM$code_module == "FFF" & studentInfo_LSTM$code_presentation == "2014J",
                                        -c(2)]


write.csv(BBB_LSTM_Input_Train, "BBB_LSTM_Input_Train.csv")
write.csv(DDD_LSTM_Input_Train, "DDD_LSTM_Input_Train.csv")
write.csv(FFF_LSTM_Input_Train, "FFF_LSTM_Input_Train.csv")
write.csv(BBB_LSTM_Input_Test, "BBB_LSTM_Input_Test.csv")
write.csv(DDD_LSTM_Input_Test, "DDD_LSTM_Input_Test.csv")
write.csv(FFF_LSTM_Input_Test, "FFF_LSTM_Input_Test.csv")


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
Auxiliary_input$num_of_prev_attempts[Auxiliary_input$code_module == "BBB"] <- range01(Auxiliary_input$num_of_prev_attempts[Auxiliary_input$code_module == "BBB"])
Auxiliary_input$num_of_prev_attempts[Auxiliary_input$code_module == "DDD"] <- range01(Auxiliary_input$num_of_prev_attempts[Auxiliary_input$code_module == "DDD"])
Auxiliary_input$num_of_prev_attempts[Auxiliary_input$code_module == "FFF"] <- range01(Auxiliary_input$num_of_prev_attempts[Auxiliary_input$code_module == "FFF"])

Auxiliary_input$studied_credits[Auxiliary_input$code_module == "BBB"] <- range01(Auxiliary_input$studied_credits[Auxiliary_input$code_module == "BBB"])
Auxiliary_input$studied_credits[Auxiliary_input$code_module == "DDD"] <- range01(Auxiliary_input$studied_credits[Auxiliary_input$code_module == "DDD"])
Auxiliary_input$studied_credits[Auxiliary_input$code_module == "FFF"] <- range01(Auxiliary_input$studied_credits[Auxiliary_input$code_module == "FFF"])

# BBB
BBB_LSTM_AUX_Train <- Auxiliary_input[Auxiliary_input$code_module == "BBB" & Auxiliary_input$code_presentation != "2014J",
                                      -c(5,12)]
BBB_LSTM_AUX_Train <- BBB_LSTM_AUX_Train[order(BBB_LSTM_AUX_Train$code_module, BBB_LSTM_AUX_Train$code_presentation, BBB_LSTM_AUX_Train$id_student),]
BBB_LSTM_AUX_Train$year <- as.factor(BBB_LSTM_AUX_Train$year)
BBB_LSTM_AUX_Train <- cbind(BBB_LSTM_AUX_Train[,c(1,2,3)],dummy_cols(BBB_LSTM_AUX_Train[,-c(1,2,3)]))
BBB_LSTM_AUX_Train <- BBB_LSTM_AUX_Train[,-c(4:7,10:12)]


BBB_LSTM_AUX_Test <- Auxiliary_input[Auxiliary_input$code_module == "BBB" & Auxiliary_input$code_presentation == "2014J",
                                      -c(5,12)]
BBB_LSTM_AUX_Test <- BBB_LSTM_AUX_Test[order(BBB_LSTM_AUX_Test$code_module, BBB_LSTM_AUX_Test$code_presentation, BBB_LSTM_AUX_Test$id_student),]
BBB_LSTM_AUX_Test$year <- as.factor(BBB_LSTM_AUX_Test$year)
BBB_LSTM_AUX_Test <- cbind(BBB_LSTM_AUX_Test[,c(1,2,3)],dummy_cols(BBB_LSTM_AUX_Test[,-c(1,2,3)]))
BBB_LSTM_AUX_Test$year_2013 <- 0
BBB_LSTM_AUX_Test$semester_B <- 0
BBB_LSTM_AUX_Test <- BBB_LSTM_AUX_Test[,-c(4:7,10:12)]
BBB_LSTM_AUX_Test <- BBB_LSTM_AUX_Test[,c(1:28,31,29,32,30)]

# DDD
DDD_LSTM_AUX_Train <- Auxiliary_input[Auxiliary_input$code_module == "DDD" & Auxiliary_input$code_presentation != "2014J",
                                      -c(5,12)]
DDD_LSTM_AUX_Train <- DDD_LSTM_AUX_Train[order(DDD_LSTM_AUX_Train$code_module, DDD_LSTM_AUX_Train$code_presentation, DDD_LSTM_AUX_Train$id_student),]
DDD_LSTM_AUX_Train$year <- as.factor(DDD_LSTM_AUX_Train$year)
DDD_LSTM_AUX_Train <- cbind(DDD_LSTM_AUX_Train[,c(1,2,3)],dummy_cols(DDD_LSTM_AUX_Train[,-c(1,2,3)]))
DDD_LSTM_AUX_Train <- DDD_LSTM_AUX_Train[,-c(4:7,10:12)]


DDD_LSTM_AUX_Test <- Auxiliary_input[Auxiliary_input$code_module == "DDD" & Auxiliary_input$code_presentation == "2014J",
                                     -c(5,12)]
DDD_LSTM_AUX_Test <- DDD_LSTM_AUX_Test[order(DDD_LSTM_AUX_Test$code_module, DDD_LSTM_AUX_Test$code_presentation, DDD_LSTM_AUX_Test$id_student),]
DDD_LSTM_AUX_Test$year <- as.factor(DDD_LSTM_AUX_Test$year)
DDD_LSTM_AUX_Test <- cbind(DDD_LSTM_AUX_Test[,c(1,2,3)],dummy_cols(DDD_LSTM_AUX_Test[,-c(1,2,3)]))
DDD_LSTM_AUX_Test$year_2013 <- 0
DDD_LSTM_AUX_Test$semester_B <- 0
DDD_LSTM_AUX_Test <- DDD_LSTM_AUX_Test[,-c(4:7,10:12)]
DDD_LSTM_AUX_Test <- DDD_LSTM_AUX_Test[,c(1:28,31,29,32,30)]



# FFF
FFF_LSTM_AUX_Train <- Auxiliary_input[Auxiliary_input$code_module == "FFF" & Auxiliary_input$code_presentation != "2014J",
                                      -c(5,12)]
FFF_LSTM_AUX_Train <- FFF_LSTM_AUX_Train[order(FFF_LSTM_AUX_Train$code_module, FFF_LSTM_AUX_Train$code_presentation, FFF_LSTM_AUX_Train$id_student),]
FFF_LSTM_AUX_Train$year <- as.factor(FFF_LSTM_AUX_Train$year)
FFF_LSTM_AUX_Train <- cbind(FFF_LSTM_AUX_Train[,c(1,2,3)],dummy_cols(FFF_LSTM_AUX_Train[,-c(1,2,3)]))
FFF_LSTM_AUX_Train <- FFF_LSTM_AUX_Train[,-c(4:7,10:12)]


FFF_LSTM_AUX_Test <- Auxiliary_input[Auxiliary_input$code_module == "FFF" & Auxiliary_input$code_presentation == "2014J",
                                     -c(5,12)]
FFF_LSTM_AUX_Test <- FFF_LSTM_AUX_Test[order(FFF_LSTM_AUX_Test$code_module, FFF_LSTM_AUX_Test$code_presentation, FFF_LSTM_AUX_Test$id_student),]
FFF_LSTM_AUX_Test$year <- as.factor(FFF_LSTM_AUX_Test$year)
FFF_LSTM_AUX_Test <- cbind(FFF_LSTM_AUX_Test[,c(1,2,3)],dummy_cols(FFF_LSTM_AUX_Test[,-c(1,2,3)]))
FFF_LSTM_AUX_Test$year_2013 <- 0
FFF_LSTM_AUX_Test$semester_B <- 0
FFF_LSTM_AUX_Test <- FFF_LSTM_AUX_Test[,-c(4:7,10:12)]
FFF_LSTM_AUX_Test <- FFF_LSTM_AUX_Test[,c(1:28,31,29,32,30)]

write.csv(BBB_LSTM_AUX_Train, "BBB_LSTM_AUX_Train.csv")
write.csv(DDD_LSTM_AUX_Train, "DDD_LSTM_AUX_Train.csv")
write.csv(FFF_LSTM_AUX_Train, "FFF_LSTM_AUX_Train.csv")
write.csv(BBB_LSTM_AUX_Test, "BBB_LSTM_AUX_Test.csv")
write.csv(DDD_LSTM_AUX_Test, "DDD_LSTM_AUX_Test.csv")
write.csv(FFF_LSTM_AUX_Test, "FFF_LSTM_AUX_Test.csv")
