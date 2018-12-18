#setwd("D:/School/BISS/MSc Thesis/Data")
setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
rm(list=ls())

#install.packages("dplyr")
#install.packages("e1071")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("nnet")
#install.packages("gridExtra")
library("dplyr")
library("e1071")
library("caret")
library("randomForest")
library("nnet")
library("gridExtra")

studentInfo <- read.csv("studentInfo_v3.csv")
studentInfo$X <- NULL
studentInfo$id_student <- as.character(studentInfo$id_student)
studentInfo <- studentInfo[studentInfo$final_result != "Withdrawn",]
studentInfo <- droplevels(studentInfo)
studentInfo <- studentInfo[studentInfo$code_module %in% c("BBB", "DDD", "FFF"),]

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

studentInfo$num_of_prev_attempts <- range01(studentInfo$num_of_prev_attempts)
studentInfo$studied_credits <- range01(studentInfo$studied_credits)

studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation !="2014J",13:23] <- range01(studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation !="2014J",13:23])
studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation =="2014J",13:23] <- range01(studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation =="2014J",13:23])
studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation !="2014J",13:23] <- range01(studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation !="2014J",13:23])
studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation =="2014J",13:23] <- range01(studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation =="2014J",13:23])
studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation !="2014J",13:23] <- range01(studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation !="2014J",13:23])
studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation =="2014J",13:23] <- range01(studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation =="2014J",13:23])

studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation !="2014J",24:34] <- range01(studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation !="2014J",24:34])
studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation =="2014J",24:34] <- range01(studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation =="2014J",24:34])
studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation !="2014J",24:34] <- range01(studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation !="2014J",24:34])
studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation =="2014J",24:34] <- range01(studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation =="2014J",24:34])
studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation !="2014J",24:34] <- range01(studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation !="2014J",24:34])
studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation =="2014J",24:34] <- range01(studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation =="2014J",24:34])

studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation !="2014J",35:45] <- range01(studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation !="2014J",35:45])
studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation =="2014J",35:45] <- range01(studentInfo[studentInfo$code_module == "BBB" & studentInfo$code_presentation =="2014J",35:45])
studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation !="2014J",35:45] <- range01(studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation !="2014J",35:45])
studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation =="2014J",35:45] <- range01(studentInfo[studentInfo$code_module == "DDD" & studentInfo$code_presentation =="2014J",35:45])
studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation !="2014J",35:45] <- range01(studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation !="2014J",35:45])
studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation =="2014J",35:45] <- range01(studentInfo[studentInfo$code_module == "FFF" & studentInfo$code_presentation =="2014J",35:45])



# Datasets pre-course
studentInfo_PreCourse <- studentInfo[,c(1:13,24,35,46,47)]
studentInfo_PreCourse_BBB <- studentInfo_PreCourse[studentInfo_PreCourse$code_module == "BBB",]
studentInfo_PreCourse_DDD <- studentInfo_PreCourse[studentInfo_PreCourse$code_module == "DDD",]
studentInfo_PreCourse_FFF <- studentInfo_PreCourse[studentInfo_PreCourse$code_module == "FFF",]


Deciles <- c(0:10)
dataframes <- replicate(length(Deciles),data.frame())
names(dataframes) <- c("studentInfo_PreCourse", "studentInfo_Decile1", "studentInfo_Decile2", "studentInfo_Decile3"
                       , "studentInfo_Decile4", "studentInfo_Decile5", "studentInfo_Decile6", "studentInfo_Decile7"
                       , "studentInfo_Decile8", "studentInfo_Decile9", "studentInfo_Decile10")

dataframes_BBB <- replicate(length(Deciles),data.frame())
names(dataframes_BBB) <- c("studentInfo_PreCourse_BBB", "studentInfo_Decile1_BBB", "studentInfo_Decile2_BBB", "studentInfo_Decile3_BBB"
                       , "studentInfo_Decile4_BBB", "studentInfo_Decile5_BBB", "studentInfo_Decile6_BBB", "studentInfo_Decile7_BBB"
                       , "studentInfo_Decile8_BBB", "studentInfo_Decile9_BBB", "studentInfo_Decile10_BBB")

dataframes_DDD <- replicate(length(Deciles),data.frame())
names(dataframes_DDD) <- c("studentInfo_PreCourse_DDD", "studentInfo_Decile1_DDD", "studentInfo_Decile2_DDD", "studentInfo_Decile3_DDD"
                           , "studentInfo_Decile4_DDD", "studentInfo_Decile5_DDD", "studentInfo_Decile6_DDD", "studentInfo_Decile7_DDD"
                           , "studentInfo_Decile8_DDD", "studentInfo_Decile9_DDD", "studentInfo_Decile10_DDD")

dataframes_FFF <- replicate(length(Deciles),data.frame())
names(dataframes_FFF) <- c("studentInfo_PreCourse_FFF", "studentInfo_Decile1_FFF", "studentInfo_Decile2_FFF", "studentInfo_Decile3_FFF"
                           , "studentInfo_Decile4_FFF", "studentInfo_Decile5_FFF", "studentInfo_Decile6_FFF", "studentInfo_Decile7_FFF"
                           , "studentInfo_Decile8_FFF", "studentInfo_Decile9_FFF", "studentInfo_Decile10_FFF")

dataframes_BBB_Train <- replicate(length(Deciles),data.frame())
names(dataframes_BBB_Train) <- c("studentInfo_PreCourse_BBB_Train", "studentInfo_Decile1_BBB_Train", "studentInfo_Decile2_BBB_Train", "studentInfo_Decile3_BBB_Train"
                           , "studentInfo_Decile4_BBB_Train", "studentInfo_Decile5_BBB_Train", "studentInfo_Decile6_BBB_Train", "studentInfo_Decile7_BBB_Train"
                           , "studentInfo_Decile8_BBB_Train", "studentInfo_Decile9_BBB_Train", "studentInfo_Decile10_BBB_Train")
dataframes_BBB_Test <- replicate(length(Deciles),data.frame())
names(dataframes_BBB_Test) <- c("studentInfo_PreCourse_BBB_Test", "studentInfo_Decile1_BBB_Test", "studentInfo_Decile2_BBB_Test", "studentInfo_Decile3_BBB_Test"
                                 , "studentInfo_Decile4_BBB_Test", "studentInfo_Decile5_BBB_Test", "studentInfo_Decile6_BBB_Test", "studentInfo_Decile7_BBB_Test"
                                 , "studentInfo_Decile8_BBB_Test", "studentInfo_Decile9_BBB_Test", "studentInfo_Decile10_BBB_Test")

dataframes_DDD_Train <- replicate(length(Deciles),data.frame())
names(dataframes_DDD_Train) <- c("studentInfo_PreCourse_DDD_Train", "studentInfo_Decile1_DDD_Train", "studentInfo_Decile2_DDD_Train", "studentInfo_Decile3_DDD_Train"
                                 , "studentInfo_Decile4_DDD_Train", "studentInfo_Decile5_DDD_Train", "studentInfo_Decile6_DDD_Train", "studentInfo_Decile7_DDD_Train"
                                 , "studentInfo_Decile8_DDD_Train", "studentInfo_Decile9_DDD_Train", "studentInfo_Decile10_DDD_Train")
dataframes_DDD_Test <- replicate(length(Deciles),data.frame())
names(dataframes_DDD_Test) <- c("studentInfo_PreCourse_DDD_Test", "studentInfo_Decile1_DDD_Test", "studentInfo_Decile2_DDD_Test", "studentInfo_Decile3_DDD_Test"
                                , "studentInfo_Decile4_DDD_Test", "studentInfo_Decile5_DDD_Test", "studentInfo_Decile6_DDD_Test", "studentInfo_Decile7_DDD_Test"
                                , "studentInfo_Decile8_DDD_Test", "studentInfo_Decile9_DDD_Test", "studentInfo_Decile10_DDD_Test")

dataframes_FFF_Train <- replicate(length(Deciles),data.frame())
names(dataframes_FFF_Train) <- c("studentInfo_PreCourse_FFF_Train", "studentInfo_Decile1_FFF_Train", "studentInfo_Decile2_FFF_Train", "studentInfo_Decile3_FFF_Train"
                                 , "studentInfo_Decile4_FFF_Train", "studentInfo_Decile5_FFF_Train", "studentInfo_Decile6_FFF_Train", "studentInfo_Decile7_FFF_Train"
                                 , "studentInfo_Decile8_FFF_Train", "studentInfo_Decile9_FFF_Train", "studentInfo_Decile10_FFF_Train")
dataframes_FFF_Test <- replicate(length(Deciles),data.frame())
names(dataframes_FFF_Test) <- c("studentInfo_PreCourse_FFF_Test", "studentInfo_Decile1_FFF_Test", "studentInfo_Decile2_FFF_Test", "studentInfo_Decile3_FFF_Test"
                                , "studentInfo_Decile4_FFF_Test", "studentInfo_Decile5_FFF_Test", "studentInfo_Decile6_FFF_Test", "studentInfo_Decile7_FFF_Test"
                                , "studentInfo_Decile8_FFF_Test", "studentInfo_Decile9_FFF_Test", "studentInfo_Decile10_FFF_Test")

















# MODELING =====================================================================================================


# Final Results: Distinction / Pass / Fail ---------------------------------------------------------------------





# FULL DATA (STUDENT BACKGROUND + CLICKS + ASSIGNMENTS + ASSIGNMENT SCORES) ------------------------------------
for (i in Deciles){
  dataframes[[i+1]] <- studentInfo[,c(1:(13+i),24:(24+i),35:(35+i),46,47)]
  print(paste("Dataframe",i+1,"Created"))
  dataframes_BBB[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "BBB"),]
  dataframes_DDD[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "DDD"),]
  dataframes_FFF[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "FFF"),]
  print(paste("BBB, DDD and FFF Dataframe",i+1,"Created"))
  
  dataframes_BBB_Train[[i+1]] <- dataframes_BBB[[i+1]][which(dataframes_BBB[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  dataframes_BBB_Test[[i+1]] <- dataframes_BBB[[i+1]][which(dataframes_BBB[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  dataframes_DDD_Train[[i+1]] <- dataframes_DDD[[i+1]][which(dataframes_DDD[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  print(paste("BBB, DDD and FFF Training Dataframe",i+1,"Created"))
  dataframes_DDD_Test[[i+1]] <- dataframes_DDD[[i+1]][which(dataframes_DDD[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  dataframes_FFF_Train[[i+1]] <- dataframes_FFF[[i+1]][which(dataframes_FFF[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  dataframes_FFF_Test[[i+1]] <- dataframes_FFF[[i+1]][which(dataframes_FFF[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  print(paste("BBB, DDD and FFF Testing Dataframe",i+1,"Created"))
}

# (Multinomial) Logistic Regression ----------------------------------------------------------------------------

LogReg_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                     "Decile8", "Decile9", "Decile10")
LogReg_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                            "Decile8", "Decile9", "Decile10")
LogReg_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                          "Decile8", "Decile9", "Decile10")
LogReg_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")


# Logistic Regression BBB
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_BBB_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_BBB_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_BBB_Test[[i]]$final_result)
  LogReg_Accuracy[1,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[1,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[1,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[1,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}


# Logistic Regression DDD
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_DDD_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_DDD_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_DDD_Test[[i]]$final_result)
  LogReg_Accuracy[2,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[2,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[2,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[2,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}


# Logistic Regression FFF
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_FFF_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_FFF_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_FFF_Test[[i]]$final_result)
  LogReg_Accuracy[3,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[3,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[3,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[3,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}
mlr_cM


# Support Vector Machine ----------------------------------------------------------------------------

svm_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                            "Decile8", "Decile9", "Decile10")
svm_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")
svm_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                          "Decile8", "Decile9", "Decile10")
svm_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")

# Support Vector Machine
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_BBB_Train[[i]])
  svm_predict <- predict(svm, dataframes_BBB_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_BBB_Test[[i]]$final_result)
  svm_Accuracy[1,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[1,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[1,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[1,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}

# Support Vector Machine DDD
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_DDD_Train[[i]])
  svm_predict <- predict(svm, dataframes_DDD_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_DDD_Test[[i]]$final_result)
  svm_Accuracy[2,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[2,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[2,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[2,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}


# Support Vector Machine FFF
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_FFF_Train[[i]])
  svm_predict <- predict(svm, dataframes_FFF_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_FFF_Test[[i]]$final_result)
  svm_Accuracy[3,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[3,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[3,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[3,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}



# Naive Bayes Classifier ----------------------------------------------------------------------------

Bayes_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                         "Decile8", "Decile9", "Decile10")
Bayes_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")
Bayes_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")
Bayes_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")

# Naive Bayes Classifier BBB
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_BBB_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_BBB_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_BBB_Test[[i]]$final_result)
  Bayes_Accuracy[1,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[1,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[1,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[1,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}

# Naive Bayes Classifier DDD
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_DDD_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_DDD_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_DDD_Test[[i]]$final_result)
  Bayes_Accuracy[2,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[2,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[2,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[2,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}


# Naive Bayes Classifier FFF
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_FFF_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_FFF_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_FFF_Test[[i]]$final_result)
  Bayes_Accuracy[3,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[3,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[3,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[3,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}



# Random Forest -------------------------------------------------------------------------------------

RF_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                           "Decile8", "Decile9", "Decile10")
RF_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")
RF_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                          "Decile8", "Decile9", "Decile10")
RF_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")


# Random Forest BBB
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_BBB_Train[[i]])
  RF_predict <- predict(RF, dataframes_BBB_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_BBB_Test[[i]]$final_result)
  RF_Accuracy[1,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[1,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[1,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[1,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}

varImpPlot(RF)

# Random Forest DDD
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_DDD_Train[[i]])
  RF_predict <- predict(RF, dataframes_DDD_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_DDD_Test[[i]]$final_result)
  RF_Accuracy[2,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[2,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[2,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[2,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF)

# Random Forest Machine FFF
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_FFF_Train[[i]])
  RF_predict <- predict(RF, dataframes_FFF_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_FFF_Test[[i]]$final_result)
  RF_Accuracy[3,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[3,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[3,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[3,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF, main = "Predictive power variables in course FFF")


# Artificial Neural Network -------------------------------------------------------------------------

ANN_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                        "Decile8", "Decile9", "Decile10")
ANN_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")
ANN_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                          "Decile8", "Decile9", "Decile10")
ANN_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")


# Artificial Neural Network BBB
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_BBB_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_BBB_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_BBB_Test[[i]]$final_result)
  ANN_Accuracy[1,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[1,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[1,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[1,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}

# Artificial Neural Network DDD
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_DDD_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_DDD_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_DDD_Test[[i]]$final_result)
  ANN_Accuracy[2,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[2,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[2,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[2,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}

# Artificial Neural Network FFF
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_FFF_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_FFF_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_FFF_Test[[i]]$final_result)
  ANN_Accuracy[3,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[3,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[3,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[3,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}


#png("LogReg_Accuracy.png", height = 50*nrow(LogReg_Accuracy), width = 200*ncol(LogReg_Accuracy))
#grid.table(LogReg_Accuracy)
#dev.off()

write.csv(LogReg_Accuracy, "LogReg_Accuracy_wDistinction_Full.csv")
write.csv(LogReg_Distinction_Sensitivity, "LogReg_Distinction_Sensitivity_wDistinction_Full.csv")
write.csv(LogReg_Fail_Sensitivity, "LogReg_Fail_Sensitivity_wDistinction_Full.csv")
write.csv(LogReg_Pass_Sensitivity, "LogReg_Pass_Sensitivity_wDistinction_Full.csv")

write.csv(svm_Accuracy, "svm_Accuracy_wDistinction_Full.csv")
write.csv(svm_Distinction_Sensitivity, "svm_Distinction_Sensitivity_wDistinction_Full.csv")
write.csv(svm_Fail_Sensitivity, "svm_Fail_Sensitivity_wDistinction_Full.csv")
write.csv(svm_Pass_Sensitivity, "svm_Pass_Sensitivity_wDistinction_Full.csv")

write.csv(Bayes_Accuracy, "Bayes_Accuracy_wDistinction_Full.csv")
write.csv(Bayes_Distinction_Sensitivity, "Bayes_Distinction_Sensitivity_wDistinction_Full.csv")
write.csv(Bayes_Fail_Sensitivity, "Bayes_Fail_Sensitivity_wDistinction_Full.csv")
write.csv(Bayes_Pass_Sensitivity, "Bayes_Pass_Sensitivity_wDistinction_Full.csv")

write.csv(RF_Accuracy, "RF_Accuracy_wDistinction_Full.csv")
write.csv(RF_Distinction_Sensitivity, "RF_Distinction_Sensitivity_wDistinction_Full.csv")
write.csv(RF_Fail_Sensitivity, "RF_Fail_Sensitivity_wDistinction_Full.csv")
write.csv(RF_Pass_Sensitivity, "RF_Pass_Sensitivity_wDistinction_Full.csv")

write.csv(ANN_Accuracy, "ANN_Accuracy_wDistinction_Full.csv")
write.csv(ANN_Distinction_Sensitivity, "ANN_Distinction_Sensitivity_wDistinction_Full.csv")
write.csv(ANN_Fail_Sensitivity, "ANN_Fail_Sensitivity_wDistinction_Full.csv")
write.csv(ANN_Pass_Sensitivity, "ANN_Pass_Sensitivity_wDistinction_Full.csv")























# PARTIAL DATA (STUDENT BACKGROUND + CLICKS - ASSIGNMENTS - ASSIGNMENT SCORES) ------------------------------------
for (i in Deciles){
  dataframes[[i+1]] <- studentInfo[,c(1:(13+i),46,47)]
  print(paste("Dataframe",i+1,"Created"))
  dataframes_BBB[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "BBB"),]
  dataframes_DDD[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "DDD"),]
  dataframes_FFF[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "FFF"),]
  print(paste("BBB, DDD and FFF Dataframe",i+1,"Created"))
  
  dataframes_BBB_Train[[i+1]] <- dataframes_BBB[[i+1]][which(dataframes_BBB[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  dataframes_BBB_Test[[i+1]] <- dataframes_BBB[[i+1]][which(dataframes_BBB[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  dataframes_DDD_Train[[i+1]] <- dataframes_DDD[[i+1]][which(dataframes_DDD[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  print(paste("BBB, DDD and FFF Training Dataframe",i+1,"Created"))
  dataframes_DDD_Test[[i+1]] <- dataframes_DDD[[i+1]][which(dataframes_DDD[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  dataframes_FFF_Train[[i+1]] <- dataframes_FFF[[i+1]][which(dataframes_FFF[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  dataframes_FFF_Test[[i+1]] <- dataframes_FFF[[i+1]][which(dataframes_FFF[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  print(paste("BBB, DDD and FFF Testing Dataframe",i+1,"Created"))
}



# (Multinomial) Logistic Regression ----------------------------------------------------------------------------

LogReg_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                            "Decile8", "Decile9", "Decile10")
LogReg_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")
LogReg_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                           "Decile8", "Decile9", "Decile10")
LogReg_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")


# Logistic Regression BBB
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_BBB_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_BBB_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_BBB_Test[[i]]$final_result)
  LogReg_Accuracy[1,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[1,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[1,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[1,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}
mlr_cM$overall[[1]]

# Logistic Regression DDD
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_DDD_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_DDD_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_DDD_Test[[i]]$final_result)
  LogReg_Accuracy[2,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[2,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[2,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[2,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}


# Logistic Regression FFF
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_FFF_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_FFF_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_FFF_Test[[i]]$final_result)
  LogReg_Accuracy[3,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[3,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[3,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[3,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}


# Support Vector Machine ----------------------------------------------------------------------------

svm_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                         "Decile8", "Decile9", "Decile10")
svm_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")
svm_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                        "Decile8", "Decile9", "Decile10")
svm_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")

# Support Vector Machine
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_BBB_Train[[i]])
  svm_predict <- predict(svm, dataframes_BBB_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_BBB_Test[[i]]$final_result)
  svm_Accuracy[1,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[1,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[1,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[1,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}

# Support Vector Machine DDD
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_DDD_Train[[i]])
  svm_predict <- predict(svm, dataframes_DDD_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_DDD_Test[[i]]$final_result)
  svm_Accuracy[2,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[2,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[2,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[2,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}


# Support Vector Machine FFF
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_FFF_Train[[i]])
  svm_predict <- predict(svm, dataframes_FFF_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_FFF_Test[[i]]$final_result)
  svm_Accuracy[3,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[3,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[3,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[3,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}



# Naive Bayes Classifier ----------------------------------------------------------------------------

Bayes_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                           "Decile8", "Decile9", "Decile10")
Bayes_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")
Bayes_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                          "Decile8", "Decile9", "Decile10")
Bayes_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")

# Naive Bayes Classifier BBB
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_BBB_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_BBB_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_BBB_Test[[i]]$final_result)
  Bayes_Accuracy[1,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[1,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[1,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[1,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}

# Naive Bayes Classifier DDD
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_DDD_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_DDD_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_DDD_Test[[i]]$final_result)
  Bayes_Accuracy[2,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[2,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[2,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[2,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}


# Naive Bayes Classifier FFF
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_FFF_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_FFF_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_FFF_Test[[i]]$final_result)
  Bayes_Accuracy[3,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[3,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[3,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[3,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}



# Random Forest -------------------------------------------------------------------------------------

RF_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                        "Decile8", "Decile9", "Decile10")
RF_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                "Decile8", "Decile9", "Decile10")
RF_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                       "Decile8", "Decile9", "Decile10")
RF_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                "Decile8", "Decile9", "Decile10")


# Random Forest BBB
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_BBB_Train[[i]])
  RF_predict <- predict(RF, dataframes_BBB_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_BBB_Test[[i]]$final_result)
  RF_Accuracy[1,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[1,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[1,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[1,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF)

# Random Forest DDD
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_DDD_Train[[i]])
  RF_predict <- predict(RF, dataframes_DDD_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_DDD_Test[[i]]$final_result)
  RF_Accuracy[2,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[2,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[2,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[2,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF)


# Random Forest Machine FFF
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_FFF_Train[[i]])
  RF_predict <- predict(RF, dataframes_FFF_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_FFF_Test[[i]]$final_result)
  RF_Accuracy[3,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[3,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[3,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[3,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF)


# Artificial Neural Network -------------------------------------------------------------------------

ANN_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                         "Decile8", "Decile9", "Decile10")
ANN_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")
ANN_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                        "Decile8", "Decile9", "Decile10")
ANN_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")


# Artificial Neural Network BBB
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_BBB_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_BBB_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_BBB_Test[[i]]$final_result)
  ANN_Accuracy[1,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[1,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[1,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[1,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}

# Artificial Neural Network DDD
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_DDD_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_DDD_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_DDD_Test[[i]]$final_result)
  ANN_Accuracy[2,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[2,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[2,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[2,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}

# Artificial Neural Network FFF
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_FFF_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_FFF_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_FFF_Test[[i]]$final_result)
  ANN_Accuracy[3,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[3,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[3,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[3,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}


write.csv(LogReg_Accuracy, "LogReg_Accuracy_wDistinction_PartialClicks.csv")
write.csv(LogReg_Distinction_Sensitivity, "LogReg_Distinction_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(LogReg_Fail_Sensitivity, "LogReg_Fail_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(LogReg_Pass_Sensitivity, "LogReg_Pass_Sensitivity_wDistinction_PartialClicks.csv")

write.csv(svm_Accuracy, "svm_Accuracy_wDistinction_PartialClicks.csv")
write.csv(svm_Distinction_Sensitivity, "svm_Distinction_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(svm_Fail_Sensitivity, "svm_Fail_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(svm_Pass_Sensitivity, "svm_Pass_Sensitivity_wDistinction_PartialClicks.csv")

write.csv(Bayes_Accuracy, "Bayes_Accuracy_wDistinction_PartialClicks.csv")
write.csv(Bayes_Distinction_Sensitivity, "Bayes_Distinction_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(Bayes_Fail_Sensitivity, "Bayes_Fail_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(Bayes_Pass_Sensitivity, "Bayes_Pass_Sensitivity_wDistinction_PartialClicks.csv")

write.csv(RF_Accuracy, "RF_Accuracy_wDistinction_PartialClicks.csv")
write.csv(RF_Distinction_Sensitivity, "RF_Distinction_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(RF_Fail_Sensitivity, "RF_Fail_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(RF_Pass_Sensitivity, "RF_Pass_Sensitivity_wDistinction_PartialClicks.csv")

write.csv(ANN_Accuracy, "ANN_Accuracy_wDistinction_PartialClicks.csv")
write.csv(ANN_Distinction_Sensitivity, "ANN_Distinction_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(ANN_Fail_Sensitivity, "ANN_Fail_Sensitivity_wDistinction_PartialClicks.csv")
write.csv(ANN_Pass_Sensitivity, "ANN_Pass_Sensitivity_wDistinction_PartialClicks.csv")















# PARTIAL DATA (STUDENT BACKGROUND + ASSIGNMENTS + ASSIGNMENT SCORES - CLICKS) ------------------------------------
for (i in Deciles){
  dataframes[[i+1]] <- studentInfo[,c(1:12,24:(24+i),35:(35+i),46,47)]
  print(paste("Dataframe",i+1,"Created"))
  dataframes_BBB[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "BBB"),]
  dataframes_DDD[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "DDD"),]
  dataframes_FFF[[i+1]] <- dataframes[[i+1]][which(dataframes[[1]]$code_module == "FFF"),]
  print(paste("BBB, DDD and FFF Dataframe",i+1,"Created"))
  
  dataframes_BBB_Train[[i+1]] <- dataframes_BBB[[i+1]][which(dataframes_BBB[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  dataframes_BBB_Test[[i+1]] <- dataframes_BBB[[i+1]][which(dataframes_BBB[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  dataframes_DDD_Train[[i+1]] <- dataframes_DDD[[i+1]][which(dataframes_DDD[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  print(paste("BBB, DDD and FFF Training Dataframe",i+1,"Created"))
  dataframes_DDD_Test[[i+1]] <- dataframes_DDD[[i+1]][which(dataframes_DDD[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  dataframes_FFF_Train[[i+1]] <- dataframes_FFF[[i+1]][which(dataframes_FFF[[i+1]]$code_presentation != "2014J"),-c(2,3,5)]
  dataframes_FFF_Test[[i+1]] <- dataframes_FFF[[i+1]][which(dataframes_FFF[[i+1]]$code_presentation == "2014J"),-c(2,3,5)]
  print(paste("BBB, DDD and FFF Testing Dataframe",i+1,"Created"))
}



# (Multinomial) Logistic Regression ----------------------------------------------------------------------------

LogReg_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                            "Decile8", "Decile9", "Decile10")
LogReg_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")
LogReg_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                           "Decile8", "Decile9", "Decile10")
LogReg_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(LogReg_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                    "Decile8", "Decile9", "Decile10")


# Logistic Regression BBB
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_BBB_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_BBB_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_BBB_Test[[i]]$final_result)
  LogReg_Accuracy[1,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[1,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[1,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[1,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}
mlr_cM$overall[[1]]

# Logistic Regression DDD
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_DDD_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_DDD_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_DDD_Test[[i]]$final_result)
  LogReg_Accuracy[2,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[2,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[2,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[2,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}


# Logistic Regression FFF
for(i in 1:ncol(LogReg_Accuracy)){
  mlr <- multinom(final_result ~ ., data = dataframes_FFF_Train[[i]])
  mlr_predict <- predict(mlr, dataframes_FFF_Test[[i]], type = "class")
  mlr_cM <- confusionMatrix(mlr_predict, dataframes_FFF_Test[[i]]$final_result)
  LogReg_Accuracy[3,i] <- round(mlr_cM$overall[[1]],2)
  LogReg_Fail_Sensitivity[3,i] <- round(mlr_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  LogReg_Distinction_Sensitivity[3,i] <- round(mlr_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  LogReg_Pass_Sensitivity[3,i] <- round(mlr_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(mlr_cM$overall[1])
}


# Support Vector Machine ----------------------------------------------------------------------------

svm_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                         "Decile8", "Decile9", "Decile10")
svm_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")
svm_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                        "Decile8", "Decile9", "Decile10")
svm_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(svm_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")

# Support Vector Machine
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_BBB_Train[[i]])
  svm_predict <- predict(svm, dataframes_BBB_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_BBB_Test[[i]]$final_result)
  svm_Accuracy[1,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[1,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[1,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[1,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}

# Support Vector Machine DDD
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_DDD_Train[[i]])
  svm_predict <- predict(svm, dataframes_DDD_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_DDD_Test[[i]]$final_result)
  svm_Accuracy[2,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[2,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[2,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[2,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}


# Support Vector Machine FFF
for(i in 1:ncol(svm_Accuracy)){
  svm <- svm(final_result ~ ., data = dataframes_FFF_Train[[i]])
  svm_predict <- predict(svm, dataframes_FFF_Test[[i]])
  svm_cM <- confusionMatrix(svm_predict, dataframes_FFF_Test[[i]]$final_result)
  svm_Accuracy[3,i] <- round(svm_cM$overall[[1]],2)
  svm_Fail_Sensitivity[3,i] <- round(svm_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  svm_Distinction_Sensitivity[3,i] <- round(svm_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  svm_Pass_Sensitivity[3,i] <- round(svm_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(svm_cM$overall[1])
}



# Naive Bayes Classifier ----------------------------------------------------------------------------

Bayes_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                           "Decile8", "Decile9", "Decile10")
Bayes_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")
Bayes_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                          "Decile8", "Decile9", "Decile10")
Bayes_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(Bayes_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                   "Decile8", "Decile9", "Decile10")

# Naive Bayes Classifier BBB
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_BBB_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_BBB_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_BBB_Test[[i]]$final_result)
  Bayes_Accuracy[1,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[1,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[1,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[1,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}

# Naive Bayes Classifier DDD
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_DDD_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_DDD_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_DDD_Test[[i]]$final_result)
  Bayes_Accuracy[2,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[2,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[2,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[2,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}


# Naive Bayes Classifier FFF
for(i in 1:ncol(Bayes_Accuracy)){
  Bayes <- naiveBayes(final_result ~ ., data = dataframes_FFF_Train[[i]])
  Bayes_predict <- predict(Bayes, dataframes_FFF_Test[[i]])
  Bayes_cM <- confusionMatrix(Bayes_predict, dataframes_FFF_Test[[i]]$final_result)
  Bayes_Accuracy[3,i] <- round(Bayes_cM$overall[[1]],2)
  Bayes_Fail_Sensitivity[3,i] <- round(Bayes_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  Bayes_Distinction_Sensitivity[3,i] <- round(Bayes_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  Bayes_Pass_Sensitivity[3,i] <- round(Bayes_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(Bayes_cM$overall[1])
}



# Random Forest -------------------------------------------------------------------------------------

RF_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                        "Decile8", "Decile9", "Decile10")
RF_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                "Decile8", "Decile9", "Decile10")
RF_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                       "Decile8", "Decile9", "Decile10")
RF_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(RF_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                "Decile8", "Decile9", "Decile10")


# Random Forest BBB
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_BBB_Train[[i]])
  RF_predict <- predict(RF, dataframes_BBB_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_BBB_Test[[i]]$final_result)
  RF_Accuracy[1,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[1,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[1,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[1,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF)

# Random Forest DDD
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_DDD_Train[[i]])
  RF_predict <- predict(RF, dataframes_DDD_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_DDD_Test[[i]]$final_result)
  RF_Accuracy[2,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[2,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[2,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[2,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF)


# Random Forest Machine FFF
for(i in 1:ncol(RF_Accuracy)){
  RF <- randomForest(final_result ~ ., data = dataframes_FFF_Train[[i]])
  RF_predict <- predict(RF, dataframes_FFF_Test[[i]])
  RF_cM <- confusionMatrix(RF_predict, dataframes_FFF_Test[[i]]$final_result)
  RF_Accuracy[3,i] <- round(RF_cM$overall[[1]],2)
  RF_Fail_Sensitivity[3,i] <- round(RF_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  RF_Distinction_Sensitivity[3,i] <- round(RF_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  RF_Pass_Sensitivity[3,i] <- round(RF_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(RF_cM$overall[1])
}
varImpPlot(RF)


# Artificial Neural Network -------------------------------------------------------------------------

ANN_Accuracy <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Accuracy) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                         "Decile8", "Decile9", "Decile10")
ANN_Fail_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Fail_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")
ANN_Distinction_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Distinction_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                        "Decile8", "Decile9", "Decile10")
ANN_Pass_Sensitivity <- data.frame(rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3),rep(0,3), row.names = c("BBB","DDD","FFF"))
names(ANN_Pass_Sensitivity) <- c("PreCourse", "Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7",
                                 "Decile8", "Decile9", "Decile10")


# Artificial Neural Network BBB
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_BBB_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_BBB_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_BBB_Test[[i]]$final_result)
  ANN_Accuracy[1,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[1,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[1,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[1,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}

# Artificial Neural Network DDD
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_DDD_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_DDD_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_DDD_Test[[i]]$final_result)
  ANN_Accuracy[2,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[2,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[2,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[2,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}

# Artificial Neural Network FFF
for(i in 1:ncol(ANN_Accuracy)){
  ANN <- nnet(final_result ~ ., size = 10, data = dataframes_FFF_Train[[i]])
  ANN_predict <- predict(ANN, dataframes_FFF_Test[[i]], type = "class")
  ANN_cM <- confusionMatrix(as.factor(ANN_predict), dataframes_FFF_Test[[i]]$final_result)
  ANN_Accuracy[3,i] <- round(ANN_cM$overall[[1]],2)
  ANN_Fail_Sensitivity[3,i] <- round(ANN_cM$byClass[[2,1]],2) # Sensitivity "Fail" = The proportion of "Fails" correctly predicted out of total "Fails"
  ANN_Distinction_Sensitivity[3,i] <- round(ANN_cM$byClass[[1,1]],2) # Sensitivity "Distinction" = The proportion of "Distinction" correctly predicted out of total "Distinction"
  ANN_Pass_Sensitivity[3,i] <- round(ANN_cM$byClass[[3,1]],2) # Sensitivity "Pass" = The proportion of "Pass" correctly predicted out of total "Pass"
  print(ANN_cM$overall[1])
}



write.csv(LogReg_Accuracy, "LogReg_Accuracy_wDistinction_PartialAsgmts.csv")
write.csv(LogReg_Distinction_Sensitivity, "LogReg_Distinction_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(LogReg_Fail_Sensitivity, "LogReg_Fail_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(LogReg_Pass_Sensitivity, "LogReg_Pass_Sensitivity_wDistinction_PartialAsgmts.csv")

write.csv(svm_Accuracy, "svm_Accuracy_wDistinction_PartialAsgmts.csv")
write.csv(svm_Distinction_Sensitivity, "svm_Distinction_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(svm_Fail_Sensitivity, "svm_Fail_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(svm_Pass_Sensitivity, "svm_Pass_Sensitivity_wDistinction_PartialAsgmts.csv")

write.csv(Bayes_Accuracy, "Bayes_Accuracy_wDistinction_PartialAsgmts.csv")
write.csv(Bayes_Distinction_Sensitivity, "Bayes_Distinction_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(Bayes_Fail_Sensitivity, "Bayes_Fail_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(Bayes_Pass_Sensitivity, "Bayes_Pass_Sensitivity_wDistinction_PartialAsgmts.csv")

write.csv(RF_Accuracy, "RF_Accuracy_wDistinction_PartialAsgmts.csv")
write.csv(RF_Distinction_Sensitivity, "RF_Distinction_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(RF_Fail_Sensitivity, "RF_Fail_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(RF_Pass_Sensitivity, "RF_Pass_Sensitivity_wDistinction_PartialAsgmts.csv")

write.csv(ANN_Accuracy, "ANN_Accuracy_wDistinction_PartialAsgmts.csv")
write.csv(ANN_Distinction_Sensitivity, "ANN_Distinction_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(ANN_Fail_Sensitivity, "ANN_Fail_Sensitivity_wDistinction_PartialAsgmts.csv")
write.csv(ANN_Pass_Sensitivity, "ANN_Pass_Sensitivity_wDistinction_PartialAsgmts.csv")