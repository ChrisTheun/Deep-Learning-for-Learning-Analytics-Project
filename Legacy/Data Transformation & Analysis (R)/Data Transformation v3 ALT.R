setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
rm(list=ls())

#install.packages("dplyr")
#install.packages("e1071")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("nnet")
#install.packages("fastDummies)
library("dplyr")
library("e1071")
library("caret")
library("randomForest")
library("nnet")
#library("zoo")
library("fastDummies")

assessments <- read.csv("assessments.csv")
courses <- read.csv("courses.csv")
studentAssessment <- read.csv("studentAssessment.csv")
studentInfo <- read.csv("studentInfo.csv")
studentRegistration <- read.csv("studentRegistration.csv")
studentVle <- read.csv("studentVle.csv")
vle <- read.csv("vle.csv")
studentInfo_Original <- studentInfo


# FFF and BBB are the courses that have the most students overall and are given in all 4 semesters

# ===============================================================================================
# DATA TRANSFORMATION

# Merge dataframes
FullAssessments <- merge(x = studentAssessment, y = assessments, by = "id_assessment")
FullVle <- merge(x = studentVle, y = vle[,c(1,4)], by = "id_site")


# MONTHLY NUMBER OF CLICKS




# Impute missing exam dates with the final day of the course 
# [Official documentation: "If the information about the final exam date is missing, it is at the end of the last presentation week."]
for(i in 1:nrow(assessments)){
  m <- as.character(assessments$code_module[i])
  p <- as.character(assessments$code_presentation[i])
  if (is.na(assessments$date[i]) & assessments$assessment_type[i] == "Exam"){
    assessments$date[i] <- courses$module_presentation_length[courses$code_module == m & courses$code_presentation == p]
  }
}

# Remove clicks that have been made on or after the final exam date conditional on course and semester
FullVle <- merge(x = FullVle, y = assessments[assessments$assessment_type == "Exam",c(1,2,5)], by = c("code_module", "code_presentation"), all.x = TRUE)
FullVle <- FullVle[!duplicated(FullVle),]
names(FullVle)[c(5,8)] <- c("date","exam_date")
FullVle <- FullVle[FullVle$date < FullVle$exam_date,]


# Remove assessments that have been submitted on or after the final exam date conditional on course and semester
FullAssessments <- merge(x = FullAssessments, y = assessments[assessments$assessment_type == "Exam",c(1,2,5)], by = c("code_module", "code_presentation"), all.x = TRUE)
FullAssessments <- FullAssessments[!duplicated(FullAssessments),]
names(FullAssessments)[c(9,11)] <- c("date", "exam_date")
FullAssessments <- FullAssessments[FullAssessments$date_submitted < FullAssessments$exam_date,]




Min_date <- min(studentVle$date)
Max_dates <- aggregate(exam_date ~ code_module + code_presentation + id_student,data = FullAssessments[FullAssessments$code_module == "BBB"
                                                                                                       | FullAssessments$code_module == "DDD"
                                                                                                       | FullAssessments$code_module == "FFF",], FUN = mean)
Max_dates$exam_date <- Max_dates$exam_date
Max_dates$min_date <- Min_date
Max_dates$day <- Max_dates$exam_date - Max_dates$min_date

expanded <-Max_dates[rep(row.names(Max_dates), Max_dates$day),]
expanded$course_student <- paste(expanded$code_module,expanded$code_presentation, expanded$id_student)
expanded$date <- sequence(rle(as.character(expanded$course_student))$lengths)+expanded$min_date-1

SI <- expanded[,c(1,2,3,8)]

StudentClicks <- aggregate(sum_click ~ code_module + code_presentation + id_student + date, data = FullVle, FUN = sum)

SI_Clicks <- merge(x = SI, y = StudentClicks, all.x = TRUE, by = c("code_module", "code_presentation", "id_student", "date"))
SI_Clicks$sum_click[is.na(SI_Clicks$sum_click)] <- 0

sumAsgmts <- aggregate(id_assessment ~ code_module + code_presentation + id_student + date_submitted, data = FullAssessments, FUN = length)
names(sumAsgmts)[4:5] <- c("date","asgmts")
SI_Clicks_Asgmts <- merge(x = SI_Clicks, y = sumAsgmts, all.x = TRUE, by = c("code_module", "code_presentation", "id_student", "date"))
SI_Clicks_Asgmts$asgmts[is.na(SI_Clicks_Asgmts$asgmts)] <- 0

ScoreAsgmts <- aggregate(score ~ code_module + code_presentation + id_student + date_submitted, data = FullAssessments, FUN = sum)
names(ScoreAsgmts)[4] <- c("date")

studentInfo_days <- merge(x = SI_Clicks_Asgmts, y = ScoreAsgmts, all.x = TRUE, by = c("code_module", "code_presentation", "id_student", "date"))
studentInfo_days$score[is.na(studentInfo_days$score)] <- 0

studentInfo_days$identifier <- paste(studentInfo_days$code_module, studentInfo_days$code_presentation, studentInfo_days$id_student)

studentInfo_days$csum_score <- 0
studentInfo_days$csum_asgmts <- 0
studentInfo_days$Avg_score <- 0
studentInfo_days$csum_score <- ave(studentInfo_days$score, studentInfo_days$identifier, FUN = cumsum)
studentInfo_days$csum_asgmts <- ave(studentInfo_days$asgmts, studentInfo_days$identifier, FUN = cumsum)
studentInfo_days$Avg_score <- round(studentInfo_days$csum_score/studentInfo_days$csum_asgmts,2)
studentInfo_days$Avg_score[is.na(studentInfo_days$Avg_score)] <- 0


maxdate <-max(studentInfo_days$date)

maxdate_indiv <- studentInfo_days %>%
  group_by(identifier) %>%
  summarise(date = last(date))

mindate <- min(maxdate_indiv$date)

backup <- studentInfo_days
studentInfo_days <- backup

for (i in 1:(maxdate-mindate)){
  studentInfo_days <- studentInfo_days %>%
    group_by(identifier) %>%
    summarise(date = last(date)) %>%
    mutate(date = ifelse(date < maxdate, date+1, NA)) %>% 
    bind_rows(studentInfo_days, .) %>% 
    arrange(identifier)
  print(paste("Iteration",i,"out of",(maxdate-mindate),"Completed"))
}

studentInfo_days <- studentInfo_days[!is.na(studentInfo_days$date),]

for (i in 1:nrow(studentInfo_days)){
  if (is.na(studentInfo_days$code_module[i])){
    studentInfo_days$code_module[i] <- studentInfo_days$code_module[i-1]
  }
  if (is.na(studentInfo_days$code_presentation[i])){
    studentInfo_days$code_presentation[i] <- studentInfo_days$code_presentation[i-1]
  }
  if (is.na(studentInfo_days$id_student[i])){
    studentInfo_days$id_student[i] <- studentInfo_days$id_student[i-1]
  }
  if (is.na(studentInfo_days$sum_click[i])){
    studentInfo_days$sum_click[i] <- -1
  }
  if (is.na(studentInfo_days$asgmts[i])){
    studentInfo_days$asgmts[i] <- -1
  }
  if (is.na(studentInfo_days$Avg_score[i])){
    studentInfo_days$Avg_score[i] <- -1
  }
    print(paste("Completed iteration", i))
}




studentInfo_days <- studentInfo_days[,c(1:3,8,4,5,6,11)]
studentInfo_days <- merge(x = studentInfo_days, y = studentInfo, all.x = TRUE, by = c("code_module", "code_presentation", "id_student"))
studentInfo_days <- studentInfo_days[order(studentInfo_days$code_module, studentInfo_days$code_presentation, studentInfo_days$id_student, studentInfo_days$date),
                                     ]
studentInfo_days <- transform(studentInfo_days, year = substr(code_presentation,1,4), semester = substr(code_presentation,5,5))
studentInfo_days$year <- as.factor(studentInfo_days$year)
studentInfo_days <- dummy_cols(studentInfo_days, c("gender", "highest_education", "imd_band", "age_band", "disability", "year", "semester"))
studentInfo_days <- studentInfo_days[,c(1:8, 20:46, 14, 15, 17)]


#backup2 <- studentInfo_days 
#write.csv(backup2, "Backup_studentInfo_Alt.csv")
#studentInfo_days <- backup2



BBB_LSTM_Input_Train_ALT <- studentInfo_days[studentInfo_days$code_module == "BBB" & studentInfo_days$code_presentation != "2014J",]
BBB_LSTM_Input_Test_ALT <- studentInfo_days[studentInfo_days$code_module == "BBB" & studentInfo_days$code_presentation == "2014J",]

DDD_LSTM_Input_Train_ALT <- studentInfo_days[studentInfo_days$code_module == "DDD" & studentInfo_days$code_presentation != "2014J",]
DDD_LSTM_Input_Test_ALT <- studentInfo_days[studentInfo_days$code_module == "DDD" & studentInfo_days$code_presentation == "2014J",]

FFF_LSTM_Input_Train_ALT <- studentInfo_days[studentInfo_days$code_module == "FFF" & studentInfo_days$code_presentation != "2014J",]
FFF_LSTM_Input_Test_ALT <- studentInfo_days[studentInfo_days$code_module == "FFF" & studentInfo_days$code_presentation == "2014J",]

BBB_LSTM_Input_Train_ALT <- BBB_LSTM_Input_Train_ALT[,-c(1:3)]
BBB_LSTM_Input_Test_ALT <- BBB_LSTM_Input_Test_ALT[,-c(1:3)]
DDD_LSTM_Input_Train_ALT <- DDD_LSTM_Input_Train_ALT[,-c(1:3)]
DDD_LSTM_Input_Test_ALT <- DDD_LSTM_Input_Test_ALT[,-c(1:3)]
FFF_LSTM_Input_Train_ALT <- FFF_LSTM_Input_Train_ALT[,-c(1:3)]
FFF_LSTM_Input_Test_ALT <- FFF_LSTM_Input_Test_ALT[,-c(1:3)]


# Memory allocation issues
#range01 <- function(x){(x-min(x))/(max(x)-min(x))} # normalization between 0 and 1


#BBB_LSTM_Input_Train_ALT[BBB_LSTM_Input_Train_ALT$sum_click != -1,3] <- lapply(BBB_LSTM_Input_Train_ALT[BBB_LSTM_Input_Train_ALT$sum_click != -1,3], FUN = range01)
#BBB_LSTM_Input_Test_ALT$sum_click <- lapply(BBB_LSTM_Input_Test_ALT$sum_click, FUN = range01)
#BBB_LSTM_Input_Train_ALT$asgmts <- lapply(BBB_LSTM_Input_Train_ALT$asgmts, FUN = range01)
#BBB_LSTM_Input_Test_ALT$asgmts <- lapply(BBB_LSTM_Input_Test_ALT$asgmts, FUN = range01)
#BBB_LSTM_Input_Train_ALT$Avg_score <- lapply(BBB_LSTM_Input_Train_ALT$Avg_score, FUN = range01)
#BBB_LSTM_Input_Test_ALT$Avg_score <- lapply(BBB_LSTM_Input_Test_ALT$Avg_score, FUN = range01)
#BBB_LSTM_Input_Train_ALT$num_of_prev_attempts <- lapply(BBB_LSTM_Input_Train_ALT$num_of_prev_attempts, FUN = range01)
#BBB_LSTM_Input_Test_ALT$num_of_prev_attempts <- lapply(BBB_LSTM_Input_Test_ALT$num_of_prev_attempts, FUN = range01)
#BBB_LSTM_Input_Train_ALT$studied_credits <- lapply(BBB_LSTM_Input_Train_ALT$studied_credits, FUN = range01)
#BBB_LSTM_Input_Test_ALT$studied_credits <- lapply(BBB_LSTM_Input_Test_ALT$studied_credits, FUN = range01)

#DDD_LSTM_Input_Train_ALT$sum_click <- lapply(DDD_LSTM_Input_Train_ALT$sum_click, FUN = range01)
#DDD_LSTM_Input_Test_ALT$sum_click <- lapply(DDD_LSTM_Input_Test_ALT$sum_click, FUN = range01)
#DDD_LSTM_Input_Train_ALT$asgmts <- lapply(DDD_LSTM_Input_Train_ALT$asgmts, FUN = range01)
#DDD_LSTM_Input_Test_ALT$asgmts <- lapply(DDD_LSTM_Input_Test_ALT$asgmts, FUN = range01)
#DDD_LSTM_Input_Train_ALT$Avg_score <- lapply(DDD_LSTM_Input_Train_ALT$Avg_score, FUN = range01)
#DDD_LSTM_Input_Test_ALT$Avg_score <- lapply(DDD_LSTM_Input_Test_ALT$Avg_score, FUN = range01)
#DDD_LSTM_Input_Train_ALT$num_of_prev_attempts <- lapply(DDD_LSTM_Input_Train_ALT$num_of_prev_attempts, FUN = range01)
#DDD_LSTM_Input_Test_ALT$num_of_prev_attempts <- lapply(DDD_LSTM_Input_Test_ALT$num_of_prev_attempts, FUN = range01)
#DDD_LSTM_Input_Train_ALT$studied_credits <- lapply(DDD_LSTM_Input_Train_ALT$studied_credits, FUN = range01)
#DDD_LSTM_Input_Test_ALT$studied_credits <- lapply(DDD_LSTM_Input_Test_ALT$studied_credits, FUN = range01)

#FFF_LSTM_Input_Train_ALT$sum_click <- lapply(FFF_LSTM_Input_Train_ALT$sum_click, FUN = range01)
#FFF_LSTM_Input_Test_ALT$sum_click <- lapply(FFF_LSTM_Input_Test_ALT$sum_click, FUN = range01)
#FFF_LSTM_Input_Train_ALT$asgmts <- lapply(FFF_LSTM_Input_Train_ALT$asgmts, FUN = range01)
#FFF_LSTM_Input_Test_ALT$asgmts <- lapply(FFF_LSTM_Input_Test_ALT$asgmts, FUN = range01)
#FFF_LSTM_Input_Train_ALT$Avg_score <- lapply(FFF_LSTM_Input_Train_ALT$Avg_score, FUN = range01)
#FFF_LSTM_Input_Test_ALT$Avg_score <- lapply(FFF_LSTM_Input_Test_ALT$Avg_score, FUN = range01)
#FFF_LSTM_Input_Train_ALT$num_of_prev_attempts <- lapply(FFF_LSTM_Input_Train_ALT$num_of_prev_attempts, FUN = range01)
#FFF_LSTM_Input_Test_ALT$num_of_prev_attempts <- lapply(FFF_LSTM_Input_Test_ALT$num_of_prev_attempts, FUN = range01)
#FFF_LSTM_Input_Train_ALT$studied_credits <- lapply(FFF_LSTM_Input_Train_ALT$studied_credits, FUN = range01)
#FFF_LSTM_Input_Test_ALT$studied_credits <- lapply(FFF_LSTM_Input_Test_ALT$studied_credits, FUN = range01)




write.csv(BBB_LSTM_Input_Train_ALT, file = "BBB_LSTM_Train_ALT.csv")
write.csv(BBB_LSTM_Input_Test_ALT, file = "BBB_LSTM_Test_ALT.csv")
write.csv(DDD_LSTM_Input_Train_ALT, file = "DDD_LSTM_Train_ALT.csv")
write.csv(DDD_LSTM_Input_Test_ALT, file = "DDD_LSTM_Test_ALT.csv")
write.csv(FFF_LSTM_Input_Train_ALT, file = "FFF_LSTM_Train_ALT.csv")
write.csv(FFF_LSTM_Input_Test_ALT, file = "FFF_LSTM_Test_ALT.csv")


































