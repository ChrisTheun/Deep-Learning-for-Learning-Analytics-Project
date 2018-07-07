setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
rm(list=ls())

#install.packages("dplyr")
#install.packages("e1071")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("nnet")
library("dplyr")
library("e1071")
library("caret")
library("randomForest")
library("nnet")

assessments <- read.csv("assessments.csv")
courses <- read.csv("courses.csv")
studentAssessment <- read.csv("studentAssessment.csv")
studentInfo <- read.csv("studentInfo.csv")
studentRegistration <- read.csv("studentRegistration.csv")
studentVle <- read.csv("studentVle.csv")
vle <- read.csv("vle.csv")
studentInfo_Original <- studentInfo

# DATA SUMMARIES

by_course <- group_by(studentInfo, code_module)
summarise(by_course, total = n(), total_unique = n_distinct(id_student))

by_semester <- group_by(studentInfo, code_presentation)
summarise(by_semester, total = n(), total_unique = n_distinct(id_student))

by_course_semester <- group_by(studentInfo, code_module, code_presentation)
Summary_StudentInfo <- summarise(by_course_semester, total = n(), total_unique = n_distinct(id_student))

# FFF and BBB are the courses that have the most students overall and are given in all 4 semesters

# ===============================================================================================
# DATA TRANSFORMATION

# Merge dataframes
FullAssessments <- merge(x = studentAssessment, y = assessments, by = "id_assessment")
FullVle <- merge(x = studentVle, y = vle[,c(1,4)], by = "id_site")


# MONTHLY NUMBER OF CLICKS

# Calculate the number of clicks a student made in a particular course and semester
StudentClicks <- aggregate(sum_click ~ code_module + code_presentation + id_student, data = FullVle, FUN = sum)

# Assume every month is 30 days
#month_limit <- c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270)



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


# Calculate and store the number of clicks a student made in a particular course and semester before the start of the course (month 0)
Avg_Clicks_PreCourse <- aggregate(sum_click ~ code_module + code_presentation + id_student,
                           data = FullVle[FullVle$date < 0,],
                           FUN = mean)
names(Avg_Clicks_PreCourse)[4] <- "Avg_Clicks_PreCourse"

courses$Decile0 <- 0
courses$Decile1 <- as.integer((courses$module_presentation_length-1)/10)
courses$Decile2 <- as.integer((2*(courses$module_presentation_length-1))/10)
courses$Decile3 <- as.integer((3*(courses$module_presentation_length-1))/10)
courses$Decile4 <- as.integer((4*(courses$module_presentation_length-1))/10)
courses$Decile5 <- as.integer((5*(courses$module_presentation_length-1))/10)
courses$Decile6 <- as.integer((6*(courses$module_presentation_length-1))/10)
courses$Decile7 <- as.integer((7*(courses$module_presentation_length-1))/10)
courses$Decile8 <- as.integer((8*(courses$module_presentation_length-1))/10)
courses$Decile9 <- as.integer((9*(courses$module_presentation_length-1))/10)
courses$Decile10 <- as.integer((10*(courses$module_presentation_length-1))/10)


FullVle$Decile0 <- 0
FullVle$Decile1 <- as.integer((FullVle$exam_date-1)/10)
FullVle$Decile2 <- as.integer((2*(FullVle$exam_date-1))/10)
FullVle$Decile3 <- as.integer((3*(FullVle$exam_date-1))/10)
FullVle$Decile4 <- as.integer((4*(FullVle$exam_date-1))/10)
FullVle$Decile5 <- as.integer((5*(FullVle$exam_date-1))/10)
FullVle$Decile6 <- as.integer((6*(FullVle$exam_date-1))/10)
FullVle$Decile7 <- as.integer((7*(FullVle$exam_date-1))/10)
FullVle$Decile8 <- as.integer((8*(FullVle$exam_date-1))/10)
FullVle$Decile9 <- as.integer((9*(FullVle$exam_date-1))/10)
FullVle$Decile10 <- as.integer((10*(FullVle$exam_date-1))/10)


FullAssessments$Decile0 <- 0
FullAssessments$Decile1 <- as.integer((FullAssessments$exam_date-1)/10)
FullAssessments$Decile2 <- as.integer((2*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile3 <- as.integer((3*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile4 <- as.integer((4*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile5 <- as.integer((5*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile6 <- as.integer((6*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile7 <- as.integer((7*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile8 <- as.integer((8*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile9 <- as.integer((9*(FullAssessments$exam_date-1))/10)
FullAssessments$Decile10 <- as.integer((10*(FullAssessments$exam_date-1))/10)



Deciles <- c(0:9)

Clicks_dfs <- replicate(length(Deciles),data.frame())
names(Clicks_dfs) <- c("Clicks_Decile1", "Clicks_Decile2", "Clicks_Decile3","Clicks_Decile4","Clicks_Decile5","Clicks_Decile6",
                       "Clicks_Decile7","Clicks_Decile8","Clicks_Decile9", "Clicks_Decile10")
varnames <- c("Avg_Clicks_Decile1", "Avg_Clicks_Decile2", "Avg_Clicks_Decile3", "Avg_Clicks_Decile4", "Avg_Clicks_Decile5",
              "Avg_Clicks_Decile6", "Avg_Clicks_Decile7", "Avg_Clicks_Decile8", "Avg_Clicks_Decile9", "Avg_Clicks_Decile10")

for (i in Deciles){
  
Clicks_dfs[[i+1]] <- aggregate(sum_click ~ code_module + code_presentation + id_student,
                        data = FullVle[FullVle$date >= FullVle[,(9+i)] & FullVle$date < FullVle[,(10+i)],],
                        FUN = sum)
Clicks_dfs[[i+1]] <- merge(x = Clicks_dfs[[i+1]], y = courses[,c(1,2,4+i,5+i)], by = c("code_module", "code_presentation"), all.x = TRUE)
Clicks_dfs[[i+1]][,7] <- Clicks_dfs[[i+1]]$sum_click/(Clicks_dfs[[i+1]][,6]-Clicks_dfs[[i+1]][,5])
names(Clicks_dfs[[i+1]])[7] <- varnames[i+1]
Clicks_dfs[[i+1]][,4:6] <- NULL

print(paste("Dataframe Decile number", i+1, "created"))

}



# Merge all dataframes into a single dataframe containing the number of clicks per month for each student in a course and semester
Clicks_dfs <- c(list(Avg_Clicks_PreCourse), Clicks_dfs)
StudentClicks_Full <- Reduce(function(x,y) merge(x,y, all=TRUE), Clicks_dfs)
StudentClicks_Full[is.na(StudentClicks_Full)] <- 0

# Add the monthly number of clicks to the studentInfo dataframe
studentInfo <- merge(x = studentInfo, y = StudentClicks_Full, by = c("code_module", "code_presentation", "id_student"), all.x = TRUE)


# Plot the average monthly number of clicks for each student success level (pass, fail, pass w distinction and withdrawn)
plot(colMeans(studentInfo[studentInfo$final_result == "Pass",13:23], na.rm = TRUE), type = "l", col = "blue",
     ylab = "Average number of Clicks", xlab = "Decile", ylim = c(0,15))
lines(colMeans(studentInfo[studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))







# MONTHLY NUMBER AND AVG SCORE OF ASSESSMENTS

# Create 10 empty dataframes for the 10 deciles the courses take to store the amount of Asgmts submitted per month in
Asgmt_dfs <- replicate(length(Deciles),data.frame())
names(Asgmt_dfs) <- c("Asgmts_Decile1", "Asgmts_Decile2", "Asgmts_Decile3","Asgmts_Decile4","Asgmts_Decile5","Asgmts_Decile6",
                      "Asgmts_Decile7","Asgmts_Decile8","Asgmts_Decile9", "Asgmts_Decile10")

# Create 10 empty dataframes for the 10 Deciles the courses take to store the average grade of Asgmts submitted per Decile in
Asgmt_Score_dfs <- replicate(length(Deciles),data.frame())
names(Asgmt_Score_dfs) <- c("Avg_Asgmt_Score_Decile1", "Avg_Asgmt_Score_Decile2", "Avg_Asgmt_Score_Decile3","Avg_Asgmt_Score_Decile4","Avg_Asgmt_Score_Decile5","Avg_Asgmt_Score_Decile6",
                            "Avg_Asgmt_Score_Decile7","Avg_Asgmt_Score_Decile8","Avg_Asgmt_Score_Decile9", "Avg_Asgmt_Score_Decile10")


# Calculate and store the number of Asgmts a student submitted in a particular course and semester before the start of the course (Decile 0)
Asgmts_PreCourse <- aggregate(id_assessment ~ code_module + code_presentation + id_student,
                          data = FullAssessments[FullAssessments$date_submitted < 0,],
                          FUN = length)
names(Asgmts_PreCourse)[4] <- "Asgmts_PreCourse"

# Calculate and store the average score of Asgmts a student submitted in a particular course and semester before the start of the course (Decile 0)
Avg_Asgmt_Score_PreCourse <- aggregate(score ~ code_module + code_presentation + id_student,
                                data = FullAssessments[FullAssessments$date_submitted < 0,],
                                FUN = mean)
names(Avg_Asgmt_Score_PreCourse)[4] <- "Avg_Asgmt_Score_PreCourse"



# Calculate and store the number and score for the Asgmts a student submitted in a particular course and semester for all subsequent months
for (i in Deciles){
  Asgmt_dfs[[i+1]] <- aggregate(id_assessment ~ code_module + code_presentation + id_student,
                              data = FullAssessments[FullAssessments$date_submitted >= FullAssessments[,12+i] & FullAssessments$date_submitted < FullAssessments[,13+i],],
                              FUN = length)
  names(Asgmt_dfs[[i+1]])[4] <- names(Asgmt_dfs)[i+1]
  
  print(paste("Asgmt Dataframe number", i+1, "created"))
  
  Asgmt_Score_dfs[[i+1]] <- aggregate(score ~ code_module + code_presentation + id_student,
                                    data = FullAssessments[FullAssessments$date_submitted < FullAssessments[,13+i],],
                                    FUN = mean)
  names(Asgmt_Score_dfs[[i+1]])[4] <- names(Asgmt_Score_dfs)[i+1]
  
  print(paste("Asgmt Score Dataframe number", i+1, "created"))
  
}

# Merge all dataframes into a single dataframe containing the number and avg score of Asgmts and CMAs per month for each student in a course and semester
Asgmt_dfs <- c(list(Asgmts_PreCourse), Asgmt_dfs)
Asgmts_Full <- Reduce(function(x,y) merge(x,y, all=TRUE), Asgmt_dfs)
Asgmts_Full[is.na(Asgmts_Full)] <- 0

Asgmt_Score_dfs <- c(list(Avg_Asgmt_Score_PreCourse), Asgmt_Score_dfs)
Asgmts_Score_Full <- Reduce(function(x,y) merge(x,y, all=TRUE), Asgmt_Score_dfs)

studentInfo <- merge(x = studentInfo, y = Asgmts_Full, by = c("code_module", "code_presentation", "id_student"), all.x = TRUE)
studentInfo <- merge(x = studentInfo, y = Asgmts_Score_Full, by = c("code_module", "code_presentation", "id_student"), all.x = TRUE)



studentInfo[,13:45][is.na(studentInfo[,13:45])] <- 0

studentInfo <- transform(studentInfo, year = substr(code_presentation,1,4), semester = substr(code_presentation,5,5))



write.csv(studentInfo, file = "studentInfo_v3.csv")



































