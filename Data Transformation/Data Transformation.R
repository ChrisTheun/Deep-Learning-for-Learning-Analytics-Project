#setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
rm(list=ls())

library("dplyr")
library("tidyr")
library("ggplot2")
library("scales")


# Import public Open University dataset
assessments <- read.csv("assessments.csv")
courses <- read.csv("courses.csv")
studentAssessment <- read.csv("studentAssessment.csv")
studentInfo <- read.csv("studentInfo.csv")
studentRegistration <- read.csv("studentRegistration.csv")
studentVle <- read.csv("studentVle.csv")
vle <- read.csv("vle.csv")


# Create an identifier for each student combining course module, course presentation and student id
studentInfo$identifier <- paste(studentInfo$code_module, studentInfo$code_presentation, studentInfo$id_student)

# Create copy of studentInfo data for later vizualization purposes
Info <- studentInfo

# Only retain students in courses BBB, DDD and FFF & remove those that dropped out
studentInfo <- studentInfo[studentInfo$code_module %in% c("BBB","DDD","FFF"),]
withdrawnStudents <- studentInfo[studentInfo$final_result == "Withdrawn",]
studentInfo <- studentInfo[studentInfo$final_result != "Withdrawn",]
studentVle <- studentVle[studentVle$code_module %in% c("BBB","DDD","FFF"),]
studentRegistration <- studentRegistration[studentRegistration$code_module %in% c("BBB","DDD","FFF"),]

#Withdraw <- merge(x=studentInfo[studentInfo$final_result != "Withdrawn",], y = studentRegistration[!is.na(studentRegistration$date_unregistration),])



# Impute missing exam dates with the final day of the course 
# [Official documentation: "If the information about the final exam date is missing, it is at the end of the last presentation week."]
for(i in 1:nrow(assessments)){
  m <- as.character(assessments$code_module[i])
  p <- as.character(assessments$code_presentation[i])
  if (is.na(assessments$date[i]) & assessments$assessment_type[i] == "Exam"){
    assessments$date[i] <- courses$module_presentation_length[courses$code_module == m & courses$code_presentation == p]
  }
}

# Merge dataframes
FullAssessments <- merge(x = studentAssessment, y = assessments[assessments$code_module %in% c("BBB","DDD","FFF"),], by = "id_assessment")
FullVle <- merge(x = studentVle, y = vle[,c(1,4)], by = "id_site")



# Remove assessments that have been submitted on or after the final exam date conditional on course and semester
FullAssessments <- merge(x = FullAssessments, y = assessments[assessments$assessment_type == "Exam",c(1,2,5)], by = c("code_module", "code_presentation"), all.x = TRUE)
FullAssessments <- FullAssessments[!duplicated(FullAssessments),]
names(FullAssessments)[c(9,11)] <- c("date", "exam_date")
FullAssessments <- FullAssessments[FullAssessments$date_submitted < FullAssessments$exam_date,]

# In course DDD students have differing exam submission dates, so a dataframe is created to collect those.
DDD_examdates <- FullAssessments %>%
  select(code_module, code_presentation, id_student,assessment_type, date_submitted) %>%
  filter(assessment_type == "Exam")
DDD_examdates$assessment_type <- NULL
names(DDD_examdates)[4] <- "exam_submitted_date"

# Add exam_submitted date column of DDD course to the FUllAssessments dataframe and fill the exam_submitted dates of the other courses with exam_date
FullAssessments <- merge(x = FullAssessments, y = DDD_examdates, by = c("code_module", "code_presentation", "id_student"), all.x=TRUE)
FullAssessments$exam_submitted_date[is.na(FullAssessments$exam_submitted_date)] <- FullAssessments$exam_date[is.na(FullAssessments$exam_submitted_date)]
# Only retain those assessments that occured before the final exam submission date
FullAssessments <- FullAssessments[FullAssessments$date_submitted < FullAssessments$exam_submitted_date,]

# Add the exam submission date from the FullAssesments df to each student record in the studentRegistration df
studentRegistration <- merge(x = studentRegistration[is.na(studentRegistration$date_unregistration),], y = FullAssessments[,c(1,2,3,12)], by = c("code_module", "code_presentation", "id_student"), all.x = TRUE)
studentRegistration <- studentRegistration[!duplicated(studentRegistration),]

# Make sure that if the exam_submission_date is not available, it is imputed with exam date of the course presentation from the assessments df
for (i in 1:nrow(studentRegistration)){
  if (is.na(studentRegistration$exam_submitted_date[i]) == TRUE){
    studentRegistration$exam_submitted_date[i] <- assessments$date[assessments$assessment_type == "Exam" & 
                                                                  assessments$code_module == studentRegistration$code_module[i] &
                                                                  assessments$code_presentation == studentRegistration$code_presentation[i]]
  }
}






# Compute the number of assignments submitted before the final exam date and the average score which is updated daily  
FullAssessments <- merge(x = FullAssessments[,1:8], y = studentRegistration[,c(1:4,6)],   by = c("code_module", "code_presentation", "id_student"), all.y = TRUE)
AsgmtCounts <- aggregate(id_assessment ~ code_module + code_presentation + id_student + date_submitted, data = FullAssessments, FUN = length)
names(AsgmtCounts)[5] <- "Asgmt_Count"
AsgmtScores <- aggregate(score ~ code_module + code_presentation + id_student + date_submitted, data = FullAssessments, FUN = sum)
names(AsgmtScores)[5] <- "Asgmt_sumScore"
Asgmts <- merge(x = studentRegistration[,c(1:4,6)], y = AsgmtCounts,by = c("code_module", "code_presentation", "id_student"))
Asgmts <- merge(x = Asgmts, y = AsgmtScores,by = c("code_module", "code_presentation", "id_student", "date_submitted"))
names(Asgmts)[4] <- "date"


# Calculate the number of clicks a student made in a particular course and semester
# & Remove clicks that have been made on or after the final exam date conditional on course and semester
Clicks <- aggregate(sum_click ~ code_module + code_presentation + id_student + date, data = FullVle, FUN=sum)
Clicks <- merge(x = Clicks[Clicks$code_module %in% c("BBB","DDD","FFF"),], y = studentRegistration[,c(1:4,6)], by = c("code_module", "code_presentation", "id_student"), all.y = TRUE)
Clicks <- Clicks[!duplicated(Clicks),]
Clicks <- Clicks[Clicks$date < Clicks$exam_submitted_date,]


# Calculate the number of clicks a student made in a particular course and semester
ClicksAsgmts <- merge(x = Clicks, y=Asgmts, by = c("code_module", "code_presentation", "id_student", "date", "date_registration", "exam_submitted_date"), all.x = TRUE, all.y = TRUE)
ClicksAsgmts <- ClicksAsgmts[apply(ClicksAsgmts,1,function(x)any(!is.na(x))),]

# Expand the date range from -25 to the end of the course
min_date_BBB <- min(ClicksAsgmts$date[ClicksAsgmts$code_module == "BBB"])
min_date_DDD <- min(ClicksAsgmts$date[ClicksAsgmts$code_module == "DDD"])
min_date_FFF <- min(ClicksAsgmts$date[ClicksAsgmts$code_module == "FFF"])
ClicksAsgmts$min_date[ClicksAsgmts$code_module == "BBB"] <- min_date_BBB
ClicksAsgmts$min_date[ClicksAsgmts$code_module == "DDD"] <- min_date_DDD
ClicksAsgmts$min_date[ClicksAsgmts$code_module == "FFF"] <- min_date_FFF
max_date_BBB <- max(ClicksAsgmts$exam_submitted_date[ClicksAsgmts$code_module == "BBB"])
max_date_DDD <- max(ClicksAsgmts$exam_submitted_date[ClicksAsgmts$code_module == "DDD"])
max_date_FFF <- max(ClicksAsgmts$exam_submitted_date[ClicksAsgmts$code_module == "FFF"])
ClicksAsgmts$days[ClicksAsgmts$code_module == "BBB"] <- max_date_BBB - ClicksAsgmts$min_date[ClicksAsgmts$code_module == "BBB"]
ClicksAsgmts$days[ClicksAsgmts$code_module == "DDD"] <- max_date_DDD - ClicksAsgmts$min_date[ClicksAsgmts$code_module == "DDD"]
ClicksAsgmts$days[ClicksAsgmts$code_module == "FFF"] <- max_date_FFF - ClicksAsgmts$min_date[ClicksAsgmts$code_module == "FFF"]

# Impute 0 if there are no clicks or assignments made on a certain day
ClicksAsgmts$sum_click[is.na(ClicksAsgmts$sum_click)] <- 0
ClicksAsgmts$Asgmt_Count[is.na(ClicksAsgmts$Asgmt_Count)] <- 0
ClicksAsgmts$Asgmt_sumScore[is.na(ClicksAsgmts$Asgmt_sumScore)] <- 0
ClicksAsgmts$identifier <- paste(ClicksAsgmts$code_module, ClicksAsgmts$code_presentation, ClicksAsgmts$id_student)

# Remove all withdrawn students still in the df
ClicksAsgmts <- ClicksAsgmts[!(ClicksAsgmts$identifier %in% withdrawnStudents$identifier),]

# Make another df to help constructing a range of day registered until day before final exam for each student
dateReference <- merge(x= studentRegistration[,c(1:4,6)], y = ClicksAsgmts[,c(1,2,3,10,11,12)], by = c("code_module", "code_presentation", "id_student"), all.y = TRUE)
dateReference <- dateReference[!duplicated(dateReference),]
row.names(dateReference) <- NULL
dateReference <-dateReference[rep(row.names(dateReference), dateReference$day),]
dateReference$date <- sequence(rle(as.character(dateReference$identifier))$lengths)+dateReference$min_date-1

fulldf <- merge(x = ClicksAsgmts, y = dateReference, by = c("code_module", "code_presentation", "id_student","identifier", "date", "date_registration", "exam_submitted_date", "min_date", "days"), all.y = TRUE)
fulldf <- fulldf[with(fulldf, order(code_module, code_presentation, id_student, date)),]
fulldf$sum_click[is.na(fulldf$sum_click) & fulldf$date < fulldf$exam_submitted_date & fulldf$date >= fulldf$date_registration] <- 0
fulldf$Asgmt_Count[is.na(fulldf$Asgmt_Count) & fulldf$date < fulldf$exam_submitted_date & fulldf$date >= fulldf$date_registration] <- 0
fulldf$Asgmt_sumScore[is.na(fulldf$Asgmt_sumScore) & fulldf$date < fulldf$exam_submitted_date & fulldf$date >= fulldf$date_registration] <- 0

#example <- fulldf[fulldf$id_student == 34353,]
#example$csum_score[!is.na(example$Asgmt_sumScore)] <- ave(example$Asgmt_sumScore[!is.na(example$Asgmt_sumScore)], example$identifier[!is.na(example$Asgmt_sumScore)], FUN = cumsum)
#example$csum_asgmts <- ave(example$Asgmt_Count, example$identifier, FUN = cumsum)

# Compute average assignment score by using the cumulative sum of scores updated daily
fulldf$csum_score[!is.na(fulldf$sum_click)] <- ave(fulldf$Asgmt_sumScore[!is.na(fulldf$sum_click)], fulldf$identifier[!is.na(fulldf$sum_click)], FUN = cumsum)
fulldf$csum_asgmts[!is.na(fulldf$Asgmt_Count)] <- ave(fulldf$Asgmt_Count[!is.na(fulldf$Asgmt_Count)], fulldf$identifier[!is.na(fulldf$Asgmt_Count)], FUN = cumsum)
fulldf$Avg_score <- round(fulldf$csum_score/fulldf$csum_asgmts,2)
fulldf$Avg_score[is.nan(fulldf$Avg_score)] <- 0

# Finalize complete dataframe used for modeling
fulldf <- fulldf[,c(1:5, 10, 11, 14, 15)]
fulldf <- merge(x = fulldf, y = studentInfo[,c(1:3, 12)], by = c("code_module", "code_presentation", "id_student"), all.x = TRUE)
fulldf <- fulldf[with(fulldf, order(code_module, code_presentation, id_student, date)),]

# Pad the shorter sequence lengths with '-1' values, so the LSTM knows when to start/end each sequence
fulldf$sum_click[is.na(fulldf$sum_click)] <- -1
fulldf$Asgmt_Count[is.na(fulldf$Asgmt_Count)] <- -1
fulldf$csum_asgmts[is.na(fulldf$csum_asgmts)] <- -1
fulldf$Avg_score[is.na(fulldf$Avg_score)] <- -1
fulldf$final_result <- droplevels(fulldf$final_result)
names(fulldf)[7] <- "asgmts"

# Split the dataframe into 3 smaller dataframes, 1 for each course
BBB_Train <- fulldf[fulldf$code_module == "BBB",]
DDD_Train <- fulldf[fulldf$code_module == "DDD",]
FFF_Train <- fulldf[fulldf$code_module == "FFF",]

# Write the dataframes to a csv file
#write.csv(x = BBB_Train, "BBB_Train.csv")
#write.csv(x = DDD_Train, "DDD_Train.csv")
#write.csv(x = FFF_Train, "FFF_Train.csv")





# VISUALIZATION
# ============================================================================================================



ggplot(data = Info, aes(x = code_presentation, fill = code_module)) +
  geom_bar(stat = 'count', position=position_dodge()) +
  facet_grid(.~code_module) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())+
  scale_fill_brewer(palette="Set2") +
  guides(fill=FALSE) +
  xlab("Code presentation") +
  ylab("Student count")


Train <- rbind(BBB_Train, DDD_Train, FFF_Train)
Train <- select(Train, code_module, code_presentation, id_student, final_result)
Train <- Train[!duplicated(Train),]
Train$final_result = factor(Train$final_result,levels(Train$final_result)[c(1,3,2)])



ggplot(data = Train, aes(x = code_presentation, fill = final_result)) +
  geom_bar(stat = 'count') +
  facet_grid(.~code_module) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())+
  scale_fill_manual(values = c("#9999CC", "#66CC99", "#CC6666")) +
  xlab("Code presentation") +
  ylab("Student count") +
  guides(fill=guide_legend(title="Final result"))

# ============================================================================================================









# Reformat datasets for traditional machine learning models

# ============================================================================================================

fulldf$csum_asgmts <- NULL
fulldf$sum_click[fulldf$sum_click == -1] <- 0
fulldf$asgmts[fulldf$asgmts == -1] <- 0
fulldf$Avg_score[fulldf$Avg_score == -1] <- 0


BBB <- fulldf[fulldf$code_module == "BBB",]
DDD <- fulldf[fulldf$code_module == "DDD",]
FFF <- fulldf[fulldf$code_module == "FFF",]


BBB_coursedays <- max(BBB$date)
BBB$Decile <- "Decile10"
BBB$Decile[BBB$date < (max(BBB$date)*9)/10] <- "Decile9"
BBB$Decile[BBB$date < (max(BBB$date)*8)/10] <- "Decile8"
BBB$Decile[BBB$date < (max(BBB$date)*7)/10] <- "Decile7"
BBB$Decile[BBB$date < (max(BBB$date)*6)/10] <- "Decile6"
BBB$Decile[BBB$date < (max(BBB$date)*5)/10] <- "Decile5"
BBB$Decile[BBB$date < (max(BBB$date)*4)/10] <- "Decile4"
BBB$Decile[BBB$date < (max(BBB$date)*3)/10] <- "Decile3"
BBB$Decile[BBB$date < (max(BBB$date)*2)/10] <- "Decile2"
BBB$Decile[BBB$date < max(BBB$date)/10] <- "Decile1"
BBB$Decile[BBB$date < 0] <- "Decile0"


DDD_coursedays <- max(DDD$date)
DDD$Decile <- "Decile10"
DDD$Decile[DDD$date < (max(DDD$date)*9)/10] <- "Decile9"
DDD$Decile[DDD$date < (max(DDD$date)*8)/10] <- "Decile8"
DDD$Decile[DDD$date < (max(DDD$date)*7)/10] <- "Decile7"
DDD$Decile[DDD$date < (max(DDD$date)*6)/10] <- "Decile6"
DDD$Decile[DDD$date < (max(DDD$date)*5)/10] <- "Decile5"
DDD$Decile[DDD$date < (max(DDD$date)*4)/10] <- "Decile4"
DDD$Decile[DDD$date < (max(DDD$date)*3)/10] <- "Decile3"
DDD$Decile[DDD$date < (max(DDD$date)*2)/10] <- "Decile2"
DDD$Decile[DDD$date < max(DDD$date)/10] <- "Decile1"
DDD$Decile[DDD$date < 0] <- "Decile0"


FFF_coursedays <- max(FFF$date)
FFF$Decile <- "Decile10"
FFF$Decile[FFF$date < (max(FFF$date)*9)/10] <- "Decile9"
FFF$Decile[FFF$date < (max(FFF$date)*8)/10] <- "Decile8"
FFF$Decile[FFF$date < (max(FFF$date)*7)/10] <- "Decile7"
FFF$Decile[FFF$date < (max(FFF$date)*6)/10] <- "Decile6"
FFF$Decile[FFF$date < (max(FFF$date)*5)/10] <- "Decile5"
FFF$Decile[FFF$date < (max(FFF$date)*4)/10] <- "Decile4"
FFF$Decile[FFF$date < (max(FFF$date)*3)/10] <- "Decile3"
FFF$Decile[FFF$date < (max(FFF$date)*2)/10] <- "Decile2"
FFF$Decile[FFF$date < max(FFF$date)/10] <- "Decile1"
FFF$Decile[FFF$date < 0] <- "Decile0"


BBB_Clicks_D <- aggregate(sum_click~code_module+code_presentation+id_student+identifier+Decile, data = BBB, FUN = sum)
BBB_Asgmts_D <- aggregate(asgmts~code_module+code_presentation+id_student+identifier+Decile, data = BBB, FUN = sum)
BBB_Scores_D <- aggregate(Avg_score~code_module+code_presentation+id_student+identifier+Decile, data = BBB, FUN = last)
BBB_Decs <- data.frame(BBB_Clicks_D, BBB_Asgmts_D$asgmts, BBB_Scores_D$Avg_score)
