setwd("D:/School/BISS/MSc Thesis/Data")
#setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
rm(list=ls())

library("ggplot2")
library("dplyr")

studentInfo <- read.csv("studentInfo_v3.csv")
studentInfo$X <- NULL
studentInfo$id_student <- as.character(studentInfo$id_student)

by_course <- group_by(studentInfo, code_module)
students_by_course <- summarise(by_course, total = n(), total_unique = n_distinct(id_student))


by_semester <- group_by(studentInfo, code_presentation)
students_by_semester <- summarise(by_semester, total = n(), total_unique = n_distinct(id_student))

by_course_semester <- group_by(studentInfo, code_module, code_presentation)
students_by_course_semester <- summarise(by_course_semester, total = n(), total_unique = n_distinct(id_student))

ggplot(studentInfo, aes(x=code_module)) +
       geom_bar()

ggplot(studentInfo, aes(x=code_presentation)) +
  geom_bar()

ggplot(studentInfo, aes(x=code_presentation, fill = code_module)) +
  geom_bar()+
  facet_grid(~code_module)

# Only courses BBB, DDD and FFF are taught all 4 semesters and they have the largest sample size.
# Therefore, we focus on these 3 courses.

studentInfo <- studentInfo[studentInfo$code_module %in% c("BBB", "DDD", "FFF"),]


# Barplots number of students in courses BBB, DDD and FFF in each semester
ggplot(studentInfo, aes(x=code_presentation, fill = code_module)) +
  geom_bar()+
  facet_grid(~code_module)

# Gender distribution in courses BBB,DDD and FFF in each semester
ggplot(studentInfo, aes(x=code_presentation, fill = gender)) +
  geom_bar()+
  facet_grid(~code_module)

# Final results in courses BBB, DDD and FFF in each semester
ggplot(studentInfo, aes(x=code_presentation, fill = final_result)) +
  geom_bar()+
  facet_grid(~code_module)


# Plot the average monthly number of clicks for each student success level (pass, fail, pass w distinction and withdrawn)
plot(colMeans(studentInfo[studentInfo$final_result == "Pass",13:23], na.rm = TRUE), type = "l", col = "blue",
     ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,18), main = "BBB, DDD, FFF All Semesters"  )
lines(colMeans(studentInfo[studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))





par(mfrow=c(2,2))

# Avg Number of Clicks per Day Over Time By Course

# Course BBB
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,10), main = "BBB")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# Course DDD
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,15), main = "DDD")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))


# Course FFF
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,35), main = "FFF")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))



# Conclusion the courses follow roughly similar patterns in terms of avg clicks per day per decile.
# However, the BBB and DDD have a lower number of clicks per day than FFF
# Therefore, it will probably be more suitable to predict results per course with different models instead of an all-encompassing model
dev.off()



par(mfrow=c(2,2))

# Avg Number of Clicks per Day Over Time (Course BBB)

# Plot the average monthly number of clicks for each student success level (pass, fail, pass w distinction and withdrawn)
# 2013B Course BBB
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation == "2013B"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,10), main = "BBB 2013B")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# 2013J Course BBB
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation == "2013J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,10), main = "BBB 2013J")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# 2014B Course BBB
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation == "2014B"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,10), main = "BBB 2014B")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))


# 2014J Course BBB
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation == "2014J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,10), main = "BBB 2014J")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# Conclusion: Clicks follow roughly the same pattern every semester in course BBB except for 2014J which is somewhat different
dev.off()




par(mfrow=c(2,2))

# Avg Number of Clicks per Day Over Time (Course DDD)

# Plot the average monthly number of clicks for each student success level (pass, fail, pass w distinction and withdrawn)
# 2013B Course DDD
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$code_presentation == "2013B"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,15), main = "DDD 2013B")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# 2013J Course DDD
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$code_presentation == "2013J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,15), main = "DDD 2013J")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# 2014B Course DDD
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$code_presentation == "2014B"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,15), main = "DDD 2014B")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))


# 2014J Course DDD
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$code_presentation == "2014J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,15), main = "DDD 2014J")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# Conclusion: Clicks follow roughly the same pattern every semester in course DDD, However the patterns in 2013 seem to be somewhat more expressive than in 2014
dev.off()


par(mfrow=c(2,2))
# Avg Number of Clicks per Day Over Time (Course FFF)

# Plot the average monthly number of clicks for each student success level (pass, fail, pass w distinction and withdrawn)
# 2013B Course FFF
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$code_presentation == "2013B"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,35), main = "FFF 2013B")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2013B"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# 2013J Course FFF
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$code_presentation == "2013J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,35), main = "FFF 2013J")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2013J"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# 2014B Course FFF
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$code_presentation == "2014B"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,35), main = "FFF 2014B")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014B"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))


# 2014J Course FFF
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$code_presentation == "2014J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,35), main = "FFF 2014J")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
#legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

# Conclusion: Clicks follow roughly the same pattern every semester in course FFF

dev.off()
par(mfrow=c(1,1))
