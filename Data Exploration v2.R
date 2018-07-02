#setwd("D:/School/BISS/MSc Thesis/Data")
setwd("D:/School/BISS/MSc Thesis/Data/Anonymized Data")
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
plot(colMeans(studentInfo[studentInfo$final_result == "Pass",13:23], na.rm = TRUE), type = "l", col = "blue", xaxt = "n",
     ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,18), main = "Average number of clicks per day by decile for all courses"  )
lines(colMeans(studentInfo[studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green")
#lines(colMeans(studentInfo[studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
legend("topright", legend = c("Distinction", "Pass", "Fail"), fill = c("green", "blue", "red"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))




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
#lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
#                           & studentInfo$final_result == "Withdrawn",13:23], na.rm = TRUE))
legend("topright", legend = c("Distinction", "Pass", "Fail", "Withdrawn"), fill = c("green", "blue", "red", "black"))

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





# BBB Training set vs Test set Clicks
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,10),xaxt = "n",
     main = "BBB Clicks per Day by Decile - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation == "2014J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green", lty = "dashed")
legend("topright", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))


# DDD Training set vs Test set Clicks
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,15),xaxt = "n",
     main = "DDD Clicks per Day by Decile - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",13:23], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green", lty = "dashed")
legend("topright", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))


# FFF Training set vs Test set Clicks
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",13:23], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average number of Clicks per Day", xlab = "Decile", ylim = c(0,35),xaxt = "n",
     main = "FFF Clicks per Day by Decile - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",13:23], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",13:23], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",13:23], na.rm = TRUE), col = "green", lty = "dashed")
legend("topright", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))













# Average Assignments Over Time
# Plot the average assignments made per decile for each student success level (pass, fail, pass w distinction)
plot(colMeans(studentInfo[studentInfo$final_result == "Pass",24:34], na.rm = TRUE), type = "l", col = "blue", xaxt = "n",
     ylab = "Average number of Assignments Submitted", xlab = "Decile", ylim = c(0,1.5), main = "Average number of assignments submitted by decile for all courses"  )
lines(colMeans(studentInfo[studentInfo$final_result == "Fail",24:34], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$final_result == "Distinction",24:34], na.rm = TRUE), col = "green")
legend("topright", legend = c("Distinction", "Pass", "Fail"), fill = c("green", "blue", "red"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))




# BBB Training set vs Test set Assignments
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",24:34], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average number of assignment submitted", xlab = "Decile", ylim = c(0,2.5),xaxt = "n",
     main = "BBB average number of assignments submitted - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",24:34], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",24:34], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",24:34], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",24:34], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",24:34], na.rm = TRUE), col = "green", lty = "dashed")
legend("topleft", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))


# DDD Training set vs Test set Assignments
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",24:34], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average number of assignment submitted", xlab = "Decile", ylim = c(0,2),xaxt = "n",
     main = "DDD average number of assignments submitted - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",24:34], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",24:34], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",24:34], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",24:34], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",24:34], na.rm = TRUE), col = "green", lty = "dashed")
legend("topleft", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))



# FFF Training set vs Test set Assignments
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",24:34], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average number of assignment submitted", xlab = "Decile", ylim = c(0,2.5),xaxt = "n",
     main = "FFF average number of assignments submitted - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",24:34], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",24:34], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",24:34], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",24:34], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",24:34], na.rm = TRUE), col = "green", lty = "dashed")
legend("topleft", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))








# Average Assignments Over Time
# Plot the average assignments made per decile for each student success level (pass, fail, pass w distinction)
plot(colMeans(studentInfo[studentInfo$final_result == "Pass",35:45], na.rm = TRUE), type = "l", col = "blue", xaxt = "n",
     ylab = "Average Assignment Score", xlab = "Decile", ylim = c(0,100), main = "Average assignment score by decile for all courses"  )
lines(colMeans(studentInfo[studentInfo$final_result == "Fail",35:45], na.rm = TRUE), col="red")
lines(colMeans(studentInfo[studentInfo$final_result == "Distinction",35:45], na.rm = TRUE), col = "green")
legend("topright", legend = c("Distinction", "Pass", "Fail"), fill = c("green", "blue", "red"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))




# BBB Training set vs Test set Assignment score
plot(colMeans(studentInfo[studentInfo$code_module == "BBB"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",35:45], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average assignment score", xlab = "Decile", ylim = c(0,130),xaxt = "n",
     main = "BBB average assignment score - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",35:45], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",35:45], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",35:45], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",35:45], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "BBB"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",35:45], na.rm = TRUE), col = "green", lty = "dashed")
legend("topleft", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))



# DDD Training set vs Test set Assignment score
plot(colMeans(studentInfo[studentInfo$code_module == "DDD"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",35:45], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average assignment score", xlab = "Decile", ylim = c(0,130),xaxt = "n",
     main = "DDD average assignment score - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",35:45], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",35:45], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",35:45], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",35:45], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "DDD"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",35:45], na.rm = TRUE), col = "green", lty = "dashed")
legend("topleft", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))




# FFF Training set vs Test set Assignment score
plot(colMeans(studentInfo[studentInfo$code_module == "FFF"
                          & studentInfo$code_presentation != "2014J"
                          & studentInfo$final_result == "Pass",35:45], na.rm = TRUE),
     type = "l", col = "blue", lwd = 2, ylab = "Average assignment score", xlab = "Decile", ylim = c(0,130),xaxt = "n",
     main = "FFF average assignment score - Training vs Test")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Fail",35:45], na.rm = TRUE), col="red",lwd = 2)
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation != "2014J"
                           & studentInfo$final_result == "Distinction",35:45], na.rm = TRUE), col = "green",lwd = 2)

lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Pass",35:45], na.rm = TRUE), col = "blue", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Fail",35:45], na.rm = TRUE), col="red", lty = "dashed")
lines(colMeans(studentInfo[studentInfo$code_module == "FFF"
                           & studentInfo$code_presentation == "2014J"
                           & studentInfo$final_result == "Distinction",35:45], na.rm = TRUE), col = "green", lty = "dashed")
legend("topleft", legend = c("Distinction Training","Distinction Test", "Pass Training","Pass Test", "Fail Training","Fail Test"),
       col = c("green", "green", "blue", "blue", "red", "red"),
       lty = c("solid", "dashed", "solid", "dashed", "solid", "dashed"))
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("PreCourse",1,2,3,4,5,6,7,8,9,10))
