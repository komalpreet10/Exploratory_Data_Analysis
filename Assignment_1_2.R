#reading the dataset
college <- read.csv('College.csv',header = TRUE)
college
# The First column header was blank, replacing it with suitable name.
colnames(college)[1] <- c("University_Name")

#Viewing and Summarizing - Insights
View(college)
head(college, 10)
tail(college, 10)
summary(college)

#Insights from summary:
#In the world of universities in the United States, a big dataset about 777 of them tells us a few things:
#Private universities outnumber public ones, with 565 compared to 212, making them twice as prevalent.
#The average university program witnesses a 67% acceptance rate, with 2,000 individuals accepted from 3,000 applicants, and subsequently, a 40% enrollment rate as approximately 800 applicants matriculate into the program.
#University costs are all over the place. On average, studying out of your home state costs about $10,441. Rooms can be around $4,358. But these costs can vary a lot.
#Some schools have more teachers for each student (14 on average). The number of full-time students varies a lot, from around 100 to over 30,000. There are also part-time students, ranging from 1 to over 21,000.
#Books can be cheap or expensive, from $96 to $2,340. Most teachers are well-educated, with around 72.66% having Ph.D.'s and 79.7% having the highest degree possible.
#People who graduated from these universities often give back. About 22.74% of them donate money. Universities also spend about $9,660 per student on teaching.
#Not everyone finishes at the same rate. Some schools have graduation rates as low as 10%, while others have rates as high as 118%.

#scatterplot matrix of first 10 columns - Insights
college$Private <- as.factor(college$Private)
colors <- ifelse(college$Private == "Yes", "#3498db", "#e74c3c")
pairs(college[, 2:11], pch = 16, col = colors, cex = 0.4, 
      main = "Pairplot Matrix",cex.labels = 0.6)

#Insights
#The enrollment in public universities is higher than private.
#There is positive correlation between the number of applications accepted and the number of students enrolled.As the number of applicants increase, there is corresponding increase in the enrollment.
#As Outstate tuition increases, the number of students enrolling in the university tends to decrease.
#Private colleges generally have higher room and board costs compared to public college.

#Outstate vs Private
plot(college$Private, college$Outstate, xlab = "Private", ylab = "Outstate",
     col = c("red", "lightgreen"), names = c("No", "Yes"), main = "Boxplot of Outstate by Private",
     pch = 16)

#Histograms for quantitative variables
par(mfrow = c(2, 2))
variables_and_bins <- list(
  list(variable = "Apps", bins = 20,colour = "skyblue"),
  list(variable = "Enroll", bins = 15,colour = "salmon"),
  list(variable = "Outstate", bins = 25,colour = "lightgreen"),
  list(variable = "S.F.Ratio", bins = 18,colour = "orchid")
)
for (i in 1:4) {
  var_info <- variables_and_bins[[i]]
  var <- var_info$variable
  bins <- var_info$bins
  colour<- var_info$colour
  hist(college[[var]], main = paste("Histogram for", var), xlab = var, col = colour, breaks = bins)
}
par(mfrow = c(1, 1))


#university name - most students in top 10% of the class
#creating new variable to split the dataset into two groups
college$HighSchoolTop10 <- ifelse(college$Top10perc > 50, "Yes", "No")
#filtering the dataset for first group students 
college_subset <-college[college$HighSchoolTop10 == "Yes", ]
university_top10 <- college_subset[which.max(college_subset$Top10), "University_Name"]
print(university_top10)

#University name - smallest acceptance rate
smallest_acceptance_rate <- college[which.min(college$Accept / college$Apps), "University_Name"]
smallest_acceptance_rate

#University name - highest acceptance rate
highest_acceptance_rate <- college[which.max(college$Accept / college$Apps), "University_Name"]
highest_acceptance_rate

