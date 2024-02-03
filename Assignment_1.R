#dataset
dataset <- read.csv('Auto.csv',header = TRUE)

#changing the column type to numeric, to perform operations
dataset$horsepower <- as.numeric(dataset$horsepower)
class(dataset$horsepower)

# Check for missing values using is.na() and sum()
missing_values <- sum(is.na(dataset))

# Display the result
print(paste("Number of missing values in the dataset:", missing_values))

# Replacing missing values with the median
median <- apply(dataset['horsepower'], 2, median, na.rm = TRUE)
dataset$horsepower[is.na(dataset$horsepower)] <- median 

#Command to replace missing values with the mode
mode <- mode(dataset$horsepower)
dataset$horsepower[is.na(dataset$horsepower)] <- mode

#What is the range of each quantitative predictor? You can answer this using the range()
#function
range(dataset$mpg)
range(dataset$displacement)
range(dataset$horsepower)
range(dataset$weight)
range(dataset$acceleration)
range(dataset$year)

#What is the mean and standard deviation of each quantitative predictor?
mean(dataset$mpg)
mean(dataset$displacement)
mean(dataset$horsepower)
mean(dataset$weight)
mean(dataset$acceleration)
mean(dataset$year)

----
sd(dataset$mpg)
sd(dataset$displacement)
sd(dataset$horsepower)
sd(dataset$weight)
sd(dataset$acceleration)
sd(dataset$year)


#Now remove the 10th through 85th observations. What is the range, mean, and standard
#deviation of each predictor in the subset of the data that remains?

data_subset <- dataset[-c(10:85), ]
data_subset$horsepower <- as.numeric(data_subset$horsepower)
summary_stats <- sapply(data_subset[,1:7], function(x) c(Range = diff(range(x, na.rm = TRUE)), Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE)))
print(summary_stats)

#Using the full data set, investigate the predictors graphically, using scatterplots or other tools of
#your choice. Create some plots highlighting the relationships among the predictors. Comment
#on your findings.

library(car)
color_codes <- ifelse(dataset$origin == 1, "#3498db",
                      ifelse(dataset$origin == 2, "#e74c3c", "#2ecc71"))
# Create a scatterplot matrix with color-coded points
pairs(dataset[, 1:7], col = color_codes, pch = 16,cex = 0.4, main = "Pairplot Matrix")
correlation_matrix <- cor(dataset[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year", "origin")])
print(correlation_matrix)

#Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your
#plots suggest that any of the other variables might be useful in predicting mpg? Justify your
#answer.

# Boxplot of Miles per Gallon by Cylinders
ggplot(dataset, aes(x = factor(cylinders), y = mpg)) +
  geom_boxplot(fill = "#e74c3c", alpha = 0.7) +
  labs(title = "Boxplot of Miles per Gallon by Cylinders",
       x = "Cylinders",
       y = "Miles per Gallon") +
  theme_minimal()

# Scatterplot of Miles per Gallon vs. Horsepower
ggplot(dataset, aes(x = horsepower, y = mpg)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Scatterplot of Miles per Gallon vs. Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon")

# Smooth Scatterplot of Miles per Gallon vs. Weight with Blue and Orange Colors
ggplot(dataset, aes(x = weight, y = mpg)) +
  geom_point(color = "#3498db", alpha = 0.7) +  # Blue color
  geom_smooth(method = "loess", se = FALSE, color = "#e74c3c") +  # Orange color
  labs(title = "Miles per Gallon vs. Weight",
       x = "Weight",
       y = "Miles per Gallon") +
  theme_minimal()  

#line graph:Mean Miles per Gallon Over Years
ggplot(dataset, aes(x = year, y = mpg)) +
  geom_line(stat = "summary", fun.y = "mean", color = "blue") +
  labs(title = "Mean Miles per Gallon Over Years",
       x = "Year",
       y = "Mean Miles per Gallon")

#boxplot :Miles per Gallon by Origin
ggplot(dataset, aes(x = factor(origin), y = mpg)) +
  geom_boxplot(fill = "#3498db", alpha = 0.7) +
  labs(title = "Miles per Gallon by Origin",
       x = "Origin",
       y = "Miles per Gallon") +
  theme_minimal()

#smooth scatter plot:Scatterplot of Miles per Gallon vs. Acceleration
ggplot(dataset, aes(x = acceleration, y = mpg)) +
  geom_point(color = "#2ecc71", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "#e74c3c") + 
  labs(title = "Scatterplot of Miles per Gallon vs. Acceleration",
       x = "Acceleration",
       y = "Miles per Gallon") +
  theme_minimal()

#smooth scatter plot:Scatterplot of Miles per Gallon vs. Displacement
ggplot(dataset, aes(x = displacement, y = mpg)) +
  geom_point(color = "#3498db", alpha = 0.7) +  # Blue color for points
  geom_smooth(method = "loess", se = FALSE, color = "#e67e22") +  # Orange color for smoothed line
  labs(title = "Miles per Gallon vs. Displacement",
       x = "Displacement",
       y = "Miles per Gallon") +
  theme_minimal()












