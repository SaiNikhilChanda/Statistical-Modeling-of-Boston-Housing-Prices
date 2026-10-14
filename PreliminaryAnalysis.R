data <- read.csv("C:/Users/aweso/OneDrive - Clark University/Year 3 Semester 2/MATH 219 - Linear Models/Final Project/BostonHousing.csv", header = TRUE)

library(ggplot2)
library(GGally)
library(dbplyr)
library(corrplot)

pairs(data, col = "red")


# View the structure and summary of the dataset
str(data)
summary(data)

# Check for missing values
sapply(data, function(x) sum(is.na(x)))

# Plot histograms of continuous variables
boston_numeric <- data[, !(colnames(data) %in% c("chas", "medv"))]

par(mfrow = c(2, 2))

for (col in colnames(boston_numeric)) {
  hist(boston_numeric[[col]], main = paste("Histogram of", col), xlab = col, col = "skyblue", border = "black")
}

for (col in colnames(boston_numeric)) {
  ggplot(data, aes(x = .data[[col]], y = medv)) + 
      geom_point(color = 'blue') +
      labs(title = paste("Scatter Plot of", col, "vs medv"), x = col, y = 'medv')
}


library(ggplot2)
library(gridExtra) # To arrange multiple plots in a grid

plots <- list() # List to store individual plots

for (col in colnames(boston_numeric)) {
    p <- ggplot(data, aes(x = .data[[col]], y = medv)) + 
      geom_point(color = 'blue', alpha = 0.7) + 
      labs(title = paste("Scatter Plot of", col, "vs medv"),
           x = col, y = "medv") + 
      theme_minimal()
    plots[[col]] <- p
}

# Arrange the first 4 plots in a 2x2 grid
do.call(grid.arrange, c(plots[1:4], nrow = 2, ncol = 2))

do.call(grid.arrange, c(plots[5:8], nrow = 2, ncol = 2))

do.call(grid.arrange, c(plots[9:12], nrow = 2, ncol = 2))

#Doing the same Except with tranformed variables
plots <- list() # List to store individual plots

for (col in colnames(boston_numeric)) {
    p <- ggplot(data, aes(x = .data[[col]], y = log(medv, 10))) + 
      geom_point(color = 'blue', alpha = 0.7) + 
      labs(title = paste("Scatter Plot of", col, "vs log(medv, 10)"),
           x = col, y = "log(medv, 10)") + 
      theme_minimal()
    plots[[col]] <- p
}


# Arrange the first 4 plots in a 2x2 grid
do.call(grid.arrange, c(plots[1:4], nrow = 2, ncol = 2))

do.call(grid.arrange, c(plots[5:8], nrow = 2, ncol = 2))

do.call(grid.arrange, c(plots[9:12], nrow = 2, ncol = 2))

#Doing the same Except with tranformed variables for both
plots <- list() # List to store individual plots

for (col in colnames(boston_numeric)) {
  p <- ggplot(data, aes(x = log(.data[[col]], 10), y = log(medv, 10))) + 
    geom_point(color = 'blue', alpha = 0.7) + 
    labs(title = paste("Scatter Plot of", "log(", col, "10) ", "vs log(medv, 10)"),
         x = col, y = "log(medv, 10)") + 
    theme_minimal()
  plots[[col]] <- p
}


# Arrange the first 4 plots in a 2x2 grid
do.call(grid.arrange, c(plots[1:4], nrow = 2, ncol = 2))

do.call(grid.arrange, c(plots[5:8], nrow = 2, ncol = 2))

do.call(grid.arrange, c(plots[9:12], nrow = 2, ncol = 2))

#Scatter plot of MEDV vs CRIM
ggplot(data, aes(x = crim, y = medv)) + 
  geom_point(color = 'blue') + 
  labs(title = 'Crime Rate vs Median Home Value', x = 'Crime Rate', y = 'Median Value of Homes')

# Making Transformed Variables
data$log_crim <- log(data$crim + 1)
data$log_medv <- log(data$medv + 1)

#Scatter plot of MEDV and CRIM but with Log tranformation
ggplot(data, aes(x = log_crim, y = log_medv)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Crime Rate vs Median Home Value",
       x = "Log(Crime Rate)", y = "Log(Median Home Value)")

# Scatter plot of MEDV vs RM (Median Value vs Average Number of Rooms)
ggplot(data, aes(x = rm, y = medv)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Averager Num Rooms vs Median Home Value", x = "Average Number of Rooms", y = "Median Value of Homes")

#Polynomial Regression on rm and medv
data$rm_squared <- data$rm^2
ggplot(data, aes(x = rm, y = medv)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", linetype = "dashed") +
  labs(title = "Polynomial Transformation of Average Rooms vs Median Home Value",
       x = "Average Number of Rooms",
       y = "Median Value of Homes")

# Scatter plot of MEDV vs lstat (Median Value vs lstat)
ggplot(data, aes(x = lstat, y = medv)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "Percentage of Lower Status Pop. vs Median Home Value", x = "Percentage of Lower Status Pop.", y = "Median Value of Homes")


#Polynomial Regression on lstat and medv
data$lstat_squared <- data$lstat^2
ggplot(data, aes(x = lstat_squared, y = medv)) +
  geom_point(color = 'blue') +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", linetype = "dashed") +
  labs(title = "Polynomial Transformation of Percentage of Lower Status Pop. vs Median Home Value",
       x = "Percentage of Lower Status Pop.",
       y = "Median Value of Homes")


# Boxplot to visualize categorical variable CHAS
ggplot(data, aes(x = as.factor(chas), y = medv)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of MEDV by CHAS", x = "CHAS (1: River bounds, 0: Otherwise)", y = "Median Value of Homes (MEDV)")

# Correlation matrix of numeric variables
cor_matrix <- cor(boston_numeric)
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)

# Summarize the categorical variable CHAS
table(data$chas)

# Pairwise scatter plots (optional, can be time-consuming)
pairs(boston_numeric, main = "Pairwise Scatter Plots")

#Running Interaction term Scatterplots Against log(Y, 10)


