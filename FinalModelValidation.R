data <- read.csv("C:/Users/aweso/OneDrive - Clark University/Year 3 Semester 2/MATH 219 - Linear Models/Final Project/BostonHousing.csv", header = TRUE)

library(rsm)
library(lmtest)
library(DAAG)
library(car)
library(performance)
library(knitr)
library(olsrr)
library(leaps)
library(caret)

min(data$medv)
max(data$medv)
set.seed(42)


analysis_of_model <- function(model, model_name, data = train_data) {
  # Summary and ANOVA
  print(summary(model))
  print(anova(model))
  
  par(mfrow = c(1, 1))
  # Residuals vs Fitted plot
  plot(fitted(model), residuals(model), 
       main = paste("Residuals vs Fitted from", model_name), 
       xlab = "Fitted Values", 
       ylab = "Residuals")
  abline(h = 0, col = "red")
  
  par(mfrow = c(1, 1))
  # QQ Plot
  qq_data <- qqnorm(residuals(model), main = paste("QQ Plot of Residuals from", model_name), plot.it = TRUE)
  qqline(residuals(model), col = "red")
  correlation_coefficient <- cor(qq_data$y, qq_data$x)
  cat("Correlation coefficient (for normality):", correlation_coefficient, "\n")
  
  
  # Breusch-Pagan Test for heteroscedasticity
  print(bptest(model))
  
  # Durbin-Watson Test for autocorrelation
  print(dwtest(model))
  
  # Run VIF For Multicollinearity
  cat("\nVariance Inflation Factors (VIF):\n")
  print(vif(model))
  
  par(mfrow = c(3, 3))
  # Run Added Variable Plots
  avPlots(model)
  
  
  #Plotting Cooks Distance and DFBETAS to find Influential Cases
  ols_plot_cooksd_bar(model)
  ols_plot_dfbetas(model)
  
  #Looking at Leverage to find outlying cases
  p <- length(coef(model))
  n <- nobs(model)
  leverage_values <- hatvalues(model)
  
  threshold <- 2 * p / n
  high_leverage_indices <- which(leverage_values > threshold)
  cat("\nInfluential Indecies based on Hat Matrix:\n")
  print(high_leverage_indices)
  
  #Looking at Cooks Distance to Find Outlying Cases
  cooks_d <- cooks.distance(model)
  threshold <- 0.01
  influential_obs <- which(cooks_d > threshold)
  cat("\nInfluential Indecies based on Cooks Distance:\n")
  print(influential_obs)
  
  #Gathering the indecies that have high leverage and high cooks distance
  common_outliers <- intersect(high_leverage_indices, influential_obs)
  
  cat("Indices that are outliers in BOTH methods:\n")
  print(common_outliers)
}


#set column names
colnames(data) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11",
                        "X12","X13","Y")

data[["Y_log"]] <- log(data[["Y"]]+1)

qualitative_vars <- c(1,2,3,5,6,7,8,9,10,11,12,13)

#prepare centered variables
for (i in qualitative_vars) {
  Xi <- data[[paste0("X", i)]]
  xi <- Xi-mean(Xi)
  data[[paste0("x", i)]] <- xi
}

#prepare quadratic variables
for (i in qualitative_vars) {
  xi <- data[[paste0("x", i)]]
  data[[paste0("x", i, "x", i)]] <- xi^2
}

#prepare log variables
for (i in 1:13) {
  Xi <- data[[paste0("X", i)]]
  data[[paste0("X", i, "_log")]] <- log(Xi+1)
}

data[["x6x13"]] <- data[["x6"]]*data[["x13"]]

#separate into training and validation set
training <- data[sample(nrow(data), 405),]
test <- data[-as.integer(rownames(training)),]

#create initial model
Housing.model <- lm(Y_log ~ x1 + x6 + x13 + X12_log + x11x11 +x6x13,
                    data=training)

summary(Housing.model)
anova(Housing.model)

Housing.valid <- lm(Y_log ~ x1 + x6 + x13 + X12_log + x11x11 +x6x13,
                    data=test)
summary(Housing.valid)
anova(Housing.valid)

#test model
n <- length(training[[1]])
n_star <- length(test[[1]])
p <- 7
p_star <- 7

MSE <- sum((training[["Y_log"]]-fitted(Housing.model))^2) /(n-p)
MSPR <- sum((test[["Y_log"]]-fitted(Housing.valid))^2) /(n_star)
MSPR/MSE
MSPR
MSE


#new model approach
models <- regsubsets(Y_log ~ x1 + x2 + X4 + x5 + x6 + x8 + x11 + 
                       x12 + x13 + X1_log + X2_log + X5_log +
                       X6_log + X7_log + X8_log + X10_log + X11_log +
                       X12_log + X13_log + x1x1 + x2x2 + x5x5 +
                       x6x6 + x8x8 + x10x10 + x11x11 + x12x12 + x13x13,
                     data=training, nbest=3, nvmax=8)
Leaps1 <- summary(models)
with(Leaps1, round(cbind(which, rsq, adjr2, cp, bic), 3))

#Y_log ~ x1 + x5 + x6 + x11 + x13 + x6_log + x8_log + x12x12

Housing.model <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X6_log + X8_log + x12x12,
                    data=training)
summary(Housing.model)
anova(Housing.model)

Housing.valid <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X6_log + X8_log + x12x12,
                    data=test)

n <- length(training[[1]])
n_star <- length(test[[1]])
p <- 9
p_star <- 9

MSE <- sum(Housing.model$residuals^2) /(n-p)
MSPR <- sum(Housing.valid$residuals^2) /(n_star)
MSPR/MSE
MSPR
MSE

ggplot(training,aes(x=(x1*x5),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x1*x6),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x1*x11),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x1*x13),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x5*x6),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x5*x11),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x5*x13),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x6*x11),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x6*x13),
                    y=Housing.model$residuals)) +
  geom_point()

ggplot(training,aes(x=(x11*x13),
                    y=Housing.model$residuals)) +
  geom_point()

training$x1x5 <- training$x1*training$x5
training$x1x6 <- training$x1*training$x6
training$x1x11 <- training$x1*training$x11
training$x1x13 <- training$x1*training$x13
training$x5x6 <- training$x5*training$x6
training$x5x11 <- training$x5*training$x11
training$x5x13 <- training$x5*training$x13
training$x6x11 <- training$x6*training$x11
training$x6x13 <- training$x6*training$x13
training$x11x13 <- training$x11*training$x13

models <- regsubsets(Y_log ~ x1 + x5 + x6 + x11 + x13 + X6_log + X8_log + x12x12 +
                       x1x5 + x1x6 + x1x11 + x1x13 + x5x6 + x5x11 + x5x13 +
                       x6x11 + x6x13 + x11x13,
                     data=training, nbest=3, nvmax=10)
Leaps1 <- summary(models)
with(Leaps1, round(cbind(which, rsq, adjr2, cp, bic), 3))

#Y_log ~ x5 + x6 + x11 + x13 + X8_log + x1x5 + x5x13 + x6x11 + x11x13

Housing.newmodel <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X8_log + x1x5 + x6x11 + x11x13,
                       data=training)
analysis_of_model(Housing.newmodel, "Hosing Model Pre Outlier", training)

ols_plot_dfbetas(Housing.newmodel)

test$x1x5 <- test$x1*test$x5
test$x6x11 <- test$x6*test$x5
test$x11x13 <- test$x11*test$x13

Housing.newvalid <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X8_log + x1x5 + x6x11 + x11x13,
                       data=test)
analysis_of_model(Housing.newvalid, "Housing Valid Before Outlier", test)

# 31 33 34 59 60 83 87 90
outliers_test<- c(31, 33, 34, 59, 60, 83, 87, 90)
test_adjusted <- test[-outliers_test, ]

Housing.outlierlessvalid <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X8_log + x1x5 + x6x11 + x11x13,
                               data=test_adjusted)

analysis_of_model(Housing.outlierlessvalid, "Housing Valid After Outlier", test_adjusted)

p <- 10
p_star <- 10
n <- length(training[[1]])
n_star <- length(test[[1]])

MSE <- sum(Housing.newmodel$residuals^2) /(n-p)
MSPR <- sum(Housing.newvalid$residuals^2) /(n_star)
MSPR/MSE
MSPR
MSE

bptest(Housing.newmodel)

analysis_of_model(Housing.newmodel,"Validation model",training)  
  
confint(Housing.newmodel)

#  7  19  40  41  66  92  98 127 159 167 170 178 212 255 277 301 342 346 357

outliers <- c(7, 19, 40, 41, 66, 92, 98, 127, 159, 167, 170, 178, 212, 255, 277, 301, 342, 346, 357)

training_adjust <- training[-outliers,]

Housing.outlierless <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X8_log + x1x5 + x6x11 + x11x13,
                          data=training_adjust)
analysis_of_model(Housing.outlierless, "Housing Model After Outlier",training_adjust)


qq_data <- qqnorm(residuals(Housing.outlierless),
                  main = paste("QQ Plot of Residuals from", "model without outliers"),
                  plot.it = TRUE)

p <- 10
p_star <- 10
n <- length(training_adjust[[1]])
n_star <- length(test_adjusted[[1]])

MSE <- sum(Housing.outlierless$residuals^2) /(n-p)
MSPR <- sum(Housing.outlierlessvalid$residuals^2) /(n_star)
sqrt(MSPR/MSE)
MSPR
MSE

#--------------------------------------------------
data$x1x5 <- data$x1 * data$x5
data$x6x11 <- data$x6 * data$x11
data$x11x13 <- data$x11 * data$x13 

Housing.corssvalFullData <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X8_log + x1x5 + x6x11 + x11x13,
                               data=data)

analysis_of_model(Housing.corssvalFullData, "Housing Full Data", data)

#11 142 146 148 149 258 263 268 366 368 369 375 381 399 406 410 411 413 415 419 <- outliers
outliers_full <- c(11, 142, 146, 148, 149, 258, 263, 268, 366, 369, 375, 381, 399, 406, 410, 411, 413, 415, 419)
data_adjusted <- data[-outliers_full,]

ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
Housing.kfoldAdjustedData <- train(Y_log ~ x1 + x5 + x6 + x11 + x13 + X8_log + x1x5 + x6x11 + x11x13,
               data=data_adjusted, method = "lm", trControl = ctrl)

print(Housing.kfoldAdjustedData)

Housing.bestKfold <- Housing.kfoldAdjustedData$finalModel
analysis_of_model(Housing.bestKfold, "K-Fold Model Result", data_adjusted)

# Retrieve the cross-validation predictions and residuals
predicted_folds <- Housing.kfoldAdjustedData$pred  # Cross-validation predictions
actual_folds <- Housing.kfoldAdjustedData$finalModel$model$Y_log  # Actual values from the model
fold_ids <- predicted_folds$Resample  # Resample IDs for each fold

# Initialize variables to store results
mspr_values <- numeric(0)
mse_values <- numeric(0)

# Loop through each fold to calculate MSPR and MSE
for (fold in unique(fold_ids)) {
  test_indices <- which(fold_ids == fold)  # Test indices for this fold
  train_indices <- setdiff(seq_along(predicted_folds$obs), test_indices) 
  
  # MSPR: Mean Squared Prediction Residual for the test data
  mspr_values[fold] <- mean((predicted_folds$obs[test_indices] - predicted_folds$pred[test_indices])^2)
  
  # MSE: Mean Squared Error for the train data
  n <- length(train_indices)
  p <- length(Housing.kfoldAdjustedData$finalModel$coefficients)
  mse_values[fold] <- sum((predicted_folds$obs[train_indices] - predicted_folds$pred[train_indices])^2)/(n-p)
}

# Average MSPR and MSE across all folds
mspr_avg <- mean(mspr_values)
mse_avg <- mean(mse_values)

# Calculate sqrt(MSPR / MSE)
result <- sqrt(mspr_avg / mse_avg)

# Print result
cat("sqrt(MSPR / MSE): ", result, "\n")
mse_avg
mspr_avg

# Residuals for the entire dataset
residuals <- predicted_folds$obs - predicted_folds$pred

par(mfrow = c(1, 1))
# Histogram of residuals
hist(residuals, 
     breaks = 20, 
     col = "skyblue", 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency")

# 95% confidence interval
mean_residuals <- mean(residuals)
se_residuals <- sd(residuals) / sqrt(length(residuals))

lower_bound <- mean_residuals - 1.96 * se_residuals
upper_bound <- mean_residuals + 1.96 * se_residuals

cat("95% Confidence Interval for Residuals: [", lower_bound, ",", upper_bound, "]\n")

confint(Housing.bestKfold)

