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



set.seed(42)

train_index <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#First Running Simple Model with all 13 Predictors
Y <- train_data$medv
X1 <- train_data$crim
X2 <- train_data$zn
X3 <- train_data$indus
X4 <- train_data$chas
X5 <- train_data$nox
X6 <- train_data$rm
X7 <- train_data$age
X8 <- train_data$dis
X9 <- train_data$rad
X10 <- train_data$tax
X11 <- train_data$ptratio
X12 <- train_data$b
X13 <- train_data$lstat


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
  
  par(mfrow = c(1, 1))
  
}


#Running all First Order Terms with Y
all_1st_order_terms.model <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, data = train_data)
analysis_of_model(all_1st_order_terms.model, "1st Order Model", train_data)


#Based on Anova and Summary, removing X3, X7, and X9
modified_1st_order_terms.model <- lm(Y ~ X1 + X2 + X4 + X5 + X6 + X8 + X10 + X11 + X12 + X13, data = train_data)
analysis_of_model(modified_1st_order_terms.model, "Modified 1st Order Model", train_data)

#Running Forward Reg With 1st Order Model
null.model <- lm(Y~1, data = train_data)
forward_1st_order.model <- step(null.model,direction="forward",scope=list(upper=all_1st_order_terms.model,lower=null.model))
analysis_of_model(forward_1st_order.model, "Forward 1st Order Model", train_data)

#Introducing Transformed Variables and Scaled Variables
x1 <- X1 - mean(X1)
x2 <- X2 - mean(X2)
x3 <- X3 - mean(X3)
x4 <- X4 - mean(X4)
x5 <- X5 - mean(X5)
x6 <- X6 - mean(X6)
x7 <- X7 - mean(X7)
x8 <- X8 - mean(X8)
x9 <- X9 - mean(X9)
x10 <- X10 - mean(X10)
x11 <- X11 - mean(X11)
x12 <- X12 - mean(X12)
x13 <- X13 - mean(X13)

Y_log10 <- log(Y, 10)
sqrtY <- sqrt(Y)
lnY <- log(Y, exp(1))

#Running Transformed Model with and without scaled variables
all_1st_log10Y.model <- lm(Y_log10 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, data = train_data)
analysis_of_model(all_1st_log10Y.model, "1st Order Model With Log_10Y", train_data)

all_1stLsqrtY.model <- lm(sqrtY ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, data = train_data)
analysis_of_model(all_1stLsqrtY.model, "1st Order Model With sqrtY", train_data)

all_1stlnY.model <- lm(lnY ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13, data = train_data)
analysis_of_model(all_1stlnY.model, "1st Order Model With lmY", train_data)

#Now Include scaled variables
scaled_1st_log10Y.model <- lm(Y_log10 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, data = train_data)
analysis_of_model(scaled_1st_log10Y.model, "1st Order Scaled Model With Log_10Y", train_data)

#Running forward regression on log model
forward_log_1st_order.model <- step(null.model,direction="forward",scope=list(upper=all_1st_log10Y.model,lower=null.model))
analysis_of_model(forward_log_1st_order.model, "Forward Log 1st Order Model", train_data)


#Notes from transformed models, X9 and X10 are heavily correlated, need to take one or both of them out 
#Scaled variables did not seem very impactful, will not be adding them

#General note: There are a good 20-30 outliers that can be removed, this will be noted down in a data table
#and we will see which ones tend to be

#Adding some quadratic and log predictors in the model


logX1 <- log(X1,10)
X13X13 <- x13^2
sqrtX8 <- sqrt(X8)
logX8 <- log(X8, 10)
logX8_2 <- logX8^2    #Turned out to be useless, and caused large amounts of multicolinearity
logX5 <- log(X5, 10)

#Adding in these terms, taking out X3, X7, and X9 based off past results
x_transformed_modified.model <- lm(Y_log10 ~ logX1 + X2 + X4 + logX5 + X6 + logX8 + X10 + X11 + X12 + X13 + X13X13, data = train_data)
analysis_of_model(x_transformed_modified.model, "Transformed X variable Model")

#Adding some Interaction Variables that could be influential
logX1logX8 <- logX1 * logX8 # 1. log(Crime) × log(Distance)
logX5X4 <- logX5 * X4       # 2. Pollution × River Dummy
X6X13 <- X6 * X13           # 3. Rooms × Lower Status
logX5logX8 <- logX5 * logX8 # 4. log(Pollution) × log(Distance)
logX1X11 <- logX1 * X11     # 5. log(Crime) × Pupil-Teacher Ratio
logX8X10 <- logX8 * X10     # 6. log(Distance) × Tax
X6X7 <- X6 * X7             # 7. Rooms × Age
X12X13 <- X12 * X13         # 8. Racial Proportion × Lower Status



#Looking at models with Leaps
subset_model <- regsubsets(Y_log10 ~ logX1 + X2 + X4 + logX5 + X6 + X7 + logX8 + X10 + X11 + X12 + X13 + 
                             X13X13 + logX1logX8 + X5X4 + logX5logX8 + logX1X11 + 
                             logX8X10 + X6X7 + X12X13
                             , data = train_data, nvmax = 10, nbest=3)  

# Extract summary
summary_subset <- summary(subset_model)
with(summary_subset, round(cbind(which, rsq, adjr2, cp, bic), 3))

leaps.model <- lm(Y_log10 ~ logX1 + X4 + logX5 + X6 + X12 + X13 + X13X13 + 
                     logX1logX8 + X6X13 + logX5logX8, data = train_data)
analysis_of_model(leaps.model, "Model From Leaps Code")

#From leaps code, noticed X13 needs to be centered, meaning X13X13 and X6X13 will change. 
#I will take out the outliers and re run the code to see if taking those out will help. 
#Model Performance isn't anything crazy.

x13x13 <- x13^2
X6x13 <- x13 * X6

outlying_cases <- c(1, 4, 7, 20, 64, 92, 127, 133, 167, 170, 212, 255, 338, 341, 346, 347, 351)
train_wo_outliers <- train_data[-outlying_cases,]

leaps_modified.model <- lm(Y_log10 ~ logX1 + X4 + logX5 + X6 + X12 + x13 + x13x13 + 
                    logX1logX8  + logX5logX8, data = train_data)
analysis_of_model(leaps_modified.model, "Model From Leaps Code")


#Trying some other transformation, on clean data this time, to ensure 
x5x5 <- x5^2
x11x11 <- x11^2
sqrtX12 <- sqrt(400 - X12)
x13x13<- x13^2

age_quantiles <- quantile(X7, probs = c(0, 0.25, 0.5, 0.75, 1))

X70_25 <- ifelse(X7 > age_quantiles[1] & X7 <= age_quantiles[2], 1, 0)
X725_50 <- ifelse(X7 > age_quantiles[2] & X7 <= age_quantiles[3], 1, 0)
X750_75 <- ifelse(X7 > age_quantiles[3] & X7 <= age_quantiles[4], 1, 0)
X775_100 <- ifelse(X7 > age_quantiles[4] & X7 <= age_quantiles[5], 1, 0)

#Adding the addional interaction terms 
X6X70_25 <- x6 * X70_25    # 7. Rooms × Age (0-25)
X6X725_50 <- x6 * X725_50   # 8. Rooms × Age (0-25)
X6X750_75 <- x6 * X750_75 # 9. Rooms × Age (0-25)
X6X775_100 <- x6 * X775_100 # 10. Rooms × Age (0-25)
sqrtX12X13 <- sqrtX12 * x13 # 11. Racial Proportion × Lower Status
x5X4 <- x5*X4
x6x13 <- x6 * x13
x5logX8 <- x5*logX8


#Deleted X6X775_100_clean because it was linearly dependent based off alias
alias(lm(logY_10 ~ logX1 + x2 + X4 + x5 + 
           X6 + X70_25 + X725_50 + X750_75 + X775_100 + 
           logX8_clean + X10_clean + x11_clean + sqrtX12_clean + x13_clean + x5x5_clean +
           x11x11_clean + x13x13_clean + logX1logX8_clean + x5X4_clean + x6x13_clean + 
           x5logX8_clean + logX1X11_clean + logX8X10_clean + X6X70_25_clean + X6X725_50_clean + 
           X6X750_75_clean + X6X775_100_clean + sqrtX12X13_clean, data = train_wo_outliers))

modified_subset_model <- regsubsets(Y_log10 ~ logX1 + x2 + X4 + x5 + 
                                      X6 + X70_25 + X725_50 + X750_75 + X775_100 + 
                                      logX8 + X10 + x11 + sqrtX12 + x13 + x5x5 +
                                      x11x11 + x13x13 + logX1logX8 + x5X4 + x6x13 + 
                                      x5logX8 + logX1X11 + logX8X10 + X6X70_25 + X6X725_50 + 
                                      X6X750_75 + sqrtX12X13, data = train_data, nvmax = 15, nbest=3)

options(max.print = 2000) 
summary_subset <- summary(modified_subset_model)
with(summary_subset, round(cbind(which, rsq, adjr2, cp, bic), 3))

best_model_index <- which.max(summary_subset$rsq)
model_vars <- names(coef(modified_subset_model, best_model_index))[-1]
model_formula <- as.formula(paste("Y_log10 ~", paste(model_vars, collapse = " + ")))
best_lm_model <- lm(model_formula, data = train_data)
analysis_of_model(best_lm_model, "Model Based on Leaps", data = train_data)

modified_leaps1.model <- lm(Y_log10 ~ logX1 + x5 + x6 + logX8 + x11 + 
                                sqrtX12 + x13 + x6x13 + logX1logX8 + X6X750_75 + sqrtX12X13, data = train_data)
analysis_of_model(modified_leaps1.model, "Modified Leaps", train_data)

#Took out some variables
modified_leaps2.model <- lm(Y_log10 ~ logX1 + x5 + x6 + X750_75 + logX8 + x11 + x13 + x6x13 + logX1logX8 + X6X750_75, data = train_data)
analysis_of_model(modified_leaps2.model, "Modified Leaps part 2", train_data)

#  4  19  51  64  78  92  98 127 133 167 170 212 220 301 338 342 346 347 351
leaps_outliers <- c(4, 19, 51, 64, 78, 92, 98, 127, 133, 167, 170, 212, 220, 301, 338, 342, 346, 347, 351)
train_wo_leaps_outliers <- train_data[-leaps_outliers, ]

Y_log10_clean <- log(train_wo_leaps_outliers$medv, 10)

logX1_clean <- log(train_wo_leaps_outliers$crim, 10)
logX8_clean <- log(train_wo_leaps_outliers$dis, 10)
x5_clean <- mean(train_wo_leaps_outliers$nox) - train_wo_leaps_outliers$nox
x6_clean <- mean(train_wo_leaps_outliers$rm) - train_wo_leaps_outliers$rm
x11_clean <- mean(train_wo_leaps_outliers$ptratio) - train_wo_leaps_outliers$ptratio
x13_clean <- mean(train_wo_leaps_outliers$lstat) - train_wo_leaps_outliers$lstat
logX1logX8_clean <- logX1_clean * logX8_clean
x6x13_clean <- x6_clean * (mean(train_wo_leaps_outliers$lstat)-train_wo_leaps_outliers$lstat)
X750_75_clean <- ifelse(train_wo_leaps_outliers$age > age_quantiles[3] & train_wo_leaps_outliers$age <= age_quantiles[4], 1, 0)
x6X750_75_clean <- x6_clean * X750_75_clean

modified_leaps2_clean.model <- lm(Y_log10_clean ~ logX1_clean + x5_clean + x6_clean + X750_75_clean + logX8_clean + x11_clean + x13_clean + x6x13_clean + logX1logX8_clean + x6X750_75_clean, data = train_wo_leaps_outliers)
analysis_of_model(modified_leaps2_clean.model, "Modified Leaps part 2", train_wo_leaps_outliers)


Y_log10 <- log(test_data$medv, 10)
logX1 <- log(test_data$crim, 10)
logX8 <- log(test_data$dis, 10)
x5 <- mean(train_wo_leaps_outliers$nox) - test_data$nox
x6 <- mean(train_wo_leaps_outliers$rm) - test_data$rm
x11 <- mean(train_wo_leaps_outliers$ptratio) - test_data$ptratio
x13 <- mean(train_wo_leaps_outliers$lstat) - test_data$lstat
logX1logX8 <- logX1 * logX8
x6x13 <- x6 * (mean(train_wo_leaps_outliers$lstat) - test_data$lstat)
X750_75 <- ifelse(test_data$age > age_quantiles[3] & test_data$age <= age_quantiles[4], 1, 0)
x6X750_75 <- x6 * X750_75

modified_leaps2_test.model <- lm(Y_log10 ~ logX1 + x5 + x6 + logX8 + x11 + x13 + x6x13 + logX1logX8 + x6X750_75, data = test_data)

# Calculate MSPR (Mean Squared Prediction Error)
MSPR <- sum(modified_leaps2_test.model$residuals^2)/df.residual(modified_leaps2_test.model)

# Retrieve MSE (Mean Squared Error) from the training model
MSE <- sum(resid(modified_leaps2_clean.model)^2) / df.residual(modified_leaps2_clean.model)

# Calculate the square root of the ratio
sqrt_MSPR_MSE <- sqrt(MSPR / MSE)

# Print the result
sqrt_MSPR_MSE

#-----------------------------------------------
data$Y_log10 <- log(data$medv, 10)
data$logX1 <- log(data$crim, 10)
data$logX8 <- log(data$dis, 10)
data$x5 <- mean(data$nox) - data$nox
data$x6 <- mean(data$rm) - data$rm
data$x11 <- mean(data$ptratio) - data$ptratio
data$x13 <- mean(data$lstat) - data$lstat
data$logX1logX8 <- logX1 * logX8
data$x6x13 <- x6 * (mean(data$lstat) - data$lstat)
data$X750_75 <- ifelse(data$age > age_quantiles[3] & data$age <= age_quantiles[4], 1, 0)
data$x6X750_75 <- x6 * X750_75

ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
Housing.kfoldAdjustedData <- train(Y_log10 ~ logX1 + x5 + x6 + logX8 + x11 + x13 + x6x13 + logX1logX8 + x6X750_75,
                                   data=data, method = "lm", trControl = ctrl)

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



