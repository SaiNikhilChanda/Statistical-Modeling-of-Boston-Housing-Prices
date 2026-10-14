data <- read.csv("C:/Users/aweso/OneDrive - Clark University/Year 3 Semester 2/MATH 219 - Linear Models/Final Project/BostonHousing.csv", header = TRUE)

library(rsm)
library(lmtest)

attach(data)
Y <- medv
Y_log <- log(Y, 10)
n <- length(Y)

original_vars <- names(data)[1:13]

qual_variables <- c(1,2,3,5,6,7,8,9,10,11,12,13)

#Assign X1 to X13
for (i in qual_variables) {
  xi <- data[[original_vars[i]]]
  assign(paste0("x", i), xi-mean(xi))
}

x4 <- data[[4]]

#Assign squared terms (X1_1, X2_2, ..., X13_13)
for (i in qual_variables) {
  xi <- get(paste0("x", i))
  assign(paste0("x", i, "_", i), xi^2)
}

#Assign interaction terms (X1_2, X1_3, ..., X12_13)
for (i in 1:12) {
  for (j in (i+1):13) {
    xi <- get(paste0("x", i))
    xj <- get(paste0("x", j))
    assign(paste0("x", i, "_", j), xi * xj)
  }
}

all_vars <- ls(pattern = "^x\\d+(_\\d+)?$")  # Matches X1, X2_3, X13_13, etc.

formula_str <- paste("Y ~", paste(all_vars, collapse = " + "))

final_formula <- as.formula(formula_str)

full_model <- lm(final_formula)
null_model <- lm(Y~1, data=data)

backward.reg <- step(full_model,direction="backward",k=log(n))

summary(backward.reg)

forward.reg <- step(null_model,direction="forward",scope=list(upper=full_model,lower=null_model))

summary(forward.reg)

# Residual Plot (Residuals vs Fitted values)
plot(fitted(forward.reg), residuals(forward.reg), 
     main = "Residuals vs Fitted from Forward Reg", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

plot(fitted(backward.reg), residuals(backward.reg), 
     main = "Residuals vs Fitted from Backward Reg", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

# Run Breusch-Pagan test
bptest(forward.reg)
bptest(backward.reg)

# Run the Durbin-Watson test on your model
dwtest(forward.reg)
dwtest(backward.reg)


# QQ Plot for residuals, at alpha = .05 -> correlation_coefficient = .987
qqnorm(residuals(forward.reg), main = "QQ Plot of Residuals from Forward Reg")
qqline(residuals(forward.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(forward.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient

qqnorm(residuals(backward.reg), main = "QQ Plot of Residuals from Backward Reg")
qqline(residuals(backward.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(backward.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient

#Repeat Same Process except with transformed Y variable
formula_str <- paste("Y_log ~", paste(all_vars, collapse = " + "))

final_formula <- as.formula(formula_str)

full_transformed.model <- lm(final_formula)

null_transformed_model <- lm(Y_log ~ 1)

backward_transformed.reg <- step(full_transformed.model,direction="backward",k=log(n))
forward_transformed.reg <- step(null_transformed_model,direction="forward",scope=list(upper=full_transformed.model,lower=null_transformed_model))


summary(backward_transformed.reg)
summary(forward_transformed.reg)

# Residual Plot (Residuals vs Fitted values)
plot(fitted(forward_transformed.reg), residuals(forward_transformed.reg), 
     main = "Residuals vs Fitted from Forward Reg (Transformed)", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

plot(fitted(backward_transformed.reg), residuals(backward_transformed.reg), 
     main = "Residuals vs Fitted from Backward Reg (Transformed)", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

#Run Breusch-Pagan test
bptest(forward_transformed.reg)
bptest(backward_transformed.reg)

# Run the Durbin-Watson test on your model
dwtest(forward_transformed.reg)
dwtest(backward_transformed.reg)

# QQ Plot for residuals, at alpha = .05 -> correlation_coefficient = .987
qqnorm(residuals(forward_transformed.reg), main = "QQ Plot of Residuals from Forward Reg (Transformed)")
qqline(residuals(forward_transformed.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(forward_transformed.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient

qqnorm(residuals(backward_transformed.reg), main = "QQ Plot of Residuals from Backward Reg (Transformed)")
qqline(residuals(backward_transformed.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(backward_transformed.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient

#log transformation of X variables
for (i in qual_variables) {
  xi <- data[[original_vars[i]]]
  assign(paste0("X", i, "_log"), log(xi+1))
}

log_vars <- ls(pattern = "^X\\d+_log$")

log_formula_str <- paste(paste(formula_str, "+"), paste(log_vars, collapse=" + "))

log_final_formula <- as.formula(log_formula_str)

log_full_model <- lm(log_final_formula)
null_model <- lm(Y~1, data=data)

log_backward.reg <- step(log_full_model,direction="backward",k=log(n))

summary(log_backward.reg)

log_forward.reg <- step(null_model,direction="forward",scope=list(upper=log_full_model,lower=null_model))

summary(log_forward.reg)

# Residual Plot (Residuals vs Fitted values)
plot(fitted(log_forward.reg), residuals(log_forward.reg), 
     main = "Residuals vs Fitted from Forward Reg with added Log", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

plot(fitted(log_backward.reg), residuals(log_backward.reg), 
     main = "Residuals vs Fitted from Backward Reg with added Log", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

# Run Breusch-Pagan test
bptest(log_forward.reg)
bptest(log_backward.reg)

# Run the Durbin-Watson test on your model
dwtest(log_forward.reg)
dwtest(log_backward.reg)

# QQ Plot for residuals, at alpha = .05 -> correlation_coefficient = .987
qqnorm(residuals(log_forward.reg), main = "QQ Plot of Residuals from Forward Reg with added Log")
qqline(residuals(log_forward.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(log_forward.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient

qqnorm(residuals(log_backward.reg), main = "QQ Plot of Residuals from Backward Reg with added Log")
qqline(residuals(log_backward.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(log_backward.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient



#Repeat Same Process except with transformed Y variable and added Log
formula_str <- paste("Y_log ~", paste(all_vars, collapse = " + "))

log_formula_str_transformed <- paste(paste(formula_str, "+"), paste(log_vars, collapse=" + "))

log_final_formula_transformed <- as.formula(log_formula_str_transformed)

log_full_transformed.model <- lm(log_final_formula_transformed)

null_transformed_model <- lm(Y_log ~ 1)

log_backward_transformed.reg <- step(log_full_transformed.model,direction="backward",k=log(n))
log_forward_transformed.reg <- step(null_transformed_model,direction="forward",scope=list(upper=log_full_transformed.model,lower=null_transformed_model))


summary(log_backward_transformed.reg)
summary(log_forward_transformed.reg)

# Residual Plot (Residuals vs Fitted values)
plot(fitted(log_forward_transformed.reg), residuals(log_forward_transformed.reg), 
     main = "Residuals vs Fitted from Forward Reg (Transformed)", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

plot(fitted(log_backward_transformed.reg), residuals(log_backward_transformed.reg), 
     main = "Residuals vs Fitted from Backward Reg (Transformed)", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference

#Run Breusch-Pagan test
bptest(log_forward_transformed.reg)
bptest(log_backward_transformed.reg)

# Run the Durbin-Watson test on your model
dwtest(log_forward_transformed.reg)
dwtest(log_backward_transformed.reg)

# QQ Plot for residuals, at alpha = .05 -> correlation_coefficient = .987
qqnorm(residuals(log_forward_transformed.reg), main = "QQ Plot of Residuals from Forward Reg (Transformed)")
qqline(residuals(log_forward_transformed.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(log_forward_transformed.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient

qqnorm(residuals(log_backward_transformed.reg), main = "QQ Plot of Residuals from Backward Reg (Transformed)")
qqline(residuals(log_backward_transformed.reg), col = "red")  # Add QQ line
qq_data <- qqnorm(residuals(log_backward_transformed.reg), plot.it=FALSE)
obtained_residuals <- qq_data$y
expected_residuals <- qq_data$x

correlation_coefficient <- cor(obtained_residuals, expected_residuals)
correlation_coefficient

