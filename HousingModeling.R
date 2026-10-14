data <- read.csv("C:/Users/aweso/OneDrive - Clark University/Year 3 Semester 2/MATH 219 - Linear Models/Final Project/BostonHousing.csv", header = TRUE)

library(rsm)
library(lmtest)

attach(data)
Y <- medv
Y_log <- log(Y, 10)
n <- length(Y)

original_vars <- names(data)[1:13]

#Assign X1 to X13
for (i in 1:13) {
  assign(paste0("X", i), data[[original_vars[i]]])
}

#Assign squared terms (X1_1, X2_2, ..., X13_13)
for (i in 1:13) {
  xi <- get(paste0("X", i))
  assign(paste0("X", i, "_", i), xi^2)
}

#Assign interaction terms (X1_2, X1_3, ..., X12_13)
for (i in 1:12) {
  for (j in (i+1):13) {
    xi <- get(paste0("X", i))
    xj <- get(paste0("X", j))
    assign(paste0("X", i, "_", j), xi * xj)
  }
}

all_vars <- ls(pattern = "^X\\d+(_\\d+)?$")  # Matches X1, X2_3, X13_13, etc.

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

backward_transformed.reg <- step(full_transformed.model,direction="backward",k=log(n))
forward_transformed.reg <- step(null_model,direction="forward",scope=list(upper=full_transformed.model,lower=null_model))


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