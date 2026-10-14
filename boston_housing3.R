# Boston Housing
# ------------------------------------------------------------------------------
# 1. Exploratory Data Analysis
# ------------------------------------------------------------------------------
housing <- read.csv("~/Desktop/Clark U/Spring 2024/MATH 219/Final Project/BostonHousing.csv",
                    sep=",", header=TRUE)
library(ggplot2)
library(GGally)
library(olsrr)
library(rsm)
library(lmtest)
library(onewaytests)
library(DAAG)

# Assign easy variable names
set.seed(42)
train_index <- sample(nrow(housing), 405)
train <- housing[train_index, ]
test <- housing[-train_index, ]
X1 <- train$crim
X2 <- train$zn
X3 <- train$indus
X4 <- train$chas
X5 <- train$nox
X6 <- train$rm
X7 <- train$age
X8 <- train$dis
X9 <- train$rad
X10 <- train$tax
X11 <- train$ptratio
X12 <- train$b
X13 <- train$lstat
Y <- train$medv
n_train <- length(Y)
colnames(housing) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                       "X10", "X11", "X12", "X13", "Y")

# Create and explore variable categories
poll <- as.matrix(X5)
loc <- as.matrix(cbind(X8, X9))
descrip <- as.matrix(cbind(X6, X7))
town <- as.matrix(cbind(X1, X2, X3, X4, X10, X11, X12, X13))
X <- as.matrix(cbind(rep(1,n_train), X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13))

ggpairs(loc)
ggpairs(descrip)
ggpairs(town)

head(X)
summary(X)

# ------------------------------------------------------------------------------
# 2. Model Building
# ------------------------------------------------------------------------------
Y_log <- log(Y)
x1 <- X1 - mean(X1)
x5 <- X5 - mean(X5)
x6 <- X6 - mean(X6)
x11 <- X11 - mean(X11)
x13 <- X13 - mean(X13)
X8_log <- log(X8)
x1_5 <- x1 * x5
x6_11 <- x6 * x11
x11_13 <- x11 * x13

for (i in 1:13) {
  boxplot(get(paste0("X", i)), main=paste0("X", i, " Boxplot"))
}
boxplot(Y, main="Y Boxplot")

par(mfrow = c(1, 2))
for (i in 1:13) {
  plot(get(paste0("X", i)), Y, xlab=paste0("X", i), main=paste0("Y-Values vs X", i))
  plot(get(paste0("X", i)), Y_log, xlab=paste0("X", i), ylab="Log(Y)", main=paste0("Log(Y)-Values vs X", i))
}

final_model <- lm(Y_log ~ x1 + x5 + x6 + x11 + x13 + X8_log + x1_5 + x6_11 + x11_13)
summary(final_model)
anova(final_model)

# ------------------------------------------------------------------------------
# 3. Residual Analysis
# ------------------------------------------------------------------------------
par(mfrow = c(1, 1))
plot(fitted(final_model), residuals(final_model), xlab="Fitted Values", ylab="Reisduals",
     main="Residuals vs. Fitted Values for Final Model")
# Plot shows residuals are balanced around the origin and fitted values are centralized
# between 2.5 and 3.5. There are likely many outlying cases and influential points.

for (i in 1:13) {
  plot(get(paste0("X", i)), residuals(final_model), xlab=paste0("X", i), ylab="Residuals",
       main=paste0("Residuals vs X", i, " for Final Model"))
  abline(h=0)
}
# Scatterplots reveal that a linear relationship is appropriate for modeling Y_log
# with most predictor variables, except X13. All other plots are symmetrical
# around e_i = 0, except X13 which appears to have a slight curve and more negative
# residuals than positive ones.

boxplot(residuals(final_model), main="Boxplot of Residuals for Final Model")
# Residuals appear normally distributed except for the many outliers on both sides.

# ------------------------------------------------------------------------------
# 4. Model Assumptions
# ------------------------------------------------------------------------------
# Assumption: Errors are Normally Distributed
qqnorm(residuals(final_model), main="Normal Probability Plot of Residuals for final Model")
qqline(residuals(final_model))
qq_data <- qqnorm(residuals(final_model), plot.it=FALSE)
residual_cor <- cor(qq_data$y, qq_data$x)
residual_cor
# Interpretation: The correlation between the residuals and their expected values is 0.980,
# which is less than the critical value (>0.987) so the assumption of normal errors is
# violated. This can be seen in the qqplot, where there is symmetric tailing from many outliers.

# Assumption: Errors have constant variance
# Due to the normal error assumption being violated, we cannot conduct the Breusch-Pagan
# Test. So, we will use the Brown-Forsythe Test which is robust against non-normality of errors.
BFmatrix <- cbind(fitted(final_model), residuals(final_model))
BFmatrix <- BFmatrix[order(BFmatrix[, 1]), ]
group1 <- BFmatrix[1:203, 1:2]
group2 <- BFmatrix[204:405, 1:2]
n1 <- nrow(group1)
n2 <- nrow(group2)
e1 <- median(group1[,2])
e2 <- median(group2[,2])
d1 <- abs(group1[,2] - e1)
d2 <- abs(group2[,2] - e2)
d1bar <- mean(d1)
d2bar <- mean(d2)
d1diff <- d1 - d1bar
d2diff <- d2 - d2bar
s2_BF <- ((t(d1diff) %*% d1diff) + (t(d2diff) %*% d2diff)) / (n_train - 2)
s_BF <- sqrt(s2_BF)

# Conduct Test
alpha <- 0.05
tstar <- (d1bar - d2bar) / (s_BF * sqrt(1/n1 + 1/n2))
tstat <- qt(1 - alpha/2, n_train - 2)
tstar
tstat
# Conclusion: Errors have non-constant variance and model assumption is violated.

# Assumption: Errors are Random
dwtest(final_model)
# Interpretation: The test p-value is much greater than 0.05, so the errors are not autocorrelated.

# Multicollinearity Check
vif(final_model)
mean(vif(final_model))
# X1 has a high multicollinearity value, but the mean is less than 5, so we proceed.

# ------------------------------------------------------------------------------
# 5. Outlier Analysis
# ------------------------------------------------------------------------------
# Getting basic stats
anova(final_model)
SSE_final <- anova(final_model)$"Sum Sq"[10]
MSE_final <- anova(final_model)$"Mean Sq"[10]
p_final <- length(final_model$coefficients)

# Leverage Values - Outlying X cases
leverage_vals <- hatvalues(final_model)
outlying_x_cases <- which(leverage_vals > (2 * p_final / n_train))
outlying_x_cases
# Cases 1, 4, 7, 14, 19, 40, 41, 49, 53, 59, 65, 66, 72, 78, 92, 98, 102, 104,
# 127, 159, 165, 167, 170, 177, 178, 209, 212, 218, 237, 255, 256, 277, 301,
# 316, 336, 341, 342, 346, 347, 357, 363, 364 are ALL outlying X-cases. (42 cases)

# Studentized Deleted Residuals - Outlying Y cases
t_i <- residuals(final_model) * sqrt((n_train - p_final - 1) / (rep(SSE_final, n_train) * (1 - leverage_vals) - residuals(final_model)^2))
outlying_y_cases <- which(t_i > (qt(1 - (alpha / (2*n_train)), n_train - p_final - 1)))
outlying_y_cases
# Case 62 is the only outlying Y-case, but it did not appear as an outlying X-case.

# Combine all outlying cases
cases <- c(outlying_x_cases, outlying_y_cases)

# DFFITS - Influential Measure
DFFITS <- t_i[cases] * leverage_vals[cases] / (1 - leverage_vals[cases])
dffits_influential <- which(DFFITS > (2 / sqrt(p_final / n_train)))

# DFBETAS - Influential Measure
DFBETAS <- dfbetas(final_model)
count_exceeding <- rowSums(abs(DFBETAS) > (2 / sqrt(n_train)))
dfbetas_influential <- which(count_exceeding > 0)
influential_obs <- data.frame(
  Index = dfbetas_influential,
  Count = count_exceeding[dfbetas_influential]
)
influential_obs <- cbind(influential_obs, train_index[influential_obs$Index])[,3:2]
influential_obs <- influential_obs[order(-influential_obs$Count), ]
influential_obs

# Cook's Distance - Influential Measure
Cooks_D <- residuals(final_model)[cases]^2 / (p_final * MSE_final) * leverage_vals[cases] / (1 - leverage_vals[cases])^2
which(Cooks_D > qf(1 - 0.5, p_final, n_train - p_final))

# Find common cases
common_cases <- intersect(influential_obs[,1], cases)
common_cases <- unique(c(common_cases, influential_obs[1:12,1]))
common_cases
length(common_cases)
plot(final_model)
x1 <- X1 - mean(X1)

# Outlier Analysis

