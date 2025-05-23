
# -------------- Polynomial regression --------------


# Load profsalary.txt file
profsalary_dts <- read.delim(file="profsalary.txt", header=TRUE)
profsalary_dts

### a) Build a simple linear regression model to explain the variable 'salary' (Y) in terms of the variable
# 'years of experience' (X).

x_experience <- profsalary_dts$Experience
y_salary <- profsalary_dts$Salary

lm_salary <- lm(y_salary ~ x_experience)
summary(lm_salary)


# Scatterplot alone
par(mfrow=c(1,1))
plot(x_experience, y_salary, xlab="Years of experience", ylab="Salary", 
     main="Years of experience vs Salary", col="blue")
grid()

# Add the regression line (case 1)
abline(a=48.50593, b=0.88345, col="red")


# Diagnostic plots
par(mfrow=c(2,2))
plot(lm_salary)

# It looks pretty normal, it has some bad residual vs leverage points 


### b) Add another variable in the form of X2

# b.1) Plot the new model and check diagnostic plots
lm_salary_sq <- lm(Salary ~ poly(Experience, 2, raw=TRUE), data=profsalary_dts)
summary(lm_salary_sq)

beta0 <- 34.720498
beta1 <- 2.872275
beta2 <- -0.053316


# Scatterplot alone
par(mfrow=c(1,1))
plot(x_experience, y_salary, xlab="Years of experience", ylab="Salary", 
     main="Years of experience vs Salary", col="blue")

x_grid_sal <- seq(-1, 38, length.out=100)
#y_grid_sal <- (-0.053316 * x_grid_sal^2) + (2.872275 * x_grid_sal) + 34.720498
y_grid_sal <- (beta2 * x_grid_sal^2) + (beta1 * x_grid_sal) + beta0 
lines(x_grid_sal, y_grid_sal, col="red")
grid()


# Diagnostic plots
par(mfrow=c(2,2))
plot(lm_salary_sq)


# b.2) ANOVA table

# Model predictions
salary_hat <- beta0 + beta1 * x_experience + beta2 * x_experience^2


# Degrees of freedom
n_sal <- length(y_salary)  # Sample size
p <- 2  # Two predictors: x_experience and x_experience^2
dof_regression <- p # Degrees of freedom for regression
dof_error <- n_sal - (p + 1) # Residual DOF (n - p - 1)
dof_total <- n_sal - 1# Total DOF (n - 1)

# Sum of Squares
SST <- sum((y_salary - mean(y_salary))^2)  # Total Sum of Squares
SS_reg <- sum((salary_hat - mean(y_salary))^2) # Sum of Squares due to regression
RSS <- sum((y_salary - salary_hat)^2) # Residual Sum of Squares

# Mean Squares
MSR <- SS_reg / dof_regression
MSE <- RSS / dof_error

# F-statistic
F_value <- MSR / MSE

# p-value
p_value <- pf(F_value, dof_regression, dof_error, lower.tail = FALSE)

# Create ANOVA DataFrame
anova_df <- data.frame(
  Source = c("Regression", "Residual", "Total"),
  Sum_square = c(SS_reg, RSS, SST),
  DoF = c(dof_regression, dof_error, dof_total),
  Mean_square = c(MSR, MSE, NA),
  F_ratio = c(F_value, NA, NA),
  p_value = c(p_value, NA, NA)
)

# Print ANOVA table
print(anova_df)

# Compare with function in ANOVA
anova(lm_salary_sq)


anova(lm_salary_sq, lm_salary)


# b1) 
# Modelo nulo (solo la media de Salary)
lm_null <- lm(Salary ~ 1, data=profsalary_dts)

# Modelo cuadrático (con Experience y Experience²)
lm_salary_sq <- lm(Salary ~ Experience + I(Experience^2), data=profsalary_dts)

# Tabla ANOVA y prueba de hipótesis
anova(lm_null, lm_salary_sq)



# b2) Confidence intervals for the overall estimators
confidence_level <- 0.99
significance_level <- 1 - confidence_level

critical_value <- qt(significance_level/2, n_sal - 3, lower.tail = FALSE)
critical_value # 2.611403

beta0_low  <- beta0 - (critical_value * 0.828724)
beta0_up  <- beta0 + (critical_value * 0.828724)


beta1_low  <- beta1 - (critical_value * 0.095697)
beta1_up  <- beta1 + (critical_value * 0.095697)


beta2_low  <- beta2 - (critical_value * 0.002477)
beta2_up  <- beta2 + (critical_value * 0.002477)


# Print results
cat("Confidence Intervals (99% Confidence):\n")
cat("Beta0: [", beta0_low, ",", beta0_up, "]\n")
cat("Beta1: [", beta1_low, ",", beta1_up, "]\n")
cat("Beta2: [", beta2_low, ",", beta2_up, "]\n")



# b3) Compute Bonferroni-adjusted confidence intervals
bonferroni_intervals <- compute_bonferroni_ci(lm_salary_sq)
print(bonferroni_intervals)


critical_value_bonferroni <- qt(1 - (0.00333 / 2), 140)
critical_value_bonferroni

se_beta0 <- 0.828724
se_beta1 <- 0.095697
se_beta2 <- 0.002477

# Calculate the adjusted CI by Bonferroni
beta0_low_bonf  <- beta0 - (critical_value_bonferroni * se_beta0)
beta0_up_bonf   <- beta0 + (critical_value_bonferroni * se_beta0)

beta1_low_bonf  <- beta1 - (critical_value_bonferroni * se_beta1)
beta1_up_bonf   <- beta1 + (critical_value_bonferroni * se_beta1)

beta2_low_bonf  <- beta2 - (critical_value_bonferroni * se_beta2)
beta2_up_bonf   <- beta2 + (critical_value_bonferroni * se_beta2)

# Print results
cat("Bonferroni Confidence Intervals (99% Overall Confidence):\n")
cat("Beta0: [", beta0_low_bonf, ",", beta0_up_bonf, "]\n")
cat("Beta1: [", beta1_low_bonf, ",", beta1_up_bonf, "]\n")
cat("Beta2: [", beta2_low_bonf, ",", beta2_up_bonf, "]\n")



# b5) Compute the prediction for a person that has worked 18 years
# Given residual variance (MSE)
s2 <- 7.9
s <- sqrt(s2)  # Residual standard error

# Sample size and degrees of freedom
n <- 143  
df <- n - 3  # Degrees of freedom for the model

# New data point (18 years of experience)
X_new <- 18

# Point estimation
Y_hat <- beta0 + beta1 * X_new + beta2 * (X_new^2)
cat("Point estimate for salary with 18 years of experience:", Y_hat, "\n")

# Mean and sum of squares for Experience
X_mean <- mean(profsalary_dts$Experience)  
Sxx <- sum((profsalary_dts$Experience - X_mean)^2)  

# Critical value for 90% confidence interval
alpha <- 0.10
t_critical <- qt(1 - alpha/2, df)

# Standard error for the mean estimate
SE_mean <- s * sqrt((1/n) + ((X_new - X_mean)^2 / Sxx))

# Confidence Interval for Mean Salary
CI_mu_low <- Y_hat - (t_critical * SE_mean)
CI_mu_up <- Y_hat + (t_critical * SE_mean)
cat("90% Confidence Interval for Mean Salary:", "[", CI_mu_low, ",", CI_mu_up, "]\n")

# Standard error for the prediction interval
SE_pred <- s * sqrt(1 + (1/n) + ((X_new - X_mean)^2 / Sxx))

# Prediction Interval for Salary
CI_pred_low <- Y_hat - (t_critical * SE_pred)
CI_pred_up <- Y_hat + (t_critical * SE_pred)
cat("90% Prediction Interval for Salary:", "[", CI_pred_low, ",", CI_pred_up, "]\n")



# c) Model with three predictors (cubic polynomial)
lm_salary_3rd <- lm(Salary ~ Experience + I(Experience^2) + I(Experience^3), data=profsalary_dts)
summary(lm_salary_3rd)

# Compare with quadratic model
anova(lm_salary_sq, lm_salary_3rd)

# Scatterplot
par(mfrow=c(1,1))
plot(profsalary_dts$Experience, profsalary_dts$Salary, xlab="Years of experience", 
     ylab="Salary", main="Years of Experience vs Salary", col="blue")

# Generate predictions
x_grid_sal3 <- seq(min(profsalary_dts$Experience), max(profsalary_dts$Experience), length.out=100)
y_grid_sal3 <- predict(lm_salary_3rd, newdata=data.frame(Experience=x_grid_sal3))

# Plot regression curve
lines(x_grid_sal3, y_grid_sal3, col="red", lwd=2)
grid()

# Diagnostic plots
par(mfrow=c(2,2))
plot(lm_salary_3rd)