
# -------------- Simple Linear regression --------------


# Load the package into the session
library(gamair)

# Load package to support latex characters
library("latex2exp")

data("hubble") # Load the data
hubble # View the dataset
??hubble
# Save important columns
distance = hubble$x
velocity = hubble$y


# 1) Plotting the scatterplot
plot(x = distance, velocity, main = "Distance vs Velocity of 24 galaxies 
    (obtained from the Hubble Space Telescope)",xlab = "Distance (Mpc)", 
     ylab = "Velocity (Km/s)",pch = 19, col = "blue")
grid(col="gray44")

cor_galaxies <- cor(x= velocity, y = distance) # 0.8631815

# It seems to exist a high positive (>0.8) correlation between predictor variable
# X (distance) and response variable Y (velocity)


# 1.2) Linear regression model
n <- length(distance) # number of observations

distance_mean <- mean(distance) # Distance mean (12.05458)
velocity_mean <- mean(velocity) # Velocity mean (924.375)

summary(distance)
summary(velocity)

SXX <- sum((distance - distance_mean)^2) # 777.6332
SXY <- sum((distance - distance_mean) * (velocity - velocity_mean)) # 59198.85

slope <- SXY / SXX # 76.12696
intercept <- velocity_mean - (slope * distance_mean) # 6.696255
# Therefore, the regression model is Y = (76.12696)x + 6.696255


# Sum of Standard Errors (SSE) and  Residual Mean Square (RMS) or (MSE) (this is sigma)
velocity_hat <- (slope * distance) + intercept
SSE <- sum((velocity - velocity_hat)^2)
MSE <- SSE / (n - 2)
# If it has the square root in it, it is called Standard Error of Regression (SER)



# Standard error for the slope and intercept
SE_slope <- sqrt(MSE / SXX)
SE_intercept <- sqrt(MSE * (1/n + distance_mean^2/SXX))


# Total Sum of Squares
SST <- sum((velocity - velocity_mean)^2)
R2 <- 1 - (SSE/SST) # 0.7450822


# t values
tvalue_slope <- slope / SE_slope # 8.018874
tvalue_intercept <- intercept / SE_intercept # 0.05291096


# p-values

# Let intercept = β0 and slope = β1, then

# H0: β0 = 0
# H1: β0 != 0
pvalue_intercept <- 2 * pt(tvalue_intercept, df = n - 2, lower.tail = FALSE)
# The p-value is 0.958, which is HUGE, therefore, we do not have enough evidence
# to reject the null hypothesis. Thus, we cannot reject the posibility that the
# model passes through the origin.

# H0: β1 = 0
# H1: β1 != 0
pvalue_slope <- 2 * pt(tvalue_slope, df = n - 2, lower.tail = FALSE)
# The p-value is 5.676651e-08, so there exists enough evidence to reject the 
# null hypothesis, which indicates, the slope is different from zero.


# F-ratio
F_ratio <- R2 / ((1 - R2) / (n - 2)) # 0.1328561


# Plot the model 
par(mfrow=c(1,1))
plot(x=distance, y=velocity, main="Linear Regression Model",
     xlab="Distance (Mpc)", ylab="Velocity (Km/s)",pch = 19, col = "blue")

subtitle <- TeX("$\\widehat{y} = 76.12696 x + 6.69626$")
mtext(subtitle, side = 3.2, outer = TRUE, line = -4, font = 1, cex = 0.9)
abline(a=intercept, b=slope, col="red", lty=2, lwd=2)
grid(col="gray44")
# abline(a=6.696, b=76.127, col="red", lty=2, lwd=2) Borrar


lm_galaxies <- lm(velocity ~ distance)
# Velocity is being modeled as a function of distance, which means that 
# velocity (y) is assumed to depend on distance (x)

summary(lm_galaxies)
par(mfrow=c(1,1))
hist(velocity)
par(mfrow=c(2,2))
plot(lm_galaxies)


# 1.3) 95% confidence intervals for the slope and the intercept
confidence_level <- 0.95
significance_level <- 1 - confidence_level

critical_value <- qt(significance_level/2, n - 2, lower.tail = FALSE)
critical_value # 2.073873

# Compute confidence interval for the slope
low_bound_slope <- slope - (critical_value * SE_slope)
up_bound_slope <- slope + (critical_value * SE_slope)
# [56.43972, 95.81428]

# Compute confidence interval for the intercept
low_bound_inter <- intercept - (critical_value * SE_intercept)
up_bound_inter <- intercept + (critical_value * SE_intercept)
# [-255.767, 269.1592]

round(262.4632,2)
confint(lm_galaxies, level = 0.95)


# 1.4) Confidence and prediction bands

# The critical value in the t distribution with n-2 dof is 
conf_level_90 <- 0.90
sig_level_10 <- 1 - conf_level_90

critical_value_10 <- qt(sig_level_10/2, n-2, lower.tail = FALSE) # 1.75305

# Values in x
x_values <- c(5.5, 14.0, 20.8)

# Calculate the standard errors
se_band_mean <- sqrt(MSE) * sqrt((1/n) + (x_values - distance_mean)^2 / SXX)
se_band_pred <- sqrt(MSE) * sqrt(1 + (1/n) + (x_values - distance_mean)^2 / SXX)

# Calculate predicted velocity estimates (y_hat)
y_hat_values <- (slope * x_values) + intercept

# Calculate the confidence bands for the mean
low_band_mean <- y_hat_values - critical_value_10 * se_band_mean
up_band_mean <- y_hat_values + critical_value_10 * se_band_mean

# Calculate the prediction bands (for a single future observation)
low_band_pred <- y_hat_values - critical_value_10 * se_band_pred
up_band_pred <- y_hat_values + critical_value_10 * se_band_pred

# ** Plot the confidence and prediction bands **
par(mfrow=c(1,1))

# Define a wider range of x values for smoother and larger bands
x_values_extended <- seq(min(distance) - 5, max(distance) + 5, length.out = 100)
y_hat_values_origin <- (slope * x_values)

# Recalculate standard errors for the extended range
se_band_mean_extended <- sqrt(MSE) * sqrt((1/n) + (x_values_extended - distance_mean)^2 / SXX)
se_band_pred_extended <- sqrt(MSE) * sqrt(1 + (1/n) + (x_values_extended - distance_mean)^2 / SXX)

# Compute new confidence and prediction bands
y_hat_extended <- (slope * x_values_extended) + intercept
low_band_mean_extended <- y_hat_extended - critical_value_10 * se_band_mean_extended
up_band_mean_extended <- y_hat_extended + critical_value_10 * se_band_mean_extended
low_band_pred_extended <- y_hat_extended - critical_value_10 * se_band_pred_extended
up_band_pred_extended <- y_hat_extended + critical_value_10 * se_band_pred_extended

# Plot with the extended x-range
plot(distance, velocity, main = "Confidence and prediction bands for velocity",
     xlab = "Distance (Mpc)", ylab = "Velocity (Km/s)", xlim = range(x_values_extended))
abline(a = 0, b = slope, col = "red", lty = 2, lwd = 2)

# Add extended confidence and prediction bands
lines(x_values_extended, low_band_mean_extended, col = "green4", lty = 2, lwd = 2)
lines(x_values_extended, up_band_mean_extended, col = "green4", lty = 2, lwd = 2)
lines(x_values_extended, low_band_pred_extended, col = "blue", lty = 2, lwd = 2)
lines(x_values_extended, up_band_pred_extended, col = "blue", lty = 2, lwd = 2)

# Add original specific points
points(x = x_values, y = y_hat_values_origin, col="darkred", pch=16, type="p")

# Legend
legend("topleft", 
       legend=c("Confidence bands for the mean", "Prediction bands", "Specific points"),
       col=c("green4", "blue", "darkred"), lwd=2, box.lwd=0, cex=0.8, 
       lty=c(2, 2, NA), pch=c(NA, NA, 16))

# 1.2.6) Hypothesis test for F ratio

# H0: Naive model (without predictor)
# HA: Full model (including the predictor term)

pval_fratio <- pf(q = F_ratio, 1, n-2, lower.tail= FALSE)
# The p-value is extremely small! That indicates HUGE evidence to reject H0

# 1.5) ANOVA test

# SST <- sum((velocity - velocity_mean)^2)
# SSE <- sum((velocity - velocity_hat)^2)
SSR <- sum((velocity_hat - velocity_mean)^2) # Or SSR = SST - SSE
SSR # 4506628

dof_regression <- 1 # There is only one variable to predict
dof_error <- n - 2
dof_total <- n - 1 # Total degrees of freedom

MSE # Previously computed as SSE/dof_error - 2
MSR <- SSR / dof_regression # Mean square due to regression

F_ratio # Previously computed, it can also be calculated as MSR/MSE
pval_fratio

# Create the dataframe 
anova_table <- data.frame(
  Source = c("Regression", "Residuals", "Total"),
  DoF = c(dof_regression, dof_error, dof_total),
  Sum_square = c(SSR, SSE, SST),
  Mean_square = c(MSR, MSE, NA),
  F_ratio = c(F_ratio, NA, NA),
  p_value = c(pval_fratio, NA, NA)
)

print(anova_table)

summary(aov(velocity ~ distance))
anova(lm_galaxies)

# 1.6) Diagnostics plots to evaluate the validity of the model
par(mfrow=c(2,2))
plot(lm_galaxies)



# -------------- Linear regression through the origin --------------

# Fit regression model passing through the origin (it has no intercept)
model_origin <- lm(velocity ~ 0 + distance)  # The 0 in the formula ensures no intercept
summary(model_origin)

# Get estimated slope (beta_1) and standard error
beta1_hat <- slope #coef(model_origin)[1]
se_beta1 <- SE_slope #summary(model_origin)$coefficients[1, 2]


# Calculate SSE and MSE
velocity_hat_origin <- (beta1_hat * distance)
SSE_origin <- sum((velocity - velocity_hat_origin)^2)
MSE_origin <- SSE_origin / (n - 1)  # Degrees of freedom is n - 1 since we're only estimating beta1 (67084.59)

# Calculate standard error for beta1
SE_beta1 <- sqrt(MSE_origin / sum((distance - mean(distance))^2))

# Hypothesis test for beta1 != 0
t_value_beta1 <- beta1_hat / SE_beta1 #8.198575
p_value_beta1 <- 2 * pt(abs(t_value_beta1), df = n - 1, lower.tail = FALSE) #2.809103e-08


# 95% Confidence Interval for beta1
critical_value_95 <- qt(0.05/2, df = n - 2, lower.tail = FALSE)
lower_bound_beta1 <- beta1_hat - critical_value_95 * SE_beta1 # 56.91867
upper_bound_beta1 <- beta1_hat + critical_value_95 * SE_beta1 # 95.33525

# Results
cat("Estimated beta1:", beta1_hat, "\n")
cat("Standard Error for beta1:", SE_beta1, "\n")
cat("t-value for beta1:", t_value_beta1, "\n")
cat("p-value for beta1:", p_value_beta1, "\n")
cat("95% Confidence Interval for beta1: [", lower_bound_beta1, ", ", upper_bound_beta1, "]\n")

# Plot the scatter plot and regression line (no intercept)
par(mfrow=c(1,1))
plot(distance, velocity, main = "Distance vs velocity of galaxies with SLR",
     xlab = "Distance (Mpc)", ylab = "Velocity (Km/s)", pch = 19, col = "green4",
     ylim=c(0,max(velocity) + 2), xlim=c(0,25))
abline(a=0,b=slope, col = "red", lwd = 2, lty = 2)  # Add regression line
grid(col="gray44")

# Plot the model 
par(mfrow=c(1,1))
plot(x=distance, y=velocity, main="Linear Regression Model",
     xlab="Distance (Mpc)", ylab="Velocity (Km/s)",pch = 19, col = "green4",
     ylim=c(0,max(velocity) + 2), xlim=c(0,25))

subtitle <- TeX("$\\widehat{y} = 76.12696 x$")
mtext(subtitle, side = 3.2, outer = TRUE, line = -4, font = 1, cex = 0.9)
abline(a=0, b=slope, col="red", lty=2, lwd=2)
grid(col="gray44")


# Check residuals and fit diagnostics
par(mfrow = c(2, 2))
plot(model_origin)


