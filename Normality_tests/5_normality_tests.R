
# -------------- Activity 1: Hypothesis test --------------


# -------------- Exercise 1: One-sided t test -------------

 

mu <- 24
milles_num <- c(12.591, 54.946, 21.094, 4.274, 25.384, 3.237, 57.278,
                7.695, 3.058, 12.751, 37.698, 62.422, 6.482, 3.788,
                26.689, 34.900, 49.496, 18.093, 13.362, 23.754, 23.769,
                4.956, 6.537, 12.168, 7.079, 35.707, 4.504, 26.836)

n <- length(milles_num)

# Summary Statistics
summary(milles_num)
par(mfrow=c(1,2))
hist(milles_num, main="Distribution of miles driven before reaching $250 in repairs", col="orange",
     xlab = "Miles number (in thousands)")
boxplot(milles_num, main="Boxplot of miles driven before reaching $250 in repairs",
        col="lightblue")

# Check normality
shapiro.test(milles_num) 

# Q-Q Plot
par(mfrow=c(1,1))
qqnorm(milles_num)
qqline(milles_num, col = "red")

## 1. Classical approach

# One-sample t-test
t_statistic_mean <- (mean(milles_num) - mu) / (sd(milles_num) / sqrt(n))
p_value_1 <- pt(t_statistic_mean, df = n-1, lower.tail=TRUE)
p_value_1  # 0.2278826

# Plot t-distribution
x_grid_tdist <- seq(-4, 4, length.out=100)
y_grid_tdist <- dt(x_grid_tdist, df = n - 1)

# Plotting the t-Student distribution

# Adjust margins to create space below the plot
par(mfrow = c(1, 1))
plot(x_grid_tdist, y_grid_tdist, type = "l", 
     main = sprintf("t-Student distribution with %d degrees of freedom", n - 1),
     xlab = "Miles (in thousands) driven before repairs exceed $250", ylab = "Density")

# Add a vertical line to indicate the observed t-statistic
abline(v = t_statistic_mean, col = "red", lty = 2)

# Add label below the dashed line at a fixed y-position
mtext("Observed t-Statistic", side = 1.5, outer = TRUE, line = -5, font = 1, cex = 0.7,
      at = 0.45, col="red")

## 2. Permutation test (Bootstrap approach)

# Permutation Test
set.seed(1594)
B <- 10^5
bootrep <- numeric(B)
set.seed(1915)
for (i in 1:B){
  samp <- sample(milles_num, size=n, replace=TRUE)
  bootrep[i] <- (mean(samp) - mu) / (sd(samp) / sqrt(n))
}
bootrep

# Calculating the p-value:
sum(bootrep >= t_statistic_mean) / B # 0.48355

# Bootstrap histogram
hist(bootrep, main="Bootstrap distribution of the t-Statistic", 
     freq = FALSE, breaks="FD", xlim=c(-6,6), ylim=c(0,0.4),
     xlab = "Miles driven before repairs exceed $250")
abline(v=t_statistic_mean, col="red", lty=2, lwd=2)
lines(x_grid_tdist, y_grid_tdist, lty=2, col="darkorange", lwd=4)
legend("topright", 
       legend=c("Null distribution", "t-Student distribution (df = 27)", 
        "t-Statistic"),
       col=c("gray44", "darkorange", "red"), lwd=2, box.lwd=0.01, cex=0.7, 
       lty=c(NA, 2, 4), pch=c(0, NA, NA))


# -------------- Exercise 2: One-sided t test -------------

# Load necessary library and dataset

## 1.
library(resampledata) 
data("Recidivism")
summary(Recidivism)

# Create a dataframe with the needed variables
recidivism_df <- data.frame(
  Age25 = Recidivism$Age25,
  Recid = Recidivism$Recid
)

# Remove NA values
recidivism_df <- na.omit(recidivism_df) 

# Total number of individuals (after removing NAs)
n_exconvicts <- nrow(recidivism_df)

# Total number of individuals under 25 and over 25
n_under25 <- length(which(recidivism_df$Age25 == "Under 25")) # 3077
n_over25 <- length(which(recidivism_df$Age25 == "Over 25")) # 13942

# Number of reoffenders under 25 and over 25
n_recid_under25 <- length(recidivism_df[recidivism_df$Age25 == "Under 25" & recidivism_df$Recid == "Yes", 2])
# 1123

n_recid_over25 <- length(recidivism_df[recidivism_df$Age25 == "Over 25" & recidivism_df$Recid == "Yes", 2])
# 4263

# Proportion of reoffenders under 25 and over 25
prop_recid_under25 <- n_recid_under25 / n_under25 # 0.3649659
prop_recid_over25 <- n_recid_over25 / n_over25 # 0.3057667

# Print results
cat("Proportion of reoffenders under 25:", prop_recid_under25, "\n")
cat("Proportion of reoffenders over 25:", prop_recid_over25, "\n")

# Difference in proportions
diff_proportions <- prop_recid_under25 - prop_recid_over25 # 0.05919913
cat("Difference in proportions:", diff_proportions, "\n")

# We can estimate p by pooling the data
p0_hat <- (n_recid_under25 + n_recid_over25) / (n_under25 + n_over25)

# Constructing a Z variable
z_proportions <- diff_proportions / sqrt((1/n_under25 + 1/n_over25) * p0_hat * (1 - p0_hat))
cat("Z-score:", z_proportions, "\n")

# For concluding, we're looking for the p-value
p_value <- 2 * pnorm(abs(z_proportions), lower.tail = FALSE)  # Two-tailed test
cat("P-value:", p_value, "\n")

x_grid_norm <- seq(-4, 4, length.out = 100)
y_grid_norm <- dnorm(x_grid_norm)

plot(x_grid_norm, y_grid_norm, type='l', main="Standard normal distribution ", 
     ylab="Density", xlab="Difference in proportions of recidivism among ex-convicts under and over 25")
abline(v=diff_proportions, col="red", lty=2)

legend("topright", 
       legend=c("Difference of proportions"), 
       col=c("red"), cex=0.8,
       lty=2, 
       bty="n")  # Deletes the border

## 2. Permutation test
set.seed(1408)

permutations_num <- 10^4
perm_diffs <- numeric(permutations_num)
               
for (i in 1:permutations_num) {
  shuffled_recid <- na.omit(sample(recidivism_df$Recid, size=length(recidivism_df$Age25), replace=TRUE))  # Shuffle recidivism labels
  
  recidivism_perm_df <- data.frame( # Create the dataframe using the shuffled values
    Age25 = recidivism_df$Age25,
    Recid = shuffled_recid # Adds the shuffled values
  )
  
  # Compute new proportions
  
  # Number of people under 25 and over 25
  n_under25_perm <- length(shuffled_recid[recidivism_perm_df$Age25 == "Under 25"] == "Yes")
  n_over25_perm <- length(shuffled_recid[recidivism_perm_df$Age25 == "Over 25"] == "Yes")
  
  # Number of reoffenders under 25 and over 25
  n_recid_under25_perm <- length(recidivism_df[recidivism_perm_df$Age25 == "Under 25" & recidivism_perm_df$Recid == "Yes", 2])
  n_recid_over25_perm <- length(recidivism_df[recidivism_df$Age25 == "Over 25" & recidivism_perm_df$Recid == "Yes", 2])
  
  # 
  prop_under25_perm <- n_recid_under25_perm / n_under25_perm
  prop_over25_perm <- n_recid_over25_perm / n_over25_perm
  
  perm_diffs[i] <- prop_under25_perm - prop_over25_perm
}

# Compute p-value (proportion of times permuted difference ≥ observed difference)
p_value_perm <- mean(abs(perm_diffs) >= abs(diff_proportions))
cat("Permutation test p-value:", p_value_perm, "\n")

# Visualizing the null distribution
par(mfrow = c(1,1))
hist(perm_diffs, col = "lightblue", main = "Null distribution of proportions difference",
     xlab = "Difference in proportions", xlim = c(-0.06, 0.06))
abline(v = diff_proportions, col = "red", lwd = 2, lty = 2)  # Observed difference
legend("topleft", 
       legend=c("Difference of proportions"), 
       col=c("red"), cex=0.8,
       lty=2, 
       bty="n")  # Deletes the border 

# Compute the p-value
sum(perm_diffs >= diff_proportions) + 1/ permutations_num + 1


# -------------- Exercise 3: Two-sided t test -------------

## 1. Classical Approach
# 1. Preliminaries
# Null Hypothesis (H₀): muVP - muHW = 0
# Alternative Hypothesis (H₁): muVP - muHW != 0

# Focus on the "ValuePRO" and "Homeworth" variables
ValuePro <- c(135, 110, 131, 142, 105, 130, 131, 110, 125, 149)
HomeWorth <- c(128, 105, 119, 140, 98, 123, 127, 115, 122, 145)

# Descriptive statistics:
summary(ValuePro)
summary(HomeWorth)

# Visualizing the distributions:
par(mfrow = c(1, 2))
hist(ValuePro)
hist(HomeWorth)

# The distribution of value pro distribution 
# seems to be not known. 
# On the other hand, the Homeworth distribution seems to be skewed to the left 
# compared to the normal"normal."

# Observed difference in means:
diff <-ValuePro - HomeWorth
diff3 <- mean(ValuePro) - mean(HomeWorth)  # approx 4.6 thousands dollars

# Is this 4.6 difference statistically significant?


# Descriptive statistics for differences
mean_diff <- mean(diff)  # Mean of differences
sd_diff <- sd(diff)      # Standard deviation of differences
n <- length(ValuePro)        # Number of pairs

# Observed test statistic for paired t-test
t_obs <- mean_diff / (sd_diff / sqrt(n))
t_obs  # 3.304

# Degrees of freedom
df <- n - 1
df
# Calculate the p-value (two-tailed test)
p_value <- 2 * pt(abs(t_obs), df = df, lower.tail = FALSE)
p_value  # 0.009 aprox

##More Descriptive Analysis: QQ Plot ###
qqnorm(ValuePro)
qqline(ValuePro, col = "red", lty = 2)
qqnorm(HomeWorth)
qqline(HomeWorth, col = "red", lty = 2)

# Normality tests:
shapiro.test(ValuePro)
shapiro.test(HomeWorth)
##Here both passes the test

## 2. Permutation test

# 1. Preliminaries
# Null Hypothesis (H₀): muVP - muHW = 0
# Alternative Hypothesis (H₁): muVP - muHW != 0

# Upload data
ValuePro <- c(135, 110, 131, 142, 105, 130, 131, 110, 125, 149)
HomeWorth <- c(128, 105, 119, 140, 98, 123, 127, 115, 122, 145)

observed <- mean(ValuePro) - mean(HomeWorth)
observed
# How likely is it to observe a difference of 4.6
# or more, assuming the evaluation of both properties is the same
# for ValuePro and Homeworth?

diff <- ValuePro - HomeWorth
C <- 10^5
results <- numeric(C)
set.seed(1742)
for (i in 1:C) {
  rowsign <- sample(c(-1, 1), size = 10, replace = TRUE)
  diff2 <- rowsign * diff
  results[i] <- mean(diff2)
}

# 3. Visualizing the permutation distribution
hist(results, breaks = 30, main = "Permutation Distribution", xlab = "Difference in Means", col = "lightblue", border = "black")
abline(v = c(-observed, observed), col = "red", lty = 2, lwd = 2)
rug(results[abs(results) >= abs(observed)], col = "brown4", lwd = 2)

# 4. Calculating the p-value
##Also, being conservative we add +1
p_value <- (sum(abs(results) >= abs(observed)) + 1) / (C + 1)
p_value




ht_x <- c(3.4, 5.3, 4.4, 3.8, 4.5, 6.3, 7.1, 4.6, 4.5)
mean(ht_x)
var(ht_x)
sd(ht_x)
# Datos
data <- c(3.4, 5.3, 4.4, 3.8, 4.5, 6.3, 7.1, 4.6, 4.5)

# Prueba t de dos colas (H0: mu = 4)
t.test(data, mu = 4, alternative = "two.sided", conf.level = 0.90)
?qt
pt(q= 2.2359, df= 8, lower.tail= TRUE()
1 - pt(2.5359, df = 8)
qt(0.10, 8, lower.tail=TRUE)

x_values_grid <- seq(-10, 10, length.out=100)
y_values_grid <- dt(x_values_grid, df=8)
plot(x = x_values_grid, y = y_values_grid, type="l")
val <- qt(0.10/2, 8, lower.tail=FALSE)
abline(v=c(1.3968), col="red")
abline(v=2.2359, col="blue")




2*qnorm(0.01/2)



x_values <- c(366.2, 203.1, 285.3,  79.0, 418.0,
              315.2, 245.8, 268.8, 252.8, 102.8,
              276.6, 431.1, 226.2, 392.4, 169.0,
              399.8, 322.7, 344.6, 191.3, 518.5,
              296.4, 248.2, 364.7, 247.9, 137.7)

mean(x_values)
var(x_values)
sd(x_values)
?mode

hist(x_values)
