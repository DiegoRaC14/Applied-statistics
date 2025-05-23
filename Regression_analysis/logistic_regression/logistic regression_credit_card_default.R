
# -------------- Logistic regression --------------

# Load package into the session
library(ISLR)

# Load library that supports latex characters
library(latex2exp)

data('Default')
head(Default) # View the first 6 rows of the df


### a. Descriptive analysis
head(Default)

# Summary
summary(Default)
# We can see a class imbalance

# Save the arrays
income <- Default$income
balance <- Default$balance
y_default <- Default$default

# Plot 'Income' vs 'Balance'
par(mfrow=c(1,1))

# Plot Income vs Balance
plot(x = income, y = balance,
     xlab = "Income", ylab="Balance", 
     main="Scatterplot: Income vs Balance",
     col = ifelse(y_default == "No", "red", "blue"),
     pch=20)

# Add a legend
legend("topright", 
       legend = c("No Default", "Default"), 
       col = c("red", "blue"), 
       pch = 20)

par(mfrow = c(1, 2))

# Plot where default = No
plot(x = income, y = balance,
     xlab = "Income", ylab = "Balance", 
     main = "Income vs Balance (No default)",
     col = ifelse(Default$default == "No", "red", "lightgray"),
     pch = 20)

# Plot where default = Yes
plot(x = income, y = balance,
     xlab = "Income", ylab = "Balance", 
     main = "Income vs Balance (Default)",
     col = ifelse(Default$default == "Yes", "blue", "lightgray"),
     pch = 20)


# Include boxplots
# Set up the plotting area: 1 row, 2 columns
par(mfrow = c(1, 2))

# Boxplot of Balance grouped by Default
boxplot(balance ~ y_default,
        col = c("red", "blue"),
        main = "Boxplot of Balance by Default",
        xlab = "Default",
        ylab = "Balance",
        names = c("No", "Yes"))

# Boxplot of Income grouped by Default
boxplot(income ~ y_default,
        col = c("red", "blue"),
        main = "Boxplot of Income by Default",
        xlab = "Default",
        ylab = "Income",
        names = c("No", "Yes"))


# Make a conditional plots
par(mfrow = c(1,1))
cdplot(default ~ balance, data = Default, main = "Conditional plot")



### b. Build a logistic regression model
default_glm <- glm(default ~ balance, data=Default, family=binomial())
summary(default_glm)


# Show diagnostic plots
par(mfrow=c(2,2))
plot(default_glm)

# Transform into a binary variable
default_binary <- ifelse(Default$default == "No", 0 , 1)
default_binary 

par(mfrow=c(1,1))
plot(x=balance, y=default_binary,
     xlab="Balance", ylab="Default")

# Plot the regression curve

# Create a sequence of balance values (for a smooth curve)
balance_seq <- seq(min(balance), max(balance), length.out = 500)

# Create a data frame with balance values (for prediction)
default_grid <- data.frame(balance = balance_seq)

# Predict probabilities using the logistic regression model (it needs to be a dataframe)
prob_default <- predict(default_glm, newdata = default_grid, type = "response")

# Plot the actual data points (0 or 1 for default)
plot(x = balance, y = default_binary,
     xlab = "Balance", ylab = "Probability of default (0 = No, 1 = Yes)",
     main = "Fitted logistic curve",
     pch = 20)

# Add the logistic regression curve
lines(balance_seq, prob_default, col = "blue", lwd = 2)

# Create a data frame for prediction
default_values <- data.frame(balance = c(950, 1550, 1990))

# Predict probabilities
predict_default <- predict(default_glm, newdata = default_values, type = "response")
cat("Predicted values: ", predict_default)



### c. Predict default based on the student variable 
model_c <- glm(default ~ student, data = Default, family = binomial())
summary(model_c)



### d. Build model with three variables
model_d <- glm(default ~ balance + student + income, data = Default, family = binomial())
summary(model_d)


### e. Build model based on 'balance' and 'student'
model_e <- glm(default ~ balance + student, data = Default, family = binomial())
summary(model_e)

# Create a sequence of balances
balance_seq <- seq(min(Default$balance), max(Default$balance), length.out = 500)

# Create a grid of values for 'balance' and 'student'
grid_students <- data.frame(
  balance = rep(balance_seq, 2),
  student = rep(c("Yes", "No"), each = length(balance_seq))
)

# Predict probabilities with the model
grid_students$prob <- predict(model_e, newdata = grid_students, type = "response")

# Use ggplot to visualize
ggplot(grid_students, aes(x = balance, y = prob, color = student)) +
  geom_line(size = 1) +
  labs(
    title = "Probability of Default vs Balance by student status",
    x = "Balance",
    y = "Probability of Default"
  ) +
  theme_minimal()


### g. Training and testing the model
set.seed(42002)
sample_indices <- sample(1: nrow(Default), 5000)
sample_indices # Random indices

# Obtain the training and test set
training_set <- Default[sample_indices, ]
test_set <- Default[-sample_indices, c('default', 'student', 'balance')]
head(test_set)

# Train the model
model_train <- glm(default ~ balance + student, data=training_set, family=binomial())
summary(model_train)

# Vector that predicts the variable (it is a float array)
y_pred <- predict(model_train, newdata = test_set, type="response")
head(y_pred) # Those are the predictions

# Evaluate the model
y_pred_default <- ifelse(y_pred > 0.5, "Yes", "No")
head(y_pred_default)

# Sum values that are true (they match in their responses)
acc_model_train <- sum(test_set$default == y_pred_default, na.rm = TRUE)

cat("Accuracy of the model:", acc_model_train/nrow(test_set),
    "(", acc_model_train/nrow(test_set) * 100, "%)") 
# Accuracy of the model: 0.9748 ( 97.48 %)

# g.2
# Try a lower threshold: e.g., 0.3
y_pred_03 <- ifelse(y_pred > 0.3, "Yes", "No")
acc_model_03 <- sum(test_set$default == y_pred_03, na.rm = TRUE) / nrow(test_set)
cat("Accuracy with threshold 0.3:", acc_model_03, "(", acc_model_03 * 100, "%)\n")

# Try a higher threshold: e.g., 0.7
y_pred_07 <- ifelse(y_pred > 0.7, "Yes", "No")
acc_model_07 <- sum(test_set$default == y_pred_07, na.rm = TRUE) / nrow(test_set)
cat("Accuracy with threshold 0.7:", acc_model_07, "(", acc_model_07 * 100, "%)\n")

