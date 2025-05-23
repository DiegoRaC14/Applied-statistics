
# -------------- Dummy variables --------------


library("bootstrap") # Load 'bootstrap' library
hormone # Visualize dataset

nrow(hormone) # Number of rows in an array, in this case: 27

# Plot the dataset
par(mfrow=c(1,1))
plot(x=hormone$hrs, y=hormone$amount, main="Scatterplot: Mg vs hrs", 
     xlab="Hours of wear", ylab="Mg of anti-inflammatory hormone", col="darkgreen", pch=16)
grid()

# Create the dummy variables
batch_B <- ifelse(hormone$Lot == 'B', 1, 0) # In case 'Batch' is equal to B, we add a 1, otherwise 0
batch_B

batch_C <- ifelse(hormone$Lot == 'C', 1, 0) # In case 'Batch' is equal to A, we add a 1, otherwise 0
batch_C
?ifelse


# Create the dataframe
hormone_df <- data.frame(
  Hours = hormone$hrs, # Continuous variable
  Batch_B = batch_B, # Dummy variables
  Batch_C = batch_C,
  Amount = hormone$amount # Response variable
)
hormone_df
sample(x =hormone_df, size=10) # Show dataframe
?sample

# hormone_df[sample(nrow(hormone_df), size = 10), ] # Take a random sample of the dataframe

## Create model 1 with independent variable 'hours'
model1_hormone <- lm(Amount ~ Hours, data=hormone_df)
summary(model1_hormone)

par(mfrow=c(2,2)) # Create a grid
plot(model1_hormone) # Diagnostic plots

# Extract the coefficients
model1_b0 <- summary(model1_hormone)$coefficients["(Intercept)", "Estimate"]
model1_b1 <- summary(model1_hormone)$coefficients["Hours", "Estimate"]

# Plot the model
par(mfrow=c(1,1))
plot(x=hormone$hrs, y=hormone$amount, main="Linear regression model (without dummies)", 
  xlab="Hours of wear", ylab="Mg of anti-inflammatory hormone", col="blue", pch=16)
abline(model1_hormone, col="red", lwd=2)
grid() # Add grid


## Create model 2 with dummy variables
model2_hormone <- lm(Amount ~ Hours + Batch_B + Batch_C, data = hormone_df)
summary(model2_hormone)

# Diagnostic plots of model 2
par(mfrow=c(2,2))
plot(model2_hormone)

# Plot model 2

# Extract the coefficients
model2_b0 <- summary(model2_hormone)$coefficients["(Intercept)", "Estimate"]
model2_b1 <- summary(model2_hormone)$coefficients["Hours", "Estimate"]
model2_batchB_b <- summary(model2_hormone)$coefficients["Batch_B", "Estimate"]
model2_batchC_b <- summary(model2_hormone)$coefficients["Batch_C", "Estimate"]
model2_batchC_b # Show the batch

# Create a color vector for batches
colors <- ifelse(hormone$Lot == "A", "darkgreen",
                 ifelse(hormone$Lot == "B", "darkorange", "red"))

# Create a symbol vectors to distinguish shapes
symbols <- ifelse(hormone$Lot == "A", 16,
                  ifelse(hormone$Lot == "B", 17, 15))
symbols


# Plot the data colored by batch
plot(x=hormone$hrs, y=hormone$amount, main="Regression lines and points by batch",
     xlab="Hours of wear", ylab="Mg of anti-inflammatory hormone",
     col=colors, pch=symbols)

# Add regression lines
abline(a=model2_b0, b=model2_b1, col="darkgreen", lwd=2) # Batch A
abline(a=model2_b0 + model2_batchB_b, b=model2_b1, col="darkorange", lwd=2) # Batch B
abline(a=model2_b0 + model2_batchC_b, b=model2_b1, col="red", lwd=2) # Batch C
#abline(model1_hormone, col="blue", lwd=2)
grid()

# Add legend
legend("topright",
       legend=c("Batch A", "Batch B", "Batch C"),
       col=c("darkgreen", "darkorange", "red"),
       pch=c(16, 17, 15),
       lty=c(2, 2, 2), # Line type
       lwd=2)



## Compare both models
# Plot the data colored by batch
plot(x=hormone$hrs, y=hormone$amount, main="Model 1 (No dummies) vs Model 2 (With dummies)",
     xlab="Hours of wear", ylab="Mg of anti-inflammatory hormone",
     col=colors, pch=symbols)

# Add regression lines
abline(a=model2_b0, b=model2_b1, col="darkgreen", lwd=2) # Batch A
abline(a=model2_b0 + model2_batchB_b, b=model2_b1, col="darkorange", lwd=2) # Batch B
abline(a=model2_b0 + model2_batchC_b, b=model2_b1, col="red", lwd=2) # Batch C
abline(model1_hormone, col="blue", lwd=3)
grid()

# Add legend
legend("topright",
       legend=c("No dummies", "Batch A", "Batch B", "Batch C"),
       col=c("blue", "darkgreen", "darkorange", "red"),
       pch=c(NA, 16, 17, 15),
       lty=c(1, 1, 1, 1), # Line type
       lwd=2,
       cex=0.8)



## LOOCV (Leave-One-Out Cross-Validation)

loocv <- function(df, col_resp, cols_independent){
  n <- nrow(df) # Number of rows in the dataframe
  squared_error <- numeric(n) # Numeric array
  
  
  cols_names <- colnames(df) # Get the columns names and save them in an array
  y_name <- cols_names[col_resp] # Response variable
  x_names <- cols_names[c(cols_independent)] 
  
  formula_text <- paste(y_name, "~", paste(x_names, collapse = "+")) # Create the formula
  formula <- as.formula(formula_text)
  
  
  for (i in 1:n){
    df_cv <- df[-i, ] # Delete the i-th row
    row <- df[i, , drop=FALSE] # Stores the deleted row
    
    model <- lm(formula, data=df_cv) # Make the model
    
    y_hat <- predict(model, newdata= row) # Predicts the value
    y_real <- df[i, y_name] # Obtain the real y value
    
    # Compute the squared error (yi - yi_hat)^2
    squared_error[i] <- (y_real - y_hat)^2
    
  }
  
  return(mean(squared_error))
}

# Call functions for both CV
cv1 <- loocv(hormone_df, col_resp=4, cols_independent=1)
cv2 <- loocv(hormone_df, col_resp=4, cols_independent=c(1,2,3))

cat("CV1 (Model 1):", cv1, "\n") # 6.027698
cat("CV2 (Model 2):", cv2, "\n") # 3.092669
# Ratio of both Cross-Validation
cat("CV1/CV2:", cv1/cv2) # 1.949028


## Create a third model
batch_A <- ifelse(hormone$Lot == "A", 1, 0) # Create a dummy variable where we just consider batch A

hormone_batchA_df <- data.frame(
  Hours = hormone$hrs, # Continuous variable, number of hours wearing the hormones
  Batch_A = batch_A,
  Amount = hormone$amount # Response variable, amount in mgr of anti-inflammatory hormones
)

hormone_batchA_df

## Create model 3 with independent variable 'hours'
model3_hormone <- lm(Amount ~ Hours + Batch_A, data=hormone_batchA_df)
summary(model3_hormone)

par(mfrow=c(2,2)) # Create a grid
plot(model1_hormone) # Diagnostic plots

# Plot the model
par(mfrow=c(1,1))
plot(x=hormone$hrs, y=hormone$amount, main="Linear regression model (with batch A)", 
     xlab="Hours of wear", ylab="Mg of anti-inflammatory hormone", col="blue", pch=16)
abline(model3_hormone, col="red", lwd=2)
grid() # Add grid

cv3 <- loocv(hormone_batchA_df, col_resp=3, cols_independent=c(1,2))

hormone_batchA_df

cat("CV3 (Model 3):", cv3, "\n") # 2.814787

# Ratio of both Cross-Validation
cat("CV3/CV1:", cv3/cv1, "\n") # 0.4669754  
cat("CV3/CV2:", cv3/cv2, "\n") # 0.9101482  


