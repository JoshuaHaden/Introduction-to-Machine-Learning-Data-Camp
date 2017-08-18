###Chapter 4 Regression

###Simple Linear Regression: Your First Step!
# The kang_nose dataset and nose_width_new are already loaded in your workspace.

# Plot nose length as function of nose width.
plot(kang_nose, xlab = "nose width", ylab = "nose length")

# Describe the linear relationship between the two variables: lm_kang
lm_kang <- lm(nose_length ~ nose_width, data = kang_nose)

# Print the coefficients of lm_kang
print(lm_kang$coefficients)

# Predict and print the nose length of the escaped kangoroo
predict(lm_kang, nose_width_new)

###Performance Measure: RMSE
# kang_nose is pre-loaded in your workspace

# Build model and make plot
lm_kang <- lm(nose_length ~ nose_width, data=kang_nose)
plot(kang_nose, xlab = "nose width", ylab = "nose length")
abline(lm_kang$coefficients, col = "red")

# Apply predict() to lm_kang: nose_length_est
nose_length_est <- predict(lm_kang)

# Calculate difference between the predicted and the true values: res
res <- kang_nose$nose_length - nose_length_est

# Calculate RMSE, assign it to rmse and print it
rmse <- sqrt(mean(res ^ 2))
print(rmse)

###Performance Measures: R-squared
# kang_nose, lm_kang and res are already loaded in your workspace

# Calculate the residual sum of squares: ss_res
ss_res <- sum(res ^ 2)

# Determine the total sum of squares: ss_tot
ss_tot <- sum((kang_nose$nose_length - mean(kang_nose$nose_length)) ^ 2)

# Calculate R-squared and assign it to r_sq. Also print it.
r_sq <- 1 - ss_res / ss_tot
r_sq

# Apply summary() to lm_kang
summary(lm_kang)

###Another Take at Regression: Be Critical
# world_bank_train and cgdp_afg is available for you to work with

# Plot urb_pop as function of cgdp
plot(world_bank_train, 
     xlab = "GDP per Capita", 
     ylab = "Percentage of urban population")

# Set up a linear model between the two variables: lm_wb
lm_wb <- lm(urb_pop ~ cgdp, data = world_bank_train)

# Add a red regression line to your scatter plot
abline(lm_wb$coefficients, col = "red")

# Summarize lm_wb and select R-squared
summary(lm_wb)$r.squared

# Predict the urban population of afghanistan based on cgdp_afg
predict(lm_wb, cgdp_afg)

###Non-Linear, But Still Linear?
# world_bank_train and cgdp_afg is available for you to work with

# Plot: change the formula and xlab
plot(urb_pop ~ log(cgdp), data = world_bank_train, 
     xlab = "log(GDP per Capita)", 
     ylab = "Percentage of urban population")

# Set up a linear model between the two variables: lm_wb
lm_wb <- lm(urb_pop ~ log(cgdp), data = world_bank_train)

# Add a red regression line to your scatter plot
abline(lm_wb$coefficients, col = "red")

# Summarize lm_wb and select R-squared
summary(lm_wb)$r.squared

# Predict the urban population of afghanistan based on cgdp_afg
predict(lm_wb, cgdp_afg)

###Going All-In With Predictors!
# shop_data has been loaded in your workspace

# Add a plot: sales as a function of inventory. Is linearity plausible?
plot(sales ~ sq_ft, shop_data)
plot(sales ~ size_dist, shop_data)
plot(sales ~ inv, shop_data)

# Build a linear model for net sales based on all other variables: lm_shop
lm_shop <- lm(sales ~ ., data = shop_data)

# Summarize lm_shop
summary(lm_shop)

###Are All Predictors Relevant?
# shop_data, shop_new and lm_shop have been loaded in your workspace

# Plot the residuals in function of your fitted observations
plot(lm_shop$fitted.values,  lm_shop$residuals, 
     xlab = "Fitted values", ylab = "Residuals")

# Make a Q-Q plot of your residual quantiles
qqnorm(lm_shop$residuals, ylab = "Residual Quantiles")

# Summarize your model, are there any irrelevant predictors?
summary(lm_shop)

# Predict the net sales based on shop_new.
predict(lm_shop, shop_new)

###Are All Predictors Relevant? Take 2!
# choco_data has been loaded in your workspace

# Add a plot:  energy/100g as function of total size. Linearity plausible?
plot(energy ~ protein, choco_data)
plot(energy ~ fat, choco_data)
plot(energy ~ size, choco_data)

# Build a linear model for the energy based on all other variables: lm_choco
lm_choco <- lm(energy ~ ., data = choco_data)

# Plot the residuals in function of your fitted observations
plot(lm_choco$fitted.values,  lm_choco$residuals, 
     xlab = "Fitted values", ylab = "Residuals")

# Make a Q-Q plot of your residual quantiles
qqnorm(lm_choco$residuals, ylab = "Residual Quantiles")

# Summarize lm_choco
summary(lm_choco)

###Does Your Model Generalize?
# world_bank_train, world_bank_test and lm_wb_log are pre-loaded

# Build the log-linear model
lm_wb_log <- lm(urb_pop ~ log(cgdp), data = world_bank_train)

# Calculate rmse_train
rmse_train <- sqrt(mean(lm_wb_log$residuals ^ 2))

# The real percentage of urban population in the test set, the ground truth
world_bank_test_truth <- world_bank_test$urb_pop

# The predictions of the percentage of urban population in the test set
world_bank_test_input <- data.frame(cgdp = world_bank_test$cgdp)
world_bank_test_output <- predict(lm_wb_log, world_bank_test_input)

# The residuals: the difference between the ground truth and the predictions
res_test <- world_bank_test_output - world_bank_test_truth


# Use res_test to calculate rmse_test
rmse_test <- sqrt(mean(res_test ^ 2))

# Print the ratio of the test RMSE over the training RMSE
rmse_test / rmse_train

###Your Own k-NN Algorithm!
###
# You don't have to change this!
# The algorithm is already coded for you; 
# inspect it and try to understand how it works!
my_knn <- function(x_pred, x, y, k){
  m <- length(x_pred)
  predict_knn <- rep(0, m)
  for (i in 1:m) {
    
    # Calculate the absolute distance between x_pred[i] and x
    dist <- abs(x_pred[i] - x)
    
    # Apply order() to dist, sort_index will contain 
    # the indices of elements in the dist vector, in 
    # ascending order. This means sort_index[1:k] will
    # return the indices of the k-nearest neighbors.
    sort_index <- order(dist)    
    
    # Apply mean() to the responses of the k-nearest neighbors
    predict_knn[i] <- mean(y[sort_index[1:k]])    
    
  }
  return(predict_knn)
}
###

# world_bank_train and world_bank_test are pre-loaded

# Apply your algorithm on the test set: test_output
test_output <- my_knn(
  world_bank_test$cgdp, 
  world_bank_train$cgdp, 
  world_bank_train$urb_pop, 
  30)

# Have a look at the plot of the output
plot(world_bank_train, 
     xlab = "GDP per Capita", 
     ylab = "Percentage Urban Population")
points(world_bank_test$cgdp, test_output, col = "green")

###Parametric vs Non-Parametric
# world_bank_train and world_bank_test are pre-loaded
# lm_wb and lm_wb_log have been trained on world_bank_train
# The my_knn() function is available

# Define ranks to order the predictor variables in the test set
ranks <- order(world_bank_test$cgdp)

# Scatter plot of test set
plot(world_bank_test, 
     xlab = "GDP per Capita", ylab = "Percentage Urban Population")

# Predict with simple linear model and add line
test_output_lm <- predict(lm_wb, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm[ranks], lwd = 2, col = "blue")

# Predict with log-linear model and add line
test_output_lm_log <- predict(lm_wb_log, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm_log[ranks], lwd = 2, col = "red")

# Predict with k-NN and add line
test_output_knn <- my_knn(world_bank_test$cgdp, world_bank_train$cgdp, world_bank_train$urb_pop, 30)
lines(world_bank_test$cgdp[ranks], test_output_knn[ranks], lwd = 2, col = "green")

# Calculate RMSE for simple linear model
sqrt(mean( (test_output_lm - world_bank_test$urb_pop) ^ 2))

# Calculate RMSE for log-linear model
sqrt(mean( (test_output_lm_log - world_bank_test$urb_pop) ^ 2))

# Calculate RMSE for k-NN technique
sqrt(mean( (test_output_knn - world_bank_test$urb_pop) ^ 2))





