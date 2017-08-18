###Chapter 2 Performance Measures

###The Confusion Matrix
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Have a look at the structure of titanic
str(titanic)

# A decision tree classification model is built on the data
tree <- rpart(Survived ~ ., data = titanic, method = "class")

# Use the predict() method to make predictions, assign to pred
pred <- predict(tree, titanic, type = "class")

# Use the table() method to make the confusion matrix
table(titanic$Survived, pred)

###Deriving Ratios from the Confusion Matrix
# The confusion matrix is available in Data Camp's workspace as conf

# Assign TP, FN, FP and TN using conf
TP <- conf[1,1]
FN <- conf[1,2]
FP <- conf[2,1]
TN <- conf[2,2]

# Calculate the accuracy: acc
acc <- (TP + TN) / (TP + FN + FP + TN)
acc

# Calculate and print out the precision: prec
prec <- TP / (TP + FP)
prec

# Calculate and print out the recall: rec
rec <- TP / (TP + FN)
rec

###The Quality of a Regression
# The air dataset is already loaded into your workspace

# Take a look at the structure of air
str(air)

# Inspect your colleague's code to build the model
fit <- lm(dec ~ freq + angle + ch_length, data = air)

# Use the model to predict for all values: pred
pred <- predict(fit)

# Use air$dec and pred to calculate the RMSE 
rmse <- sqrt(sum( (air$dec - pred) ^ 2) / nrow(air))

# Print out rmse
rmse

###Adding Complexity To Increase Quality
# The air dataset is already loaded into your workspace

# Previous model
fit <- lm(dec ~ freq + angle + ch_length, data = air)
pred <- predict(fit)
rmse <- sqrt(sum( (air$dec - pred) ^ 2) / nrow(air))
rmse

# Your colleague's more complex model
fit2 <- lm(dec ~ freq + angle + ch_length + velocity + thickness, data = air)

# Use the model to predict for all values: pred2
pred2 <- predict(fit2)

# Calculate rmse2
rmse2 <- sqrt(sum( (air$dec - pred2) ^ 2) / nrow(air))

# Print out rmse2
rmse2

###Let's Do Some Clustering!
# The seeds dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Explore the structure of the dataset
str(seeds)

# Group the seeds in three clusters
km_seeds <- kmeans(seeds, 3)

# Color the points in the plot based on the clusters
plot(length ~ compactness, data = seeds, col = km_seeds$cluster)

# Print out the ratio of the WSS to the BSS
km_seeds$tot.withinss / km_seeds$betweenss

###Split the Sets
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled 
n <- nrow(titanic)
shuffled <- titanic[sample(n),]

# Split the data in train and test
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Print the structure of train and test
str(train)
str(test)

###First You Train, Then You Test
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset; build train and test
n <- nrow(titanic)
shuffled <- titanic[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Change the model that has been learned. One of the arguments is incorrect.
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the outcome on the test set with tree: pred
pred <- predict(tree, test, type = "class")

# Calculate the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print this confusion matrix
print(conf)

###Using Cross Validation
# The shuffled dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# The shuffled dataset is already loaded into your workspace

# Initialize the accs vector
accs <- rep(0,6)

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Survived ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree
  pred <- predict(tree, test, type = "class")
  
  # Assign the confusion matrix to conf
  conf <- table(test$Survived, pred)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs
mean(accs)

###Overfitting the Spam!
# The spam filter that has been 'learned' for you
spam_classifier <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 4] <- 1
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(factor(prediction, levels=c("1","0")))
}

# Apply spam_classifier to emails_full: pred_full
pred_full <- spam_classifier(emails_full$avg_capital_seq)

# Build confusion matrix for classifier on emails_full: conf_full
conf_full <- table(emails_full$spam, pred_full)

# Calculate the accuracy with conf_full: acc_full
acc_full <- sum(diag(conf_full)) / sum(conf_full)

# Print out acc_full
acc_full

###Increasing the Bias
# The all-knowing classifier that has been learned for you
# You should change the code of the classifier, simplifying it
spam_classifier <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 4] <- 1
  prediction[x <= 4] <- 0
  return(factor(prediction, levels=c("1","0")))
}

# conf_small and acc_small have been calculated for you
conf_small <- table(emails_small$spam, spam_classifier(emails_small$avg_capital_seq))
acc_small <- sum(diag(conf_small)) / sum(conf_small)
acc_small

# Apply spam_classifier to emails_full and calculate the confusion matrix: conf_full
conf_full <- table(emails_full$spam, spam_classifier(emails_full$avg_capital_seq))

# Calculate acc_full
acc_full <- sum(diag(conf_full)) / sum(conf_full)

# Print acc_full
acc_full

