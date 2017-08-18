###Chapter 3 Classification

###Learn a Decision Tree
# The train and test set are loaded into your workspace.

# Set random seed. Don't remove this line
set.seed(1)

# Load the rpart, rattle, rpart.plot and RColorBrewer package
library(rpart)
library(RGtk2Extras)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Build a tree model: tree
tree <- rpart(Survived ~ ., train, method = "class")

# Draw the decision tree
fancyRpartPlot(tree)

###Classify With the Decision Tree
# The train and test set are loaded into your workspace.

# Code from previous exercise
set.seed(1)
library(rpart)
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the values of the test set: pred
pred <- predict(tree, test, type = "class")

# Construct the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print out the accuracy
print(sum(diag(conf)) / sum(conf))

###Pruning the Tree
# All packages are pre-loaded, as is the data

# Calculation of a complex tree
set.seed(1)
tree <- rpart(Survived ~ ., train, method = "class", control = rpart.control(cp=0.00001))

# Draw the complex tree
fancyRpartPlot(tree)

# Prune the tree: pruned
pruned <- prune(tree, cp = 0.01)

# Draw pruned
fancyRpartPlot(pruned)

###Splitting Criterion
# All packages, emails, train, and test have been pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Train and test tree with gini criterion
tree_g <- rpart(spam ~ ., train, method = "class")
pred_g <- predict(tree_g, test, type = "class")
conf_g <- table(test$spam, pred_g)
acc_g <- sum(diag(conf_g)) / sum(conf_g)

# Change the first line of code to use information gain as splitting criterion
tree_i <- rpart(spam ~ ., train, method = "class", parms = list(split = "information"))
pred_i <- predict(tree_i, test, type = "class")
conf_i <- table(test$spam, pred_i)
acc_i <- sum(diag(conf_i)) / sum(conf_i)

# Draw a fancy plot of both tree_g and tree_i
fancyRpartPlot(tree_g)
fancyRpartPlot(tree_i)

# Print out acc_g and acc_i
acc_g
acc_i

###Preprocess the Data
# train and test are pre-loaded

# Store the Survived column of train and test in train_labels and test_labels
train_labels <- train$Survived
test_labels <- test$Survived

# Copy train and test to knn_train and knn_test
knn_train <- train
knn_test <- test

# Drop Survived column for knn_train and knn_test
knn_train$Survived <- NULL
knn_test$Survived <- NULL

# normalize Pclass
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

# normalize Age
min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_age) / (max_age - min_age)

###The knn() Function
# knn_train, knn_test, train_labels and test_labels are pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Load the class package
library(class)

# Make predictions using knn: pred
pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 5)

# Construct the confusion matrix: conf
conf <- table(test_labels, pred)

# Print out the confusion matrix
conf

###K's Choice
# knn_train, knn_test, train_labels and test_labels are pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Load the class package, define range and accs
library(class)
range <- 1:round(0.2 * nrow(knn_train))
accs <- rep(0, length(range))

for (k in range) {
  
  # Make predictions using knn: pred
  pred <- knn(knn_train, knn_test, train_labels, k = k)
  
  # Construct the confusion matrix: conf
  conf <- table(test_labels, pred)
  
  # Calculate the accuracy and store it in accs[k]
  accs[k] <- sum(diag(conf)) / sum(conf)
}

# Plot the accuracies. Title of x-axis is "k".
plot(range, accs, xlab = "k")

# Calculate the best k
which.max(accs)

###Creating the ROC Curve (1)
# train and test are pre-loaded

# Set random seed. Don't remove this line
set.seed(1)

# Build a tree on the training set: tree
tree <- rpart(income ~ ., train, method = "class")

# Predict probability values using the model: all_probs
all_probs <- predict(tree, test, type = "prob")

# Print out all_probs
all_probs

# Select second column of all_probs: probs
probs <- all_probs[,2]

###Creating the ROC Curve (2)
# train and test are pre-loaded

# Code of previous exercise
set.seed(1)
tree <- rpart(income ~ ., train, method = "class")
probs <- predict(tree, test, type = "prob")[,2]

# Load the ROCR library
library(ROCR)

# Make a prediction object: pred
pred <- prediction(probs, test$income)

# Make a performance object: perf
perf <- performance(pred, "tpr", "fpr")

# Plot this curve
plot(perf)

###The Area Under the Curve
# test and train are loaded into your workspace

# Build tree and predict probability values for the test set
set.seed(1)
tree <- rpart(income ~ ., train, method = "class")
probs <- predict(tree, test, type = "prob")[,2]

# Load the ROCR library
library(ROCR)

# Make a prediction object: pred
pred <- prediction(probs, test$income)

# Make a performance object: perf
perf <- performance(pred, "auc")

# Print out the AUC
print(perf@y.values[[1]])

###Comparing the Methods
# Load the ROCR library
library(ROCR)

# Make the prediction objects for both models: pred_t, pred_k
pred_t <- prediction(probs_t, test$spam)
pred_k <- prediction(probs_k, test$spam)

# Make the performance objects for both models: perf_t, perf_k
perf_t <- performance(pred_t, "tpr", "fpr")
perf_k <- performance(pred_k, "tpr", "fpr")

# Draw the ROC lines using draw_roc_lines()
draw_roc_lines(tree = perf_t, knn = perf_k)

