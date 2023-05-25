
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
install.packages('caret')
library(caret)
install.packages('e1071', dependencies=TRUE)
library(randomForest)
install.packages("caTools")
library(caTools)
install.packages("randomForest")
library(randomForest)
install.packages("gbm")
library(gbm)


cancer= read.table(file=file.choose(), sep=",", header=TRUE)
head(cancer, n=10)

#Based on kaggle, there were no missing values but lets make sure!
# create new dataset without potential missing data
cancer= na.omit(cancer)

#Eventually I will be doing binomial classification so I'm changing Benign to 0
#and Malignant to 1
#changing diagnosis class from char to numeric
cancer$diagnosis[cancer$diagnosis == 'B'] = 0
cancer$diagnosis[cancer$diagnosis == 'M'] = 1
cancer$diagnosis = as.numeric(cancer$diagnosis)
head(cancer)
class(cancer$diagnosis)



#Feature Importance
cancer=data.frame(cancer)
#seeing if the data is balanced
colSums(cancer==0) #357
colSums(cancer==1) #212
  #data is not balanced so I will need to account for that

regressor <- randomForest(diagnosis ~ . , data = cancer, importance=TRUE) # fit the random forest with default parameter
varImp(regressor, conditional=TRUE) # conditional=True, adjusts for correlations between predictors
imp = as.data.frame(varImp(regressor))
imp = data.frame(overall = imp$Overall,
           names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]
#Top 10 important variables to the predictor:
  #1.concave.points_worst
  #2.area_worst
  #3.radius_worst
  #4.perimeter_worst
  #5.texture_worst
  #6.texture_mean
  #7.concance.points_mean
  #8.area_se
#These will be what I build my model on when prediciting a diagnosis for a patient

#Engineering interaction variables based on important features
cancer$interaction_1 = cancer$concave.points_worst * cancer$area_worst
cancer$interaction_2 = cancer$concave.points_worst * cancer$radius_worst
cancer$interaction_3 = cancer$area_worst * cancer$radius_worst

#Initial analysis
hist(cancer$diagnosis)
count_0 <- sum(cancer$diagnosis == 0)
count_1 <- sum(cancer$diagnosis == 1)
print(count_0)
print(count_1)



# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(cancer$diagnosis, p = 0.8, list = FALSE)
train <- cancer[trainIndex,]
test <- cancer[-trainIndex,]

train
df[, ]
train[,]

head(test[,])

head(cancer)
train= train[,c("diagnosis", "concave.points_worst", "area_worst", "radius_worst", "perimeter_worst", "texture_worst",
                  "texture_mean", "concave.points_mean", "area_se")]

test= test[,c("diagnosis","concave.points_worst", "area_worst", "radius_worst", "perimeter_worst", "texture_worst",
                  "texture_mean", "concave.points_mean", "area_se")]
#test= test
#### RF 300 trees Depth 3
# Fit random forest model on training data
rf_model= randomForest(diagnosis ~ ., data = train, ntree = 300, maxdepth = 3)

# Make predictions on test data
rf_pred= predict(rf_model, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$pred_labels= ifelse(rf_pred >= threshold, 1, 0)

test$pred_labels= as.factor(test$pred_labels)
test$diagnosis= as.factor(test$diagnosis)

# Compute confusion matrix and recall
if (is.factor(test$pred_labels) ){
  print(is.factor(test$pred_labels))
}
if ( is.factor(test$diagnosis)){
  print(is.factor(test$diagnosis))
}
print(length(levels(test$diagnosis)))
print(length(levels(test$pred_labels)))
conf_mat= confusionMatrix(test$diagnosis, test$pred_labels)

# Print confusion matrix and recall
print(conf_mat)



## RF 500 trees Depth 3
rf_model= randomForest(diagnosis ~ ., data = train, ntree = 500, maxdepth = 3)

# Make predictions on test data
rf_pred= predict(rf_model, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$pred_labels= ifelse(rf_pred >= threshold, 1, 0)

test$pred_labels= as.factor(test$pred_labels)
test$diagnosis= as.factor(test$diagnosis)


conf_mat= confusionMatrix(test$diagnosis, test$pred_labels)

# Print confusion matrix and recall
print(conf_mat)






## RF 300 trees Depth 5
rf_model= randomForest(diagnosis ~ ., data = train, ntree = 300, maxdepth = 5)

# Make predictions on test data
rf_pred= predict(rf_model, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$pred_labels= ifelse(rf_pred >= threshold, 1, 0)

test$pred_labels= as.factor(test$pred_labels)
test$diagnosis= as.factor(test$diagnosis)


conf_mat= confusionMatrix(test$diagnosis, test$pred_labels)

# Print confusion matrix and recall
print(conf_mat)




## RF 500 trees Depth 5
rf_model= randomForest(diagnosis ~ ., data = train, ntree = 500, maxdepth = 5)

# Make predictions on test data
rf_pred= predict(rf_model, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$pred_labels= ifelse(rf_pred >= threshold, 1, 0)

test$pred_labels= as.factor(test$pred_labels)
test$diagnosis= as.factor(test$diagnosis)


conf_mat= confusionMatrix(test$diagnosis, test$pred_labels)

# Print confusion matrix and recall
print(conf_mat)



# Initialize vector to store accuracy results
accuracy_results <- numeric(10)

for (i in 1:10) {
  # Fit random forest model on training data
  rf_model <- randomForest(diagnosis ~ ., data = train, ntree = 300, maxdepth = 3)

  # Make predictions on test data
  rf_pred <- predict(rf_model, newdata = test, type = "response")

  # Turning predictions into labels
  threshold <- 0.1
  test$pred_labels <- ifelse(rf_pred >= threshold, 1, 0)
  test$pred_labels <- as.factor(test$pred_labels)
  test$diagnosis <- as.factor(test$diagnosis)

  # Compute confusion matrix and recall
  conf_mat <- confusionMatrix(test$diagnosis, test$pred_labels)

  # Store accuracy result
  accuracy_results[i] <- conf_mat$overall["Accuracy"]
}
# Print accuracy results
print(accuracy_results)
mean(accuracy_results)



# Initialize vector to store accuracy results
accuracy_results <- numeric(10)

for (i in 1:10) {
  # Fit random forest model on training data
  rf_model <- randomForest(diagnosis ~ ., data = train, ntree = 500, maxdepth = 3)

  # Make predictions on test data
  rf_pred <- predict(rf_model, newdata = test, type = "response")

  # Turning predictions into labels
  threshold <- 0.1
  test$pred_labels <- ifelse(rf_pred >= threshold, 1, 0)
  test$pred_labels <- as.factor(test$pred_labels)
  test$diagnosis <- as.factor(test$diagnosis)

  # Compute confusion matrix and recall
  conf_mat <- confusionMatrix(test$diagnosis, test$pred_labels)

  # Store accuracy result
  accuracy_results[i] <- conf_mat$overall["Accuracy"]
}

# Print accuracy results
print(accuracy_results)
mean(accuracy_results)



# Initialize vector to store accuracy results
accuracy_results <- numeric(10)

for (i in 1:10) {
  # Fit random forest model on training data
  rf_model <- randomForest(diagnosis ~ ., data = train, ntree = 500, maxdepth = 5)

  # Make predictions on test data
  rf_pred <- predict(rf_model, newdata = test, type = "response")

  # Turning predictions into labels
  threshold <- 0.1
  test$pred_labels <- ifelse(rf_pred >= threshold, 1, 0)
  test$pred_labels <- as.factor(test$pred_labels)
  test$diagnosis <- as.factor(test$diagnosis)

  # Compute confusion matrix and recall
  conf_mat <- confusionMatrix(test$diagnosis, test$pred_labels)

  # Store accuracy result
  accuracy_results[i] <- conf_mat$overall["Accuracy"]
}

# Print accuracy results
print(accuracy_results)
mean(accuracy_results)








test$diagnosis
test$pred_labels

dim(predicted_labels)
dim(test$diagnosis)
dim(test)
levels(test$diagnosis)
levels(predicted_labels)



########################################### Gradient Boost #######################################################


## Gradient Boost with 300 trees and depth of 3
# Fit random forest model on training data
gb_md = gbm(diagnosis ~ ., data = train, n.trees = 300, interaction.depth = 3)

# Make predictions on test data
gb_pred= predict(gb_md, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$gb_labels= ifelse(gb_pred >= threshold, 1, 0)

test$gb_labels= as.factor(test$gb_labels)
test$diagnosis= as.factor(test$diagnosis)

# Compute confusion matrix and recall
if (is.factor(test$gb_labels) ){
  print(is.factor(test$gb_labels))
}
if ( is.factor(test$diagnosis)){
  print(is.factor(test$diagnosis))
}
print(length(levels(test$diagnosis)))
print(length(levels(test$gb_labels)))
conf_mat= confusionMatrix(test$diagnosis, test$gb_labels)

# Print confusion matrix and recall
print(conf_mat)



## Gradient Boost with 500 trees and depth of 3
# Fit random forest model on training data
gb_md = gbm(diagnosis ~ ., data = train, n.trees = 500, interaction.depth = 3)

# Make predictions on test data
gb_pred= predict(gb_md, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$gb_labels= ifelse(gb_pred >= threshold, 1, 0)

test$gb_labels= as.factor(test$gb_labels)
test$diagnosis= as.factor(test$diagnosis)


conf_mat= confusionMatrix(test$diagnosis, test$gb_labels)

# Print confusion matrix
print(conf_mat)




## Gradient Boost with 300 trees and depth of 5
# Fit random forest model on training data
gb_md = gbm(diagnosis ~ ., data = train, n.trees = 300, interaction.depth = 5)

# Make predictions on test data
gb_pred= predict(gb_md, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$gb_labels= ifelse(gb_pred >= threshold, 1, 0)

test$gb_labels= as.factor(test$gb_labels)
test$diagnosis= as.factor(test$diagnosis)

conf_mat= confusionMatrix(test$diagnosis, test$gb_labels)

# Print confusion matrix and recall
print(conf_mat)



## Gradient Boost with 500 trees and depth of 5
# Fit random forest model on training data
gb_md = gbm(diagnosis ~ ., data = train, n.trees = 500, interaction.depth = 5)

# Make predictions on test data
gb_pred= predict(gb_md, newdata = test, type = "response")

#turning predictins into labels
threshold= 0.1
test$gb_labels= ifelse(gb_pred >= threshold, 1, 0)

test$gb_labels= as.factor(test$gb_labels)
test$diagnosis= as.factor(test$diagnosis)

#confusion matrix
conf_mat= confusionMatrix(test$diagnosis, test$gb_labels)

# Print confusion matrix and recall
print(conf_mat)



# Initialize vector to store accuracy results
accuracy_results <- numeric(10)

for (i in 1:10) {
  # Fit gradient boosting model on training data
  gb_md <- gbm(diagnosis ~ ., data = train, n.trees = 300, interaction.depth = 5)

  # Make predictions on test data
  gb_pred <- predict(gb_md, newdata = test, type = "response")

  # Turning predictions into labels
  threshold <- 0.1
  test$gb_labels <- ifelse(gb_pred >= threshold, 1, 0)
  test$gb_labels <- as.factor(test$gb_labels)
  test$diagnosis <- as.factor(test$diagnosis)

  # Compute confusion matrix and recall
  conf_mat <- confusionMatrix(test$diagnosis, test$gb_labels)

  # Store accuracy result
  accuracy_results[i] <- conf_mat$overall["Accuracy"]
}
# Print accuracy results
print(accuracy_results)
mean(accuracy_results)



############################################# AdaBoost##############################################

install.packages("ada")
library(ada)


##### AdaBoost with 300 trees Depth of 3
# Fit adaboost model on training data
ada_md = ada(diagnosis ~ ., data = train, iter = 300, control = rpart.control(maxdepth = 3))

# Make predictions on test data
ada_pred= predict(ada_md, newdata = test, type = "response")
ada_pred= as.numeric(as.character(ada_pred))

#turning predictins into labels
threshold= 0.1
test$ada_labels= ifelse(ada_pred >= threshold, 1, 0)

test$ada_labels= as.factor(test$ada_labels)
test$diagnosis= as.factor(test$diagnosis)


# Compute confusion matrix and recall
if (is.factor(test$ada_labels) ){
  print(is.factor(test$ada_labels))
}
if ( is.factor(test$diagnosis)){
  print(is.factor(test$diagnosis))
}
print(length(levels(test$diagnosis)))
print(length(levels(test$ada_labels)))

conf_mat= confusionMatrix(test$diagnosis, test$ada_labels)

# Print confusion matrix and recall
print(conf_mat)



#### Adaboost with 500 trees and Depth of 3
# Fit adaboost model on training data
ada_md= ada(diagnosis ~ .,data = train, iter= 500, control = rpart.control(maxdepth = 3))

# Make predictions on test data
ada_pred= predict(ada_md, newdata = test, type = "response")
ada_pred= as.numeric(as.character(ada_pred))

#turning predictins into labels
threshold= 0.1
test$ada_labels= ifelse(ada_pred >= threshold, 1, 0)

test$ada_labels= as.factor(test$ada_labels)
test$diagnosis= as.factor(test$diagnosis)

#Computing confusion matrix
conf_mat= confusionMatrix(test$diagnosis, test$ada_labels)

# Print confusion matrix and recall
print(conf_mat)






#### Adaboost with 300 trees and Depth of 5
# Fit adaboost model on training data
ada_md= ada(diagnosis ~ .,data = train, iter= 300, control = rpart.control(maxdepth = 5))

# Make predictions on test data
ada_pred= predict(ada_md, newdata = test, type = "response")
ada_pred= as.numeric(as.character(ada_pred))

#turning predictins into labels
threshold= 0.1
test$ada_labels= ifelse(ada_pred >= threshold, 1, 0)

test$ada_labels= as.factor(test$ada_labels)
test$diagnosis= as.factor(test$diagnosis)

#Computing confusion matrix
conf_mat= confusionMatrix(test$diagnosis, test$ada_labels)

# Print confusion matrix and recall
print(conf_mat)





#### Adaboost with 500 trees and Depth of 5
# Fit adaboost model on training data
ada_md= ada(diagnosis ~ .,data = train, iter= 500, control = rpart.control(maxdepth = 5))

# Make predictions on test data
ada_pred= predict(ada_md, newdata = test, type = "response")
ada_pred= as.numeric(as.character(ada_pred))

#turning predictins into labels
threshold= 0.1
test$ada_labels= ifelse(ada_pred >= threshold, 1, 0)

test$ada_labels= as.factor(test$ada_labels)
test$diagnosis= as.factor(test$diagnosis)

#Computing confusion matrix
conf_mat= confusionMatrix(test$diagnosis, test$ada_labels)

# Print confusion matrix and recall
print(conf_mat)




# Initialize vector to store accuracy results
accuracy_results <- numeric(10)

for (i in 1:10) {
  # Fit Adaboost model on training data
  ada_md <- ada(diagnosis ~ .,data = train, iter = 500, control = rpart.control(maxdepth = 5))

  # Make predictions on test data
  ada_pred <- predict(ada_md, newdata = test, type = "response")
  ada_pred <- as.numeric(as.character(ada_pred))

  # Turning predictions into labels
  threshold <- 0.1
  test$ada_labels <- ifelse(ada_pred >= threshold, 1, 0)
  test$ada_labels <- as.factor(test$ada_labels)
  test$diagnosis <- as.factor(test$diagnosis)

  # Compute confusion matrix and recall
  conf_mat <- confusionMatrix(test$diagnosis, test$ada_labels)

  # Store accuracy result
  accuracy_results[i] <- conf_mat$overall["Accuracy"]
}
# Print accuracy results
print(accuracy_results)
mean(accuracy_results)



#chaning target variable to factor
cancer$diagnosis <- as.factor(cancer$diagnosis)
train$diagnosis <- as.factor(train$diagnosis)
test$diagnosis <- as.factor(test$diagnosis)



#building model based on data split
rf_md= randomForest(diagnosis~.,data= train)
rf_md

#predicting on the test variables
pred_test= predict(rf_md, newdata = test, type= "class")
pred_test

# Create a confusion matrix
rf_cm= table(test$diagnosis, pred_test)
print(rf_cm)





# Set up empty list to store model results
recall_list= list()
# Set up for loops to iterate over number of trees and tree depth
for (i in seq(1:10)) {
  for (n_trees in c(50, 100, 200)) {
    for (tree_depth in c(2, 4, 6)) {
      # Set up model name
      model_name= paste0("rf_", n_trees, "_trees_", tree_depth, "_depth")

      # Fit random forest model on training data
      rf_model= randomForest(diagnosis ~ ., data = train, ntree = n_trees, maxdepth = tree_depth)

      # Make predictions on test data
      rf_pred= predict(rf_model, newdata = test)

      # Compute confusion matrix and recall
      conf_mat= confusionMatrix(factor(ifelse(rf_pred < 0.1, 0, 1)), test$diagnosis)

    }
  }
}
 recall= conf_mat$byClass[1] # Recall for class "M"

      # Append model and recall to the list
      recall_list[[model_name]] <- recall

# Fit random forest model on training data
rf_model <- randomForest(diagnosis ~ ., data = train, ntree = 100, maxdepth = 6)

# Make predictions on test data
rf_pred <- predict(rf_model, newdata = test)

#turning predictins into labels
threshold= 0.1
predicted_labels= ifelse(rf_pred >= threshold, 1, 0)
# Compute confusion matrix and recall
conf_mat= confusionMatrix(predicted_labels, as.factor(test$diagnosis))
recall= conf_mat$diagnosis[1] # Recall for class "M"

# Print confusion matrix and recall
print(conf_mat)
print(recall)

test$diagnosis













recall_list= list()
for (i in seq(1:10)) {
  for (k in c(50, 100, 200)) {
    for (j in c(2, 4, 6)) {
      model_name= paste0("gbm_", i, "_trees_", k, "_depth", j)
      gb_md= gbm(diagnosis ~ ., data = train, n.trees = k, interaction.depth = j)
      # Make predictions on the test set
      pred= predict(gb_md, newdata = test, n.trees = k, type = "response")
      # Compute confusion matrix and recall
      cm= confusionMatrix(ifelse(pred > 0.1, 1, 0), test$diagnosis)
      recall= cm$byClass["recall"]
      # Create a new list for the recall value and add it to the larger list
      recall_list[[model_name]]= list(recall = recall)
    }
  }
}

# Check the levels of the diagnosis variable in the train and test datasets
lv1 = levels(train$diagnosis)

lv2 = levels(test$diagnosis)

print("stop")
# If the levels of the diagnosis variable are NULL, set the levels using unique() function
if (is.null(levels(train$diagnosis))) {
  levels(train$diagnosis) <- unique(train$diagnosis)
}

if (is.null(levels(test$diagnosis))) {
  levels(test$diagnosis) <- unique(test$diagnosis)
}

# Check the levels of the diagnosis variable again to confirm they are set
levels(train$diagnosis)
levels(test$diagnosis)
levels(labels)

if (is.null(levels(labels))) {
  levels(labels) <- unique(labels)
}


# Check for missing values in the diagnosis variable of the test dataset
sum(is.na(test$diagnosis))



# Set up empty list to store model results
recall_list <- list()
labels_list <- list()
conf_mat_list <- list()

# Set up for loops to iterate over number of trees and tree depth
for (i in seq(1:10)) {
  for (n_trees in c(50, 100, 200)) {
    for (tree_depth in c(2, 4, 6)) {
      # Set up model name
      model_name <- paste0("rf_", n_trees, "_trees_", tree_depth, "_depth")

      # Fit random forest model on training data
      rf_model <- randomForest(diagnosis ~ ., data = train, ntree = n_trees, maxdepth = tree_depth)

      # Make predictions on test data
      rf_pred <- predict(rf_model, newdata = test)

      # Compute predicted labels
      labels <- ifelse(rf_pred < 0.1, 0, 1)

      # Compute confusion matrix
      conf_mat <- confusionMatrix(labels, test$diagnosis)

      # Compute recall
      recall <- conf_mat$byClass[1] # Recall for class "M"

      # Store predicted labels and confusion matrix separately
      labels_list[[model_name]] <- labels
      conf_mat_list[[model_name]] <- conf_mat$confusionMatrix

      # Append model and recall to the list
      recall_list[[model_name]] <- recall
    }
  }
}






