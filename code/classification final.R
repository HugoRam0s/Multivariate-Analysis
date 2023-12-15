library (dplyr)
#Load the data 
diabetes <- read.csv("/Users/beatrizrebelo/Downloads/diabetes_PCA (1).csv")
head(diabetes)

class_counts <- table(diabetes$Diabetes_012)
print(class_counts)
diabetes <- diabetes[, -1]


library(ggplot2)
library(lattice)
library(caret)
library(nnet)

diabetes$Diabetes_012 <- as.factor(diabetes$Diabetes_012)

set.seed(60)  # for reproducibility
partition <- createDataPartition(diabetes$Diabetes_012, p = 0.60, list = FALSE)


# Create data frames for training and testing
train_data <- diabetes[partition, ]
test_data <- diabetes[-partition, ]

X_train <- train_data[, -which(names(train_data) == "Diabetes_012")]
y_train <- train_data$Diabetes_012
X_test <- test_data[, -which(names(test_data) == "Diabetes_012")]
y_test <- test_data$Diabetes_012



train_control <- trainControl(method = "cv", number = 5)
# Logistic Regression Model for Classification
lr_clf <- train(x = X_train, y = y_train, method = "multinom", 
                trControl = train_control, 
                tuneGrid = expand.grid(.decay = c(0.1, 1, 10)))

# Print the best tuning parameters
print(lr_clf$bestTune)

predictions1 <- predict(lr_clf, newdata = X_test)




library(MLmetrics)
# Calculate Accuracy
accuracy <- confusionMatrix(predictions1, test_data$Diabetes_012)
lr_accuracy <- accuracy$overall['Accuracy']

# Print the metrics
print(paste("Accuracy:", lr_accuracy))



cm1 <- confusionMatrix(predictions1, test_data$Diabetes_012)

# Calculate metrics from the confusion matrix
overall_stats <- cm1$overall
class_stats <- cm1$byClass


# Class-specific Precision, Recall, and F1 Score
precision_values <- class_stats[,'Precision']
recall_values <- class_stats[,'Recall']
f1_values <- class_stats[,'F1']

# Print the metrics
print(paste("Accuracy:", lr_accuracy))
print(paste("Precision for each class:", toString(precision_values)))
print(paste("Recall for each class:", toString(recall_values)))
print(paste("F1 Score for each class:", toString(f1_values)))

# Macro-averaged Precision, Recall, and F1 Score
macro_precision <- mean(precision_values)
macro_recall <- mean(recall_values)
macro_f1 <- mean(f1_values)

print(paste("Macro-averaged Precision:", macro_precision))
print(paste("Macro-averaged Recall:", macro_recall))
print(paste("Macro-averaged F1 Score:", macro_f1))



#RANDOM FOREST
library(lattice)
library(usethis)
library(randomForest)
library(caret)
library(devtools)

#rf_clf <- randomForest(y_train ~ ., data=X_train, ntree = 100, mtry = sqrt(ncol(X_train)))

tuneGrid <- expand.grid( mtry = c(2, 3, 4, 5))  
rf_clf <- train(x = X_train, y = y_train, method="rf", trControl=train_control, tuneGrid=tuneGrid, ntree=100)
print(rf_clf$bestTune)




predictions2 <- predict(rf_clf, newdata = X_test)

# Calculate Accuracy
accuracy <- confusionMatrix(predictions2, y_test)
rf_accuracy <- accuracy$overall['Accuracy']

# Print the metrics
print(paste("Accuracy_rf:", rf_accuracy))


cm2 <- confusionMatrix(predictions2, y_test)

# Calculate metrics from the confusion matrix
overall_stats2 <- cm2$overall
class_stats2 <- cm2$byClass


# Class-specific Precision, Recall, and F1 Score
precision_values2 <- class_stats2[,'Precision']
recall_values2 <- class_stats2[,'Recall']
f1_values2 <- class_stats2[,'F1']

# Print the metrics
print(paste("Accuracy_rf:", rf_accuracy))
print(paste("Precision for each class_rf:", toString(precision_values2)))
print(paste("Recall for each class_rf:", toString(recall_values2)))
print(paste("F1 Score for each class_rf:", toString(f1_values2)))

# Macro-averaged Precision, Recall, and F1 Score
macro_precision_rf <- mean(precision_values2)
macro_recall_rf <- mean(recall_values2)
macro_f1_rf <- mean(f1_values2)

print(paste("Macro-averaged Precision:", macro_precision_rf))
print(paste("Macro-averaged Recall:", macro_recall_rf))
print(paste("Macro-averaged F1 Score:", macro_f1_rf))


#SVM Model
library(e1071)

#svm_linear_clf <- svm(y_train ~ ., data=X_train, method="C-classification", kernel="linear")

#svm_rbf_clf <- svm(y_train ~ ., data=X_train, method="C-classification", kernel="radial")

tuneGrid <- expand.grid(
  sigma = 10^seq(-3, 2, by=1),  
  C = 10^seq(-2, 1, by=1)
)

svm_clf <- train(x = X_train, y = y_train, method="svmRadial", trControl=train_control, tuneGrid=tuneGrid)
print(svm_clf$bestTune)


#predictions31 <- predict(svm_linear_clf, newdata = X_test)
predictions32 <- predict(svm_clf, newdata = X_test)


# Calculate Accuracy
accuracy <- confusionMatrix(predictions32, y_test)
svm_rbf_accuracy <- accuracy$overall['Accuracy']

# Print the metrics
print(paste("Accuracy_svm_rbf:", svm_rbf_accuracy))



cm3 <- confusionMatrix(predictions32, y_test)

# Calculate metrics from the confusion matrix
overall_stats3 <- cm3$overall
class_stats3 <- cm3$byClass


# Class-specific Precision, Recall, and F1 Score
precision_values3 <- class_stats3[,'Precision']
recall_values3 <- class_stats3[,'Recall']
f1_values3 <- class_stats3[,'F1']

# Print the metrics
print(paste("Accuracy_svm:", svm_rbf_accuracy))
print(paste("Precision for each class_svm:", toString(precision_values3)))
print(paste("Recall for each class_svm:", toString(recall_values3)))
print(paste("F1 Score for each class_svm:", toString(f1_values3)))

# Macro-averaged Precision, Recall, and F1 Score
macro_precision_svm <- mean(precision_values3)
macro_recall_svm <- mean(recall_values3)
macro_f1_svm <- mean(f1_values3)

print(paste("Macro-averaged Precision:", macro_precision_svm))
print(paste("Macro-averaged Recall:", macro_recall_svm))
print(paste("Macro-averaged F1 Score:", macro_f1_svm))



#K_Nearest Neighbours 
k_values <- 1:20  # This tests k from 1 to 20, adjust based on your dataset

# Define the tuning grid
tuneGrid <- expand.grid(k = k_values)

# Train the KNN model
set.seed(42)  # for reproducibility
knn_clf <- train(x = X_train, y = y_train, 
                   method="knn", 
                   tuneLength=10,
                   trControl=train_control,
                   tuneGrid=tuneGrid)

# Print the best parameters
print(knn_clf$bestTune)

# Making predictions on the test data
predictions4 <- predict(knn_clf, X_test)

# Calculate Accuracy
accuracy <- confusionMatrix(predictions4, y_test)
knn_accuracy <- accuracy$overall['Accuracy']

# Print the metrics
print(paste("Accuracy_knn:", knn_accuracy))


cm4 <- confusionMatrix(predictions4, y_test)

# Calculate metrics from the confusion matrix
overall_stats4 <- cm4$overall
class_stats4 <- cm4$byClass


# Class-specific Precision, Recall, and F1 Score
precision_values4 <- class_stats4[,'Precision']
recall_values4 <- class_stats4[,'Recall']
f1_values4 <- class_stats4[,'F1']

# Print the metrics
print(paste("Accuracy_svm:", knn_accuracy))
print(paste("Precision for each class_svm:", toString(precision_values4)))
print(paste("Recall for each class_svm:", toString(recall_values4)))
print(paste("F1 Score for each class_svm:", toString(f1_values4)))

# Macro-averaged Precision, Recall, and F1 Score
macro_precision_knn <- mean(precision_values4)
macro_recall_knn <- mean(recall_values4)
macro_f1_knn <- mean(f1_values4)

print(paste("Macro-averaged Precision:", macro_precision_knn))
print(paste("Macro-averaged Recall:", macro_recall_knn))
print(paste("Macro-averaged F1 Score:", macro_f1_knn))




#Naive Bayes

# Define the range of hyperparameters
laplace_values <- seq(0, 1, by = 0.1)  # Adjust as needed
usekernel_values <- c(TRUE, FALSE)
adjust_values <- seq(0.5, 2, by = 0.1)  # Adjust as needed

# Define the tuning grid
tuneGrid <- expand.grid(laplace = laplace_values,
                        usekernel = usekernel_values,
                        adjust = adjust_values)



# Train the Naive Bayes model
set.seed(42)  # for reproducibility
nb_clf <- train(x = X_train, y = y_train, 
                           method="naive_bayes",
                           trControl=train_control,
                           tuneGrid=tuneGrid)

# Print the best parameters
print(nb_clf$bestTune)

predictions5 <- predict(nb_clf, newdata = X_test)

# Calculate Accuracy
accuracy <- confusionMatrix(predictions5, y_test)
nb_accuracy <- accuracy$overall['Accuracy']

# Print the metrics
print(paste("Accuracy_nb:", nb_accuracy))


cm5 <- confusionMatrix(predictions5, y_test)

# Calculate metrics from the confusion matrix
overall_stats5 <- cm5$overall
class_stats5 <- cm5$byClass


# Class-specific Precision, Recall, and F1 Score
precision_values5 <- class_stats5[,'Precision']
recall_values5 <- class_stats5[,'Recall']
f1_values5 <- class_stats5[,'F1']

# Print the metrics
print(paste("Accuracy_nb:", nb_accuracy))
print(paste("Precision for each class_nb:", toString(precision_values5)))
print(paste("Recall for each class_nb:", toString(recall_values5)))
print(paste("F1 Score for each class_nb:", toString(f1_values5)))

# Macro-averaged Precision, Recall, and F1 Score
macro_precision_nb <- mean(precision_values5)
macro_recall_nb <- mean(recall_values5)
macro_f1_nb <- mean(f1_values5)

print(paste("Macro-averaged Precision:", macro_precision_nb))
print(paste("Macro-averaged Recall:", macro_recall_nb))
print(paste("Macro-averaged F1 Score:", macro_f1_nb))






# Assuming you have calculated the metrics for each model and stored them as follows:
models <- c("lr_clf", "rf_clf", "svm_rbf_clf", "knn_clf", "nb_clf")
accuracy <- c(lr_accuracy, rf_accuracy, svm_rbf_accuracy, knn_accuracy, nb_accuracy)
precision <- c(macro_precision, macro_precision_rf, macro_precision_svm, macro_precision_knn, macro_precision_nb)
recall <- c(macro_recall, macro_recall_rf, macro_recall_svm, macro_recall_knn, macro_recall_nb)
f1_score <- c(macro_f1, macro_f1_rf, macro_f1_svm, macro_f1_knn, macro_f1_nb)

# Create a data frame
model_comparison <- data.frame(models, accuracy, precision, recall, f1_score)

# Display the comparison table
print(model_comparison)


library(ggplot2)
melted_data <- reshape2::melt(model_comparison, id.vars = 'models')

ggplot(melted_data, aes(x = models, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(y = "Score", x = "Models") +
  ggtitle("Comparison of Model Metrics") +
  scale_fill_discrete(name = "Metrics")

