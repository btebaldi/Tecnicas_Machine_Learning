
# Setup -------------------------------------------------------------------
# Clear all
rm(list = ls())

# libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(FNN)

# Load Data ---------------------------------------------------------------
tbl <- read.csv("./database/WB6 - KNN Iris.csv")

# Data Preparation --------------------------------------------------------
boxplot(tbl[, -5])

# Normalize the data
tbl_data_scaled <- tbl[, -5] %>% scale() %>% as.data.frame()
tbl_data_scaled$Species <- tbl[,5]

# Visualization -----------------------------------------------------------
boxplot(tbl_data_scaled[, -5])

# Train-Test Split --------------------------------------------------------
set.seed(42)  # For reproducibility

n <- nrow(tbl_data_scaled)
train_idx <- sample(1:n, n / 2, replace = FALSE)
train_sample <- tbl_data_scaled[train_idx, ]
test_sample <- tbl_data_scaled[-train_idx, ]

# Model Training and Evaluation -------------------------------------------
# k = 3
prediction_k3 <- FNN::knn(train = train_sample[, -5],
                          test = test_sample[, -5],
                          cl = train_sample[, 5], k = 3)

y_test <- test_sample$Species
confusion_matrix_k3 <- table(y_test, prediction_k3)
accuracy_k3 <- sum(diag(confusion_matrix_k3)) / sum(confusion_matrix_k3)
cat(sprintf("Accuracy for k=2: %3.2f\n", accuracy_k3))


# Plotting accuracy for different k values --------------------------------
k_values <- 1:20
accuracy_values <- sapply(k_values, function(k) {
  prediction <- FNN::knn(train = train_sample[, -5],
                         test = test_sample[, -5],
                         cl = train_sample[, 5], k = k)
  confusion_matrix <- table(test_sample[,5], prediction)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(accuracy)
})

accuracy_df <- data.frame(k = k_values, accuracy = accuracy_values)

ggplot(accuracy_df, aes(x = k, y = accuracy)) +
  geom_line() +
  labs(title = "Accuracy for Different k Values", x = "k", y = "Accuracy")
