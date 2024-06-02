# Setup -------------------------------------------------------------------
# Clear all
rm(list = ls())

# Libraries
library(caret)
library(dplyr)

# Load Data ---------------------------------------------------------------
Sacramento <- read.csv("./database/WB7 - CV- Sacramento.csv")

# Análise Exploratória de Dados -------------------------------------------
head(Sacramento)
summary(Sacramento)

# Pré-processamento -------------------------------------------------------

# Tratar valores ausentes e variáveis categóricas
housing_data <- Sacramento %>%
  mutate_if(is.character, factor) %>%
  na.omit()

# Implementação de Validação Cruzada --------------------------------------
# k-Fold Cross-Validation
set.seed(123)
train_control_kfold <- trainControl(method = "cv", number = 10)
model_kfold <- train(price ~ beds + baths + sqft + type,
                     data = housing_data, method = "lm", trControl = train_control_kfold)
print(model_kfold)
summary(model_kfold)

# Repeated k-Fold Cross-Validation
set.seed(123)
train_control_repeated <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_repeated <- train(price ~ beds + baths + sqft + type,
                        data = housing_data, method = "lm", trControl = train_control_repeated)
print(model_repeated)
summary(model_repeated)


# Leave-One-Out Cross-Validation
set.seed(123)
train_control_loocv <- trainControl(method = "LOOCV")
model_loocv <- train(price ~ beds + baths + sqft + type,
                     data = housing_data, method = "lm", trControl = train_control_loocv)
print(model_loocv)
summary(model_loocv)
