rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
library(glmnet)

# data(Boston, package = "MASS")
# Boston %>% readr::write_csv(file = "./database/Boston.csv") 
# colnames(Boston)

# Data Load ---------------------------------------------------------------

Boston <- readr::read_csv("./database/Boston.csv")


# Modelo Linear -----------------------------------------------------------

Boston.scaled <- scale(Boston)

ols.mdl <- lm(medv ~ . , data = Boston)
summary(ols.mdl)



# Rodando Ridge -----------------------------------------------------------
x <- Boston.scaled[, 1:13] %>% data.matrix()
y <- Boston.scaled[, 14] %>% data.matrix()

lambdas_to_try <- 10^seq(-3, 3, length.out = 100)


ridge_cv <- cv.glmnet(x, y,
                      alpha = 0,
                      lambda = lambdas_to_try,
                      standardize = FALSE,
                      nfolds = 7)



# Plot cross-validation results
plot(ridge_cv)

# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.min


# Ridge Regression - coeficientes
ridge.mdl <- glmnet(x, y, alpha = 0, lambda = lambda_cv, standardize = FALSE)

# Coeficientes do modelo
coef(ridge.mdl)

# Fitted values
y_hat <- predict(ridge.mdl, x)



ridge.mdl2 <- glmnet(x, y, alpha = 0, lambda = lambdas_to_try, standardize = FALSE)
plot(ridge.mdl2)


# Rodando Lasso -----------------------------------------------------------

lasso_cv <- cv.glmnet(x, y,
                      alpha = 1,
                      lambda = lambdas_to_try,
                      standardize = FALSE,
                      nfolds = 7)



# Plot cross-validation results
plot(lasso_cv)

# Best cross-validated lambda
lambda_cv <- lasso_cv$lambda.min


# Ridge Regression - coeficientes
lasso.mdl <- glmnet(x, y, alpha = 1, lambda = lambda_cv, standardize = FALSE)

# Coeficientes do modelo
coef(lasso.mdl)

# Fitted values
y_hat <- predict(lasso.mdl, x)



lasso.mdl2 <- glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = TRUE)
plot(lasso.mdl2)
