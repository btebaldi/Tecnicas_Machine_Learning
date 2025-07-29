# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)
library(dplyr)
library(glmnet)


# Data load ---------------------------------------------------------------
student_mat <- read_delim("./database/WB3 - student-mat.csv", 
                          delim = ";",
                          escape_double = FALSE,
                          trim_ws = TRUE)


# Estatistica descritivas -------------------------------------------------
str(student_mat)
summary(student_mat)



# Exclusao de variaveis ---------------------------------------------------

student_mat <- student_mat %>% select(-G2,-G3, -Mjob, -Fjob, -reason, -guardian)


# Alocacao de fatores
for (col in colnames(student_mat)) {
  
  cat(sprintf("Processing: %s\n", col))
  if(!is.numeric(student_mat[[col]])) {
    cat(sprintf("Colunm %s is non numeric: creating factors. ", col))
    
    aux <- factor(student_mat[[col]])
    
    if(length(levels(aux)) != 2){
      # cat(col, " has more than two levels\n" )
      cat(sprintf("%s has more than two levels. Dropping.", col), "\n" )
      student_mat[col] = NULL
    }
    else{
      student_mat[col] <- as.numeric(aux) -1
      
      cat(sprintf("%s = 0\t%s = 1\n", levels(aux)[1], levels(aux)[2]))
    }
  } else {
    # cat("No action required.\n")
  }
  # cat("\n")
}


# Padronizacao das variaveis ----------------------------------------------

y <- student_mat %>% select(G1) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- student_mat %>% select(-G1) %>% scale() %>% as.matrix()


# Lasso Regression - cross-validation -------------------------------------

lambdas_to_try <- 10^seq(-3, 4, length.out = 100)

ridge_cv <- cv.glmnet(X, y,
                      alpha = 1,
                      lambda = lambdas_to_try,
                      standardize = FALSE,
                      nfolds = 10)

# Plot cross-validation results
plot(ridge_cv)

# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.min


# Lasso Regression - coeficientes, SSR e R2 -------------------------------

model <- glmnet(X, y, alpha = 1, lambda = 1, standardize = FALSE)

# Coeficientes do modelo
coef(model)

# Fitted values
y_hat <- predict(model, X)
SSR <- t(y - y_hat) %*% (y - y_hat)
rsq_ridge <- cor(y, y_hat)^2

cat(sprintf("SSR: %5.3f\n R2: %5.3f", SSR, rsq_ridge))

# Lasso Regression - coeficientes vs lambda -------------------------------

res <- glmnet(X, y, alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
