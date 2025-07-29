


# Setup -------------------------------------------------------------------
rm(list = ls())
library(glmnet)
library(readxl)
library(dplyr)


#' ### Item a)
#' Para a nossa analise vamos utilizar um banco com 30 variaveis explicativas e
#' uma variavel dependente. Carregue os dados disponíveis no e-class
#' ("logistic.xlsx").

# Data Load ---------------------------------------------------------------

# Para a nossa analise vamos utilizar um banco com 30 variaveis explicativas e
# uma variavel dependente. Carregue os dados disponíveis no e-class
# (``logistic.csv'').
database <- read_excel(path= here::here("./AX - Logistic Lasso/logistic.xlsx"))


#' ### Item b)
#' Determine as estatísticas descritivas do banco de dados.

# Determine as estatísticas descritivas do banco de dados.
summary(database)


#' ### Item c)
#' Faça um diagrama de boxplot dos dados.
database %>% select(starts_with("X")) %>% boxplot()



#' ### Item d)
#' Dado a distribuição dos dados, você faria uma normalização nos dados?
#' (justifique)
#' 
#' **Nao. Os dado tem média e variancia aproximadas**
#' 


#' ### Item e)
#' Utilize 10 *folds* em um processo de cross-validation para determinar
#' o melhor valor de lamba. ($\lambda \in [10^{-5}, 10^3]$).


# Separando x e y
x <- database %>% select(starts_with("X")) %>% data.matrix()
y <- if_else(database$Y == "H", 1, 0)


# Cross Validation --------------------------------------------------------

# Utilize 10 folds em um processo de cross-validation para determinar o melhor
# valor de lamba.
lambdas_to_try = 10^seq(-5, 3,  length.out=100)

cv.fit = cv.glmnet(x, y, nfolds = 10,
                   lambda = lambdas_to_try,
                   family = "binomial",
                   type.measure = "class")


plot(cv.fit)


lambda.min <- cv.fit$lambda.min

#' ### Item f)
#' Determine os coeficientes do modelo, para o melhor lambda escolhido no item
#' anterior.

fit = glmnet(x, y, lambda = lambda.min, family = "binomial", alpha = 1)

coef(fit, s = "lambda.min")


#' ### Item g)
#' Avaliação de performance *In-sample*: Faça uma previsão dos valores.
#' Apresente os resultados em uma *confusion matrix*.

pred <- predict(fit, newx = x, type = "class")

# confusion matrix
table(pred, y)

