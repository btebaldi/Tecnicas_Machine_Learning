# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)
library(dplyr)
library(glmnet)


# Data Load ---------------------------------------------------------------
data <- read.csv('./database/WB5 - Meticas - dados_compras.csv')

set.seed(12345)

trainIndex <- sample(1:nrow(data), size = floor(0.8*nrow(data)))
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# Treinar o modelo de regressão linear
modelo <- lm(valor_total_compras ~ idade + renda_anual + compras_anuais, data = trainData)
summary(modelo)

# Fazer previsões
# previsoes <- predict(modelo, newdata = testData, type = "response")
previsoes <- predict(modelo, newdata = testData)

# Valores reais
valores_reais <- testData$valor_total_compras

# Calcular métricas
mae <- mean(abs(valores_reais - previsoes))
mse <- mean((valores_reais - previsoes)^2)
rmse <- sqrt(mse)
r_squared <- summary(modelo)$r.squared
adjusted_r_squared <- summary(modelo)$adj.r.squared
mape <- mean(abs((valores_reais - previsoes) / valores_reais)) * 100

resp = c("MAE" = mae, "MSE" = mse, "RMSE" = rmse, "R2" = r_squared, "R2 Ajus." = adjusted_r_squared, "MAPE" = mape)

# Imprimir métricas
cat("\n", sprintf("%8s: %10.4f\n", names(resp), resp))
# MAE:   797.3787
# MSE: 1031878.0472
# RMSE:  1015.8140
# R2:     0.9012
# R2 Ajus.:     0.9009
# MAPE:     3.2322
