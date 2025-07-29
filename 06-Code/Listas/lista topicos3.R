rm(list = ls())


# Carregar biblioteca MASS para utilizar LDA
library(MASS)

# Carregar o conjunto de dados Smarket (já incluído no R)
# data(Smarket)
# Smarket <- ISLR::Smarket
# data.table::fwrite(Smarket, file = "Smarket.csv")
Smarket <- data.table::fread("Smarket.csv")
# Criar um subconjunto de dados apenas com anos anteriores a 2005
train <- Smarket$Year < 2005

# Ajustar o modelo LDA usando Lag1 e Lag2 como preditores
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)


# Exibir os resultados do modelo
lda.fit

# Plotar os discriminantes lineares
plot(lda.fit)

# Fazer previsões para os dados de 2005
Smarket.2005 <- subset(Smarket, Year == 2005)
lda.pred <- predict(lda.fit, Smarket.2005)

# Exibir os elementos do objeto de previsão
names(lda.pred)

# Criar matriz de confusão comparando as previsões com os valores reais
lda.class <- lda.pred$class
table(lda.class, Smarket.2005$Direction)

# Calcular a precisão do modelo
mean(lda.class == Smarket.2005$Direction)

# Analisar as probabilidades posteriores
head(lda.pred$posterior)

# Modificar o limiar para prever uma queda apenas se a probabilidade posterior for maior que 90%
sum(lda.pred$posterior[, 1] > 0.9)

# Exibir a maior probabilidade posterior encontrada
max(lda.pred$posterior[, 1])

