# Regressão Linear Múltipla

# Utilizando o conjunto de dados Auto.csv

# Produza um gráfico de dispersão para todas as variáveis no conjunto de
# dados.

# Calcule a matriz de correlações entre as variáveis. Você precisará
# excluir a variável name, cor(), que é qualitativa.

# Execute uma regressão linear múltipla com mpg como resposta e todas as
# outras variáveis, exceto name, como preditores. Comente os resultados
# encontrados (por exemplo: (i) Existe uma relação entre os preditores e a
# resposta? (ii) Quais preditores parecem ter uma relação estatisticamente
# signicativa com a resposta? (iii) O que o coeficiente para a variável year
# sugere?


# Tente algumas transformações diferentes das variáveis, como $log(X)$, $X^2$.
# Comente sobre suas descobertas.

# Setup -------------------------------------------------------------------
rm(list=ls())
library(data.table)



# Data Load ---------------------------------------------------------------

tbl <- fread("C:/Users/bteba/OneDrive/Educacao/Aulas e Monitorias Lecionadas/01_Cursos/Master/Tecnicas de Machine Learning/03-Database/Lista de exercicio/Auto.csv")

# ajuste da coluna Orign para fatores
tbl$origin <- factor(tbl$origin, levels = c(1,2,3), labels = c("EUA", "EUR", "JAP"))

# estrutura da tabela
str(tbl)

# estatisticas descritivas
summary(tbl)



# Graficos de dispersao ---------------------------------------------------

plot(tbl[, .(mpg, cylinders, displacement, horsepower, weight, acceleration, year)])


# Matriz de correlacao ----------------------------------------------------

cor(tbl[, .(mpg, cylinders, displacement, horsepower, weight, acceleration, year)])


# OLS ---------------------------------------------------------------------

# Execute uma regressão linear múltipla com mpg como resposta e todas as
# outras variáveis, exceto name, como preditores. Comente os resultados
# encontrados (por exemplo: (i) Existe uma relação entre os preditores e a
# resposta? (ii) Quais preditores parecem ter uma relação estatisticamente
# signicativa com a resposta? (iii) O que o coeficiente para a variável year
# sugere?)

mdl <- lm(mpg ~ -1 + cylinders + displacement + horsepower + weight + acceleration + year + origin, data = tbl)
summary(mdl)

# (i) Existe uma relação entre os preditores e a resposta? (ii) Quais preditores
# parecem ter uma relação estatisticamente signicativa com a resposta?
#
# R: Com excessao dos regressores acceleration e cylinders, todos os outros
# apresentaram relação estatisticamente significante (a 5% de significancia).

# (iii) O que o coeficiente para a variável year sugere?
#
# R: O coeficiente da variavel year sugere que carros fabricados mais
# recentemente possuem um mpg melhor do que carros antigos. Evidenciando que
# temos melhorias tecnologicas.


# Transformation ----------------------------------------------------------


mdl.2 <- lm(log(mpg) ~ -1 + cylinders + displacement + horsepower + weight + 
              acceleration + year + origin, data = tbl)
summary(mdl.2)

# A analise dos graficos de dispersão indicam que mpg parece ter um
# comportamento exponencial. Para controlar esse efeito vamos aplicar o log
# nessa variavel; Ao se aplicar o log verifica-se que o coeficiente de numero de
# cilindros passou a ser significante.
