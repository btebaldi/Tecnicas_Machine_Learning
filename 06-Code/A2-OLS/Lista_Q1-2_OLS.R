# Boston Data

# Utilizando o conjunto de dados \textbf{Bonston.csv}, construa um modelo para
# prever a taxa de criminalidade per capita usando as variáveis fornecidas neste
# conjunto de dados.

# Para cada preditor, ajuste um modelo de regressão linear simples para prever a
# resposta. Descreva seus resultados. Em qual dos modelos há uma associação
# estatisticamente significativa entre o preditor e a resposta? Crie alguns
# gráficos para respaldar suas afirmações.
#
# Ajuste um modelo de regressão múltipla para prever a resposta usando todos os
# preditores. Descreva seus resultados. Para quais preditores podemos rejeitar a
# hipótese nula $H_0 : \beta_j = 0$?
#
# Como seus resultados de (\ref{part_a}) se comparam aos seus resultados de
# (\ref{part_b})?
#
# Há evidência de associação não linear entre qualquer um dos preditores e a
# resposta? Para responder a esta pergunta, para cada preditor X, ajuste um
# modelo da forma $Y = \beta_0 + \beta_1 X + \beta_2 X_2 + \beta_0 X_3 +
# \epsilon$. \end{parts}


# Setup -------------------------------------------------------------------
rm(list=ls())
library(data.table)



# Data Load ---------------------------------------------------------------

tbl <- fread("C:/Users/bteba/OneDrive/Educacao/Aulas e Monitorias Lecionadas/01_Cursos/Master/Tecnicas de Machine Learning/03-Database/Lista de exercicio/Boston.csv")

# estrutura da tabela
str(tbl)

# estatisticas descritivas
summary(tbl)


# Para cada preditor, ajuste um modelo de regressão linear simples para prever a
# resposta. Descreva seus resultados. Em qual dos modelos há uma associação
# estatisticamente significativa entre o preditor e a resposta? Crie alguns
# gráficos para respaldar suas afirmações.

plot(tbl)
for(col in colnames(tbl)){
  if(col =="crim"){
    next()
  }
  
  formula <- sprintf("crim ~ %s", col)
  print(formula)
  mdl <- lm(formula = formula, data = tbl)
  print(summary(mdl))
}


# Ajuste um modelo de regressão múltipla para prever a resposta usando todos os
# preditores. Descreva seus resultados. Para quais preditores podemos rejeitar a
# hipótese nula $H_0 : \beta_j = 0$?

mdl <- lm(crim ~ ., data = tbl)
print(summary(mdl))

# Há evidência de associação não linear entre qualquer um dos preditores e a
# resposta? Para responder a esta pergunta, para cada preditor X, ajuste um
# modelo da forma $Y = \beta_0 + \beta_1 X + \beta_2 X_2 + \beta_0 X_3 +
# \epsilon$
plot(tbl)
for(col in colnames(tbl)){
  if(col =="crim"){
    next()
  }
  
  formula <- sprintf("crim ~ %s + I(%s^2) + I(%s^3)", col, col, col)
  print(formula)
  mdl <- lm(formula = formula, data = tbl)
  print(summary(mdl))
}


