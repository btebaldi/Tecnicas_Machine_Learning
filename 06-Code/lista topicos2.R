# FROM: https://rstudio-pubs-static.s3.amazonaws.com/587609_9d4369c85be743eaa0b56cbaefff5c3e.html#content

# Setup -------------------------------------------------------------------
rm(list = ls())
library(data.table)
library(factoextra)
library(FactoMineR)


# Data load ---------------------------------------------------------------


# Carregue o conjunto de dados Multistat2.txt
Multistat2txt <- fread("C:/Users/bteba/Downloads/Part 4 Exercise 1 Principal Component Analysis, PCA.csv")
str(Multistat2txt)


# Selecione apenas as variáveis aids\_rt, babymort, birth\_rt, death\_rt,
# density, fertilty, gdp\_cap, lifeexpf, lifeexpm, literacy, pop\_incr,
# populatn, urban para a análise utilizando componentes principais.
Dataset = Multistat2txt[, .(aids_rt, babymort, birth_rt, death_rt, density, fertilty,
                            gdp_cap, lifeexpf, lifeexpm, literacy, pop_incr, populatn, urban)]


# Verifique se os dados tem o mesmo nível de variância. Isso pode ser feito
# usando a função 'boxplot´.
boxplot(Dataset)

# Utilize a função `prcomp()` para calcular os componentes principais
# considerando as variáveis socioeconômicas. Gere um sumário dos componentes
# principais.
PCA.model <- prcomp(Dataset, center = TRUE, scale = TRUE)
summary(PCA.model)

# Construa um \textit{scree plot} para visualizar a variância explicada por cada
# componente.
fviz_screeplot(PCA.model, addlabels = TRUE, ylim = c(0, 70))

# Quantos componentes devem ser utilizados se queremos pelo menos um modelo com
# 85\% da variação?
get_eig(PCA.model)
Dataset_PC <<- within(Multistat2txt, {
  PC4 <- PCA.model$x[, 4]
  PC3 <- PCA.model$x[, 3]
  PC2 <- PCA.model$x[, 2]
  PC1 <- PCA.model$x[, 1]
})


p <- ggplot(Dataset_PC) + aes(x = PC1, y = PC2, colour = religion, label = country) + 
  geom_point(size = 2L) + scale_color_hue() + theme_light()

p + geom_text(vjust = -1, size = 2)


# Exiba as cargas fatoriais (loadings) associadas a cada variável.  
PCA.model$rotation


# Gere uma representação gráfica dos dois primeiros componentes principais com a
# projeção de cada variável.
fviz_contrib(PCA.model, choice = "var", axes = 1) 
fviz_contrib(PCA.model, choice = "var", axes = 2) 
fviz_pca_var(PCA.model, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                                                               "#FC4E07"), repel = TRUE)



