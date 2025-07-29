
# Setup -------------------------------------------------------------------
rm(list = ls())
library(readr)
library(ggplot2)
library(dplyr)


# Dataload ----------------------------------------------------------------
DonnerParty <- read_csv("C:/Users/bteba/Documents/GitHub/Statistics/Data Analysis/database/DonnerParty.csv",
                        col_types = cols(
                          Age = col_double(),
                          Sex_Male = col_double(),
                          Survived = col_double()
                        ))
head(DonnerParty)

donner <- DonnerParty
survive=donner$Survived
age=donner$Age
sex=donner$Sex_Male


# Distribuicoes marginais -------------------------------------------------

table(DonnerParty$Survived, DonnerParty$Sex_Male,
      dnn = c("Survived", "Male"))

g1 <- DonnerParty %>% mutate(Survived=factor(Survived,
                                             levels = c(1,0),
                                             labels = c("Survived", "Died"))) %>% 
  ggplot() + 
  geom_boxplot(aes(y=Age, x=Survived)) + 
  theme_bw() + 
  labs(title = "Boxplot")
print(g1)
# ggsave("./Boxplot_DonnerParty.png",
#        plot = g1,
#        scale = 1,
#        units = "in", width = 11, height = 9, dpi = 100)

# Modelo Linear OLS -------------------------------------------------------

## linear regression
LM <- lm(Survived ~ Age, data = DonnerParty)
Ols.mdl <- lm(Survived ~ Age, data = DonnerParty)
summary(Ols.mdl)

CI.Ols.mdl <- confint(Ols.mdl)

DonnerParty$Ols.res <- Ols.mdl$residuals

### Plot Survive*Age

g2 <- ggplot(DonnerParty) + 
  geom_point(aes(x=Age, y=Survived)) +
  geom_abline(slope = Ols.mdl$coefficients["Age"],
              intercept = Ols.mdl$coefficients["(Intercept)"], colour="red") +
  geom_abline(slope = CI.Ols.mdl["Age", 1],
              intercept = CI.Ols.mdl["(Intercept)", 1], colour="red", linetype="dashed") +
  geom_abline(slope = CI.Ols.mdl["Age", 2],
              intercept = CI.Ols.mdl["(Intercept)", 2], colour="red", linetype="dashed") +
  theme_bw() +
  ylim(-1.5, 1.5)+
  labs(title = "Survive v.s. age")
print(g2)
# ggsave("./OlsModel_DonnerParty.png",
#        plot = g2,
#        scale = 1,
#        units = "in", width = 8, height = 6, dpi = 100)

### Q-Q plot
qqnorm(residuals(LM),main="Q-Q Plot")

### Studentized residuals v.s Observation
plot(rstudent(LM),main="Studentized residual v.s. observation")
abline(h=0)

g3 <- ggplot(DonnerParty) +
  stat_qq(mapping = aes(sample = Ols.res)) +
  stat_qq_line(mapping = aes(sample = Ols.res)) +
  theme_bw() +
  labs(title = "Q-Q Plot",
       subtitle = "Ols Model")
print(g3)
# ggsave("./OlsModel_QQ_DonnerParty.png",
#        plot = g3,
#        scale = 1,
#        units = "in", width = 8, height = 6, dpi = 100)


# Fitting logistic regression survive~age ---------------------------------

logit.mdl = glm(Survived ~ Age,family=binomial("logit"), data = DonnerParty)
summary(logit.mdl)

DonnerParty$log1.res <- logit.mdl$residuals

## to get the specific coefficient, this command will produce a vector
coefficients(logit.mdl)
## to access the estimated slope and turn it into the odds-ratio
exp(coefficients(logit.mdl)[2])

confint(logit.mdl) ## confidence interval for parameters
exp(confint(logit.mdl)) ## exponentiate to get on the odds-scale

### Diagnostics Measures

lm.influence(logit.mdl)


g4 <- ggplot(DonnerParty) +
  stat_qq(mapping = aes(sample = log1.res)) +
  stat_qq_line(mapping = aes(sample = log1.res)) +
  theme_bw() +
  labs(title = "Q-Q Plot",
       subtitle = "Ols Model")
print(g4)
# ggsave("./Logit1Model_QQ_DonnerParty.png",
#        plot = g4,
#        scale = 1,
#        units = "in", width = 8, height = 6, dpi = 100)


# fitting logistic regression survive~age+sex -----------------------------

logit2.mdl=glm(survive~age+sex,family=binomial("logit"))
summary(logit2.mdl)
confint(logit2.mdl) ## confidence interval for the parameters 

DonnerParty$log2.res <- logit2.mdl$residuals


### Diagnostics Measures

lm.influence(logit2.mdl)

g5 <- ggplot(DonnerParty) +
  stat_qq(mapping = aes(sample = log2.res)) +
  stat_qq_line(mapping = aes(sample = log2.res)) +
  theme_bw() +
  labs(title = "Q-Q Plot",
       subtitle = "Ols Model")
print(g5)
# ggsave("./Logit5Model_QQ_DonnerParty.png",
#        plot = g5,
#        scale = 1,
#        units = "in", width = 8, height = 6, dpi = 100)



# fitting logistic regression survive~age+sex+age*sex ---------------------

# logit3.mdl=glm(survive~age*sex,family=binomial("probit"))
logit3.mdl=glm(survive~age*sex,family=binomial("logit"))
summary(logit3.mdl)
confint(logit3.mdl) ## confidence interval for the parameters 

DonnerParty$log3.res <- logit3.mdl$residuals


g6 <- ggplot(DonnerParty) +
  stat_qq(mapping = aes(sample = log3.res)) +
  stat_qq_line(mapping = aes(sample = log3.res)) +
  theme_bw() +
  labs(title = "Q-Q Plot",
       subtitle = "Ols Model")
print(g6)

# ggsave("./Logit6Model_QQ_DonnerParty.png",
#        plot = g6,
#        scale = 1,
#        units = "in", width = 8, height = 6, dpi = 100)
# 






# Valores previstos podem ser calculados diretamente pelo predict usando a
# formula de distribuicao logistica
PREVISAO_manual <- exp(predict(logit3.mdl,  DonnerParty))/(1+exp(predict(logit3.mdl,  DonnerParty)))

# comparando para mostrar que a previsao manual é igual aos valores previstos
PREVISAO_manual == logit3.mdl$fitted.values

# Matriz de confusao
table(survive, as.numeric(logit3.mdl$fitted.values > 0.5))




