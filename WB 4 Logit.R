# Setup -------------------------------------------------------------------
rm(list = ls())

library(readr)
library(dplyr)
library(glmnet)


# Data load ---------------------------------------------------------------
tbl <- read_delim("./database/WB4 - DirectMail.csv", 
                          delim = ",",
                          escape_double = FALSE,
                          trim_ws = TRUE)


mdl <- glm(resp ~ AGE + CRED + INCOME + MILEAGE + MRTGI + GENDER, family = binomial("logit"), data = tbl)
summary(mdl)
nrow(tbl2)

write.table(x = tbl2, file = "../../temp/Aula 4.csv", sep = ",", quote = FALSE, dec = ".", row.names = FALSE)

tbl <- read.csv("../../temp/Aula 4.csv")
mdl <- glm(resp ~ AGE + CRED + INCOME + MILEAGE + MRTGI + GENDER, family = binomial("logit"), data = tbl2)
summary(mdl)

