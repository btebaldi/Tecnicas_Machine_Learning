
# Setup -------------------------------------------------------------------

rm(list = ls())
library(data.table)


# Data Load ---------------------------------------------------------------

tbl <- fread("./database/WB2 - Carseats.csv")


# Execucao do OLS ---------------------------------------------------------

mdl <- lm(Sales ~ Price + CompPrice + Income + Advertising + Population + Age + Education + Urban + US, data = tbl)

summary(mdl)

