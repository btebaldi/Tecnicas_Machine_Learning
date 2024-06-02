
# Setup -------------------------------------------------------------------

rm(list = ls())
library(data.table)


# Variables ---------------------------------------------------------------
Jaquetas <- c(0, 1, 2, 3, 4, 6, 7, 8)
ni<- c(2, 7, 41, 146, 178, 108, 42, 13)

tbl <- data.table(Jaquetas, ni)


# Determinacao de variaveis -----------------------------------------------
n <-  sum(tbl$ni)

tbl[, fi := ni/sum(ni)]

tbl[, xi := Jaquetas]
tbl[, xi_fi := xi*fi]
mu <- sum(tbl$xi_fi)

tbl[, x_c := xi - mu]
tbl[, x_c2 := x_c^2]

tbl[, x_c2_fi := x_c2*fi]
sigma.pop <- sum(tbl$x_c2_fi)
sigma.amt <- sigma.pop * n/(n-1)

resp <- c("Media" = mu,
        "Variancia Populacional" = sigma.pop,
        "Variancia Amostral" = sigma.amt,
        "Desvio Padrao Populacional" = sqrt(sigma.pop),
        "Desvio Padrao Amostral" = sqrt(sigma.amt))
# Respostas ---------------------------------------------------------------

cat(sprintf("\t%30s:%9.4f\n", names(resp), resp))


