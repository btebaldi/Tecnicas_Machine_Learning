rm(list = ls())
library(ggplot2)

AirPassengers
plot(AirPassengers)
tbl <- as_tibble(x = AirPassengers)

tbl$Date <- seq(from= as.Date("1949-01-01"),
                to = as.Date("1960-12-01"),
                by = "month")

ggplot(tbl) + 
  geom_line(aes(x = Date, y = x))

tbl <- tbl %>% 
  mutate(t = row_number(),
         lnx = log(x))

ggplot(tbl) + 
  geom_line(aes(x = Date, y = lnx))



# Criando os lag ----------------------------------------------------------

for(i in 1:30){
  if(i==1){
    tbl[ , sprintf("lnx_%s", i)] = lag(tbl[ , "lnx"])
  } else {
  tbl[ , sprintf("lnx_%s", i)] = lag(tbl[ , sprintf("lnx_%s", i-1)])
  }
}

tbl2 <- tbl %>% na.omit()

y <- tbl2[, 4] %>% data.matrix()
x <- tbl2[, c(3,5:34)] %>% data.matrix()

# Rodando Lasso -----------------------------------------------------------
lambdas_to_try <- 10^seq(-4, 4, length.out = 100)


ridge_cv <- cv.glmnet(x, y,
                      alpha = 1,
                      lambda = lambdas_to_try,
                      standardize = FALSE,
                      nfolds = 7)



# Plot cross-validation results
plot(ridge_cv)

plot(ridge_cv$cvm)
ridge_cv$lambda


# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.min

log(10^-2.3)

# Ridge Regression - coeficientes, SSR e R2 -------------------------------
model <- glmnet(x, y, alpha = 1, lambda = 1.261857e-02, standardize = FALSE)


# Coeficientes do modelo
coef(model)

# Fitted values
y_hat <- predict(model, x)

# Sum of Squared Residuals
SSR <- t(y - y_hat) %*% (y - y_hat)
rsq_ridge <- cor(y, y_hat)^2

cat(sprintf("SSR: %5.3f\n R2: %5.3f", SSR, rsq_ridge))


# Ridge Regression - coeficientes vs lambda -------------------------------
res <- glmnet(x, y, alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")


mdl <- lm(lnx ~ t + lnx_12 - 1, data = tbl)
summary(mdl)

plot(mdl)


arima(x = tbl$lnx,
      xreg = tbl$t,
      order = c(0, 0, 0),
      seasonal = list(order=c(1,0,0), period = 12),
      include.mean = FALSE)



