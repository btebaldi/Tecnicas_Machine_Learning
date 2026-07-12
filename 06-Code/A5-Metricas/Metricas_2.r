# Setup -------------------------------------------------------------------

rm(list=ls())
library(dplyr)

iris$Species2 <- factor(iris$Species,
                        levels = c("setosa", "versicolor", "virginica"),
                        labels = c(1, 0, 0) )

iris$Species2 <- if_else(iris$Species == "setosa", 1, 0)

is.factor(iris$Species2)

lgi.mdl <- glm(Species2 ~ Sepal.Length,
               data = iris,
               family = binomial(link = "logit"))
plot(iris)
summary(lgi.mdl)

iris$pred <- if_else(predict(lgi.mdl, type = "response") > 0.5, 1, 0)

CM <- table(iris$pred, iris$Species2)

Acc <- sum(diag(CM))/sum(CM)


library(caret)
caret::confusionMatrix(CM)


library(ROCR)
previsto_obj <- ROCR::prediction(predictions = predict(lgi.mdl, type = "response"),
                             labels = iris$Species2)
summary(previsto_obj)

previsto_obj

perf_obj <- ROCR::performance(previsto_obj, measure = "tpr", x.measure = "fpr")

plot(perf_obj, main = "ROC Curve Example (ROCR)", col = "darkgreen", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
