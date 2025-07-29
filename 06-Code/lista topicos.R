rm(list = ls())
# USArrests
# class(USArrests)
# colnames(USArrests)

y =rep(NA, 10)
for(i in 2:10){
  mKmeans <- kmeans(x = data.matrix(USArrests), centers = i)
  y[i] =  mKmeans$tot.withinss
}

plot(1:10, y)


library(forcats)
mKmeans <- kmeans(x = data.matrix(USArrests), centers = 3)
USArrests$class <- factor(mKmeans$cluster)
USArrests$state <- fct_infreq(f = rownames(USArrests), w = USArrests$Assault)

factor(rownames(USArrests),
       labels = rownames(USArrests),
       # levels = USArrests$Murder,
       ordered = TRUE)

library(ggplot2)


ggplot(USArrests) +
  geom_point(aes(y=Assault, x = Murder, colour = class))

ggplot(USArrests) +
  geom_point(aes(y=Assault, x = UrbanPop, colour = class))

ggplot(USArrests) +
  geom_point(aes(y=Assault, x = Rape, colour = class))


ggplot(USArrests) +
  geom_point(aes(y=UrbanPop, x = Murder, colour = class))

ggplot(USArrests) +
  geom_point(aes(y=UrbanPop, x = Rape, colour = class))

ggplot(USArrests) +
  geom_point(aes(y=Murder, x = Rape, colour = class))



USArrests$state <- fct_infreq(f = rownames(USArrests), w = USArrests$Murder)
ggplot(USArrests) +
  geom_col(aes(y=state, x = Murder, fill=class,group = class))

USArrests$state <- fct_infreq(f = rownames(USArrests), w = USArrests$Assault)
ggplot(USArrests) +
  geom_col(aes(y=state, x = Assault, fill=class,group = class))

USArrests$state <- fct_infreq(f = rownames(USArrests), w = USArrests$UrbanPop)
ggplot(USArrests) +
  geom_col(aes(y=state, x = UrbanPop, fill=class,group = class))

USArrests$state <- fct_infreq(f = rownames(USArrests), w = USArrests$Rape)
ggplot(USArrests) +
  geom_col(aes(y=state, x = Rape, fill=class,group = class))


