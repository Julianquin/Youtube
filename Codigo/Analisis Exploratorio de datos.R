######################################################
#########  Analisis Exploratorio de datos  ###########
######################################################
setwd("D:/Tematicas/Nueva carpeta/vacaciones")
fifa17 <- read.csv("FullData.csv",header = T, sep = ";")
str(fifa17)

datos <- fifa17[1:100,]
attach(datos)
summary(Rating)
sd(Rating)
hist(Rating)
plot(density(Rating))
boxplot(Rating)
qqnorm(Rating)
qqline(Rating)
plot(Altura,Rating)
plot(Peso,Rating)
plot(Reaccion,Rating)
cor(Altura,Rating)
cor(Peso,Rating)
cor(Reaccion,Rating)
