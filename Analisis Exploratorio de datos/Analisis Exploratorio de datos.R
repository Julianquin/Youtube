######################################################
#########  Analisis Exploratorio de datos  ###########
######################################################
setwd("D:/Tematicas/Nueva carpeta/vacaciones")

fifa17 <- read.csv("Datos/FullData.csv",header = T, sep = ";")
str(fifa17)

datos <- fifa17[1:100,]

attach(datos)

# Variables continuas

summary(Rating)
sd(Rating)
x11();hist(Rating, xlab = "Rating", ylab = "Frecuencia", main = "Histograma de Rating")
x11();plot(density(Rating),xlab = "Rating", ylab = "Frecuencia", main = "Funcion de densidad")

x11();boxplot(Rating~Pierna.preferida,col = "bisque",lty=1,lwd = 2,pch="+")
title("Comparando Boxplot del Rating con respecto a la pierna preferida")

x11();par(mfrow=c(1,3))
plot(Altura,Rating, main = "Diagrama de dispersion Altura-Rating",lwd = 2, pch=15)
plot(Peso,Rating, main = "Diagrama de dispersion Peso-Rating",lwd = 2, pch=16)
plot(Reaccion,Rating, main = "Diagrama de dispersion Reaccion-Rating",lwd = 2, pch=17)

cor(Altura,Rating)
cor(Peso,Rating)
cor(Reaccion,Rating)


# Variables Categoricas

# Tablas de frecuencia

table(Posicion.Club)
table(Pierna.preferida)
table(Ritmo.de.trabajo)
t1 <- table(Pierna.preferida,Ritmo.de.trabajo)

install.packages("gmodels")
library("gmodels")
CrossTable(Ritmo.de.trabajo,Pierna.preferida,prop.r = F,prop.t = T,prop.c = F,prop.chisq = F)


# Grafico de barras
x11();plot(Posicion.Club, las=2) 

x11();plot(Ritmo.de.trabajo,Pierna.preferida)

x11();par(mar=c(8, 4, 4, 2))
barplot(t1,las=2) # horiz=T
