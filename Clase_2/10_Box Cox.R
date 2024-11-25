# limpiar el entorno de variables
rm(list = ls())
library(car)
library(stats)
library(datasets)
library(ggplot2)
library(lmtest)
library(nortest)
library(tseries)
library(lawstat)
library(MASS)
data(cars)
head(cars)
ggplot(data=cars, aes(x=speed, y=dist)) + geom_point() + geom_smooth(method="lm", se=TRUE)
mod_cars <- lm(dist ~ speed, data=cars)
summary(mod_cars)
 
ggpairs(cars)

residuos <- residuals(mod_cars)
ajustados <- fitted(mod_cars)

#Analizo normalidad de los residuos
shapiro.test(residuos)
#El p-valor es menor a 0.05, por lo que rechazo la normalidad de los residuos
# Realizar la prueba de Anderson-Darling
ad.test(residuos)
#El p-valor es menor a 0.05, por lo que rechazo la normalidad de los residuos

#Analizo la autocorrelación de los residuos
durbinWatsonTest(mod_cars)
#El p-valor es mayor a 0.05, por lo que no rechazo la independencia de los residuos
# Realizar la prueba de Breusch-Godfrey
bgtest(mod_cars)
#El p-valor es mayor a 0.05, por lo que no rechazo la independencia de los residuos
# Realizar la prueba de Ljung-Box
Box.test(residuos, type = "Ljung-Box")
#El p-valor es mayor a 0.05, por lo que no rechazo la independencia de los residuos

#Analizo la homocedasticidad de los residuos
bptest(mod_cars)
# El p-valor es mayor a 0.05, por lo que no rechazo la homocedasticidad de los residuos
# Realizar la prueba de Goldfeld-Quandt
gqtest(mod_cars)
# El p-valor es mayor a 0.05, por lo que no rechazo la homocedasticidad de los residuos
# Realizar la prueba de Harrison-McCabe
hmctest(mod_cars)




ggplot(data.frame(x = ajustados, y = residuos), aes(x = x, y = y)) + 
  geom_point() + 
  labs(title = 'Residuos vs Ajustados', x = 'Ajustados', y = 'Residuos') + 
  theme_bw()


# dibujar histograma de los residuos
hist(residuos, col = 'lightblue', border = 'black', main = 'Histograma de los residuos', xlab = 'Residuos')

qqPlot(residuos, pch = 19 , col = 'red4', main = 'QQplot para los residuos del modelo lineal', 
       xlab = 'Cuantiles teoricos', ylab = 'Cuantiles muestrales')

boxCox(object = mod_cars, plotit = TRUE)
summary(powerTransform(mod_cars))
bc <- (boxcox(mod_cars, lambda = seq(-2, 2, 0.1), plotit = FALSE))
lambda_optimo <- bc$x[which.max(bc$y)]
lambda_optimo

mod_cars2 <- lm(dist^0.5 ~ speed, data=cars)
summary(mod_cars2)

residuos2 <- residuals(mod_cars2)
ajustados2 <- fitted(mod_cars2)
shapiro.test(residuos2)
durbinWatsonTest(mod_cars2)
bptest(mod_cars2)

ggplot(data.frame(x = ajustados2, y = residuos2), aes(x = x, y = y)) + 
  geom_point() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(title = 'Residuos vs Ajustados', x = 'Ajustados', y = 'Residuos') + 
  theme_bw()

qqPlot(residuos2, pch = 19 , col = 'red4', main = 'QQplot para los residuos del modelo lineal', 
       xlab = 'Cuantiles teoricos', ylab = 'Cuantiles muestrales')
