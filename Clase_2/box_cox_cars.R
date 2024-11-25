# limpiar las variables
rm(list=ls())

library(car)
library(MASS)
library(ggplot2)


data('cars')
cars
summary(cars)
# hacer un gráfico de dispersión de las variables x= speed e y=dist
ggplot(cars, aes(x=speed, y=dist)) + geom_point() + labs(title='Relación entre velocidad y distancia', x='Velocidad', y='Distancia')
mod_cars <- lm(dist ~ speed, data=cars)
summary(mod_cars)
# Hacer un gráfico de dispersion donde x son los valores ajustados y en y los residuos
ggplot(cars, aes(x=fitted(mod_cars), y=resid(mod_cars))) + geom_point() + labs(title='Residuos vs valores ajustados', x='Valores ajustados', y='Residuos')

# testear normalidad de los residuos
shapiro.test(mod_cars$residuals)
# testear homocedasticidad
ncvTest(mod_cars)
#testear independencia
dwtest(mod_cars)

boxcox(mod_cars)
# Realizar la transformación de Box-Cox y obtener el mejor valor de lambda
bc <- boxcox(mod_cars, plotit = FALSE)
lambda_optimo <- bc$x[which.max(bc$y)]
lambda_optimo

mod_cars2 <- lm(dist**.5 ~ speed, data=cars)
summary(mod_cars2)
qqPlot(mod_cars2)
mod_cars3 <- lm(dist**.4306 ~ speed, data=cars)
summary(mod_cars3)
qqPlot(mod_cars3)
# Hacer un gráfico de dispersion donde x son los valores ajustados y en y los residuos
ggplot(cars, aes(x=fitted(mod_cars2), y=resid(mod_cars2))) + geom_point() + labs(title='Residuos vs valores ajustados', x='Valores ajustados', y='Residuos')
# testear normalidad de los residuos
shapiro.test(mod_cars2$residuals)
# testear homocedasticidad
ncvTest(mod_cars2)
#testear independencia
dwtest(mod_cars2)
