# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection



library(readxl)
require("data.table")
library(ggplot2)
library(dplyr)
library(MVN)
library(aod)
library(lmtest) 
library(MASS)
library(car)
library(carData)
library(nortest)
library(ggplot2)
library(GGally)



####################
## Load
####################
archivo = "crime.csv"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-fread(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
#attach(dataset)
dt <- dataset
crime_data <- dt
head(crime_data)
str(crime_data)
#summary(crime_data)



####################
## a) modelo OLS
####################


# Ajustar el modelo de regresión OLS
model_ols <- lm(crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data)
summary(model_ols)
# <<< ¿Hay que sacar sid? >>>>>


# Análisis diagnóstico: leverage y distancias de Cook
par(mfrow = c(2, 2))
plot(model_ols)



# <<< agregar aca analisis de supuestos >>>>>





# Valores de leverage y distancias de Cook
leverage <- hatvalues(model_ols)
cooksd <- cooks.distance(model_ols)

# Gráfico de leverage y distancias de Cook
plot(leverage, type = "h", main = "Leverage")
abline(h = 2*mean(leverage), col = "red")

plot(cooksd, type = "h", main = "Distancias de Cook")
abline(h = 4/nrow(crime_data), col = "red")



# Modelo OLS: El modelo de regresión OLS nos proporciona una primera impresión 
# de la relación entre las variables predictoras y la tasa de crímenes violentos.
# Sin embargo, el análisis diagnóstico sugiere la presencia de observaciones influyentes 
# que podrían distorsionar los resultados.




####################
## b) modelo OLS sin influyentes
####################


# Identificar observaciones influyentes
influential <- as.numeric(names(cooksd)[cooksd > 4/nrow(crime_data)])
influential           #  1  9 11 25 51

# Ajustar modelo sin observaciones influyentes
crime_data_no_influential <- crime_data[-influential, ]
model_ols_no_influential <- lm(crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data_no_influential)
summary(model_ols_no_influential)       # R = 0.82 

# Comparar coeficientes
coef(model_ols)
coef(model_ols_no_influential)

#<<<<< ¿que se puede decir "comparar los coeficientes de ambos modelos?" >>>>>>>


# Observaciones Influyentes: Identificamos y eliminamos las observaciones influyentes.
# Ajustar el modelo sin estas observaciones cambió los coeficientes estimados, lo que indica 
# que estas observaciones tenían un impacto significativo en el modelo original.









####################
## c) valor absoluto residuos
####################

# Generar la variable con el valor absoluto de los residuos
crime_data$residuals_abs <- abs(residuals(model_ols))

# Ordenar los residuos en orden descendente
top_residuals <- crime_data[order(-crime_data$residuals_abs), ]

# Mostrar los diez residuos más altos
top_residuals[1:10, ]

# Conclusion 3 de los resiudos mas altos coinciden con los valores influyentes
# Son el 1, 9, 25.



# Residuos Altos: Los diez residuos más altos no necesariamente coinciden con las
# observaciones influyentes, pero proporcionan información adicional sobre las observaciones
# que no son bien explicadas por el modelo.



####################
## d) Modelo robusto IRLS - Minimos cuadrados ponderados iterados
####################

# Ajustar el modelo de regresión robusta usando pesos de Huber
# model_robust_huber <- rlm(crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data, psi = psi.huber)
model_robust_huber <- rlm(crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data, k2 = 1.345)
summary(model_robust_huber)
model_robust_huber$coeffcientes
rlm(formula = crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data, k2 = 1.345)




# Ajustamos un modelo robusto con la propuesta de LTS
model_robust_LTS <- lqs(crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data, method = "lts")
summary(model_robust_LTS)
lqs.formula(formula = crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data, method = "lts")


# Ajustamos un modelo robusto con la propuesta de LAD
library(quantreg)
model_robust_LAD <- rq(crime ~ murder + poverty + single + pctmetro + pctwhite, data = crime_data, tau = 0.5)
rq(formula = crime ~ murder + poverty + single + pctmetro + pctwhite, tau = 0.5, data = crime_data)





# Gráfico diagnóstico para el modelo robusto
par(mfrow = c(2, 2))
plot(model_robust_huber)



# Modelo de Regresión Robusta: Ajustar un modelo de regresión robusta con pesos de Huber
# y la función bicuadrada proporcionó estimaciones que son menos sensibles a las 
# observaciones influyentes. Los resultados de estos modelos pueden ser más confiables
# cuando hay datos atípicos o influencias significativas.








####################
## e)  Ajustar un modelo de regresión robusta con IRLS usando la función bicuadrada
####################


# Ajustar el modelo de regresión robusta usando la función bicuadrada
model_robust_bisquare <- rlm(violent ~ murder + poverty + single + metro + white + highschool, data = crime_data, psi = psi.bisquare)
summary(model_robust_bisquare)

# Gráfico diagnóstico para el modelo robusto con función bicuadrada
par(mfrow = c(2, 2))
plot(model_robust_bisquare)
