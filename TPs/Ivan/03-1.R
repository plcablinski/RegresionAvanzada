# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# Instalar y cargar la librer?a necesaria
library(Brq)
library(readxl)
require("data.table")
library(ggplot2)
library(dplyr)
library(MVN)
library(aod)
library(lmtest) 
library(MASS)
library(car)
library(nortest)
library(ggplot2)
library(GGally)

library(MPV)
library(olsrr)
require(leaps) 

####################
## Load
####################

data("table.b3")
head(table.b3)




####################
## a) modelo saturado
####################

# Ajustar el modelo saturado
modelo_saturado <- lm(y ~ ., data = table.b3)
summary(modelo_saturado)



####################
## b) VIF
###################

# Calcular el VIF para el modelo saturado
vif(modelo_saturado)



####################
## c) Akaike
###################
# c) Realizar una seleccion de variables foward teniendo en cuenta el criterio
# de Akaike.
model <- lm(y ~ ., data = table.b3)
ols_step_forward_aic(model)
k <- ols_step_forward_aic(model)
plot(k)
# selection metrics
k$metrics
# extract final model
k$model
modelaso <- k$model
summary(modelaso)


# # Selección de variables forward
# modelo_forward <- stepAIC(modelo_saturado, direction = "forward")
# # Resumen del modelo forward
# summary(modelo_forward)





####################
## d) analisis diagnostico
###################

# Escribir la expresion del modelo seleccionado. Realizar un analisis diagnostico del mismo.
# Expresión del modelo seleccionado
formula(k$model)    # y ~ x1 + x4

# Análisis diagnóstico
par(mfrow = c(2, 2))
plot(k$model)


residuos=residuals(modelaso)

# Normalidad
shapiro.test(residuos)         # p-value = 0.501       Es normal
ad.test(residuos)              # p-value = 0.408       Es normal
lillie.test(residuos)          # p-value = 0.643       Es normal
qqPlot(residuos, pch=19,
       main="QQplot para los residuos del Modelo Lineal",
       xlab="Cuantiles teóricos",
       ylab="Cuantiles muestrales")

# Viendo el grafico vemos que hay dos puntos que atentan contra la normalidad: 12 y 15.




# Independencia
dwtest(modelaso,alternative
       ="two.sided",iterations = 1000)            # p-value = 0.951  Hay independencia



# Homocedasticidad
bptest(modelaso)                                  # p-value = 0.0350    Se rechaza homocedasticidad.
gqtest(modelaso, data=table.b3)                   # p-value = 0.9045    Se rechaza homocedasticidad.


ajustados=modelaso$fitted.values
databp=data.frame(ajustados,residuos)
ggplot(databp,aes(x=ajustados,y=residuos))+
  geom_point(color = "#013ADF", fill = "#013ADF", size = 4,
             shape = 18, alpha = 0.5)+xlab("ajustados")+
  geom_abline(slope = 0,linetype="dashed")

# No hay estructura en el grafico, entonces hay homocedasticidad.





####################
## e) backward y r2 ajustado
###################


# Realizar una seleccion backward teniendo en cuenta el criterio de R2
# ajustado. Se selecciona el mismo modelo?


mejores_modelos_backward <- regsubsets(y ~ ., data = table.b3, nvmax = 10, method = "backward") 
summary(mejores_modelos_backward)
# se identifica el valor m?ximo de R ajustado 
which.max(summary(mejores_modelos_backward)$adjr2)

coef(object = mejores_modelos_backward, 3)

# y  ~ x5 + x8 + x10
# Conclusion: No se selecciona el mismo modelo







####################
## f) ols_step_all_possible
###################

# Utilizando la funcion ols_step_all_possible de la biblioteca olsrr creada
# por Hebbali (2020) obtener todas las regresiones posibles. Elegir un unico modelo visualizando graficamente 
# los resultados y considerando los criterios BIC, AIC, CP y R2 adj .


lm.fit1 <- lm(y ~ ., data = table.b3)
k <- ols_step_all_possible(lm.fit1)
k
summary(k)
plot(k)



k_best <- ols_step_best_subset(lm.fit1)
k_best



# ---------- VER BIEN ESTA PARTE