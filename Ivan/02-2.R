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
library(nortest)
library(ggplot2)
library(GGally)

####################
## Load
####################
archivo = "publicidad.xlsx"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-read_excel(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
attach(dataset)
dt <- as.data.table(dataset)




####################
## a) Modelo lineal simple
####################


# Ajustar modelos de regresión lineal simples
model_radio <- lm(ventas ~ radio, data = dt)
model_tv <- lm(ventas ~ tv, data = dt)
model_newspaper <- lm(ventas ~ periodico, data = dt)

# Resúmenes de los modelos
summary(model_radio)          # R=0.32 coefic. significativos.
summary(model_tv)             # R=0.60 coef. significativos.
summary(model_newspaper)      # R=0.04 coef. significtivos.

# Diagnóstico de los modelos
par(mfrow = c(2, 2))
plot(model_radio)
plot(model_tv)
plot(model_newspaper)





####################
## b) Modelo Aditivo
####################


# Ajustar un modelo aditivo con las tres variables
model_full <- lm(ventas ~ radio + tv + periodico, data = dt)
summary(model_full)       # periodico es no significativa.

# Test de Wald para los coeficientes del modelo
wald.test(Sigma = vcov(model_full), b = coef(model_full),  Terms = 2:4)      # pvalue=0 ----> coeficiente sumamente significativo.





####################
## c) Seleccion de variables
####################


# Ajustar modelos de a pares
model_radio_tv <- lm(ventas ~ radio + tv, data = dt)
model_radio_newspaper <- lm(ventas ~ radio + periodico, data = dt)
model_tv_newspaper <- lm(ventas ~ tv + periodico, data = dt)

# Resúmenes de los modelos
summary(model_radio_tv)
summary(model_radio_newspaper)
summary(model_tv_newspaper)

# Comparar AIC
AIC(model_radio_tv, model_radio_newspaper, model_tv_newspaper)
#                       df       AIC
# model_radio_tv         4  780.3941
# model_radio_newspaper  4 1154.4723
# model_tv_newspaper     4 1027.7782



# Comparar R² ajustado
adj_r_squared_radio_tv <- summary(model_radio_tv)$adj.r.squared                 # 0.896
adj_r_squared_radio_newspaper <- summary(model_radio_newspaper)$adj.r.squared   # 0.325
adj_r_squared_tv_newspaper <- summary(model_tv_newspaper)$adj.r.squared         # 0.642



# Comparar Cp de Mallows
cp_mallows <- function(model) {
  p <- length(coef(model))
  n <- length(model$residuals)
  rss <- sum(residuals(model)^2)
  sigma2 <- sum((model$residuals)^2) / (n - p)
  cp <- rss / sigma2 - n + 2 * p
  return(cp)
}

cp_mallows_radio_tv <- cp_mallows(model_radio_tv)                     # 3
cp_mallows_radio_newspaper <- cp_mallows(model_radio_newspaper)       # 3
cp_mallows_tv_newspaper <- cp_mallows(model_tv_newspaper)             # 3




####################
## d) Plano de ajuste
####################


# Suponiendo que el modelo con Radio y TV fue el mejor
library(scatterplot3d)

# Graficar el plano de ajuste
s3d <- scatterplot3d(dt$radio, dt$tv, dt$ventas, pch = 16, highlight.3d = TRUE,
                     xlab = "Radio", ylab = "TV", zlab = "Ventas")

# Añadir el plano de ajuste
s3d$plane3d(model_radio_tv, lty.box = "solid")




####################
## e) Modelos con y sin interaccion
####################

# Ajustar modelo con interacción
model_interaction <- lm(ventas ~ radio * tv, data = dt)
summary(model_interaction)

# Comparar modelos con y sin interacción
anova(model_radio_tv, model_interaction)

# El model_interaccion es mejor porque presenta un pvalue mas significativo en anova.





# Conclusiones:
# Modelos simples: Ajustamos modelos de regresión lineal simples para cada predictor (Radio, TV, Newspaper) y realizamos diagnósticos para detectar problemas potenciales.
# Modelo aditivo: Ajustamos un modelo con todas las variables predictoras y realizamos un test de Wald para evaluar la significancia de cada predictor.
# Modelos de a pares: Ajustamos modelos que incluyen pares de variables predictoras y seleccionamos el mejor utilizando AIC, R² ajustado y Cp de Mallows.
# Plano de ajuste: Graficamos el plano de ajuste para el modelo seleccionado y evaluamos su adecuación.
# Modelo con interacción: Ajustamos un modelo con interacción entre las variables y comparamos su rendimiento con el modelo sin interacción utilizando un test de ANOVA.


