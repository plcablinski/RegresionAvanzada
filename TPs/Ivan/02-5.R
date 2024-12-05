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
archivo = "datos_fifa.csv"
# ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
ruta = "c:\\Users\\iparra\\Downloads\\Austral\\RegAvanz\\datasets\\RegresionAvanzada-main\\"
dataset<-fread(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
#attach(dataset)
dt <- dataset
fifa_data <- dt
# Verificar la estructura de los datos
str(dt)


####################
## a) Visualizar relacion
####################


# Visualizar la relación entre Overall y Valor
ggplot(fifa_data, aes(x = Overall, y = Valor)) +
  geom_point() +
  labs(title = "Relación entre Overall y Valor", x = "Overall", y = "Valor")



####################
## b) Ajustar un modelo lineal simple
####################

# Ajustar el modelo lineal simple
modelo_lineal <- lm(Valor ~ Overall, data = fifa_data)

# Resumen del modelo
summary(modelo_lineal)

# Visualizar la línea de regresión
ggplot(fifa_data, aes(x = Overall, y = Valor)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Modelo Lineal Simple", x = "Overall", y = "Valor")



####################
## c) Ajustar un modelo polinomico
####################

# Modelo polinómico de grado 2
modelo_poli2 <- lm(Valor ~ poly(Overall, 2), data = fifa_data)
summary(modelo_poli2)

# Modelo polinómico de grado 3
modelo_poli3 <- lm(Valor ~ poly(Overall, 3), data = fifa_data)
summary(modelo_poli3)

# Modelo polinómico de grado 4
modelo_poli4 <- lm(Valor ~ poly(Overall, 4), data = fifa_data)
summary(modelo_poli4)



####################
## d) Metrica RMSE y conjunto de validacion
####################
# Dividir los datos en conjunto de entrenamiento y validación
set.seed(123)
sample_index <- sample(1:nrow(fifa_data), 0.7 * nrow(fifa_data))
train_data <- fifa_data[sample_index, ]
validation_data <- fifa_data[-sample_index, ]

# Calcular RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Evaluar el modelo lineal simple
pred_lineal <- predict(modelo_lineal, newdata = validation_data)
rmse_lineal <- rmse(validation_data$Valor, pred_lineal)

# Evaluar el modelo polinómico de grado 2
pred_poli2 <- predict(modelo_poli2, newdata = validation_data)
rmse_poli2 <- rmse(validation_data$Valor, pred_poli2)

# Evaluar el modelo polinómico de grado 3
pred_poli3 <- predict(modelo_poli3, newdata = validation_data)
rmse_poli3 <- rmse(validation_data$Valor, pred_poli3)

# Evaluar el modelo polinómico de grado 4
pred_poli4 <- predict(modelo_poli4, newdata = validation_data)
rmse_poli4 <- rmse(validation_data$Valor, pred_poli4)

# Comparar los RMSE
rmse_results <- data.frame(
  Model = c("Lineal", "Polinómico Grado 2", "Polinómico Grado 3", "Polinómico Grado 4"),
  RMSE = c(rmse_lineal, rmse_poli2, rmse_poli3, rmse_poli4)
)
print(rmse_results)






####################
## e) Analisis diagnostico
####################

# Análisis diagnóstico del modelo lineal simple
par(mfrow = c(2, 2))
plot(modelo_lineal)

# Análisis diagnóstico del modelo polinómico de grado 2
par(mfrow = c(2, 2))
plot(modelo_poli2)

# Análisis diagnóstico del modelo polinómico de grado 3
par(mfrow = c(2, 2))
plot(modelo_poli3)

# Análisis diagnóstico del modelo polinómico de grado 4
par(mfrow = c(2, 2))
plot(modelo_poli4)


