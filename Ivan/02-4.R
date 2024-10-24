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

# Cargar el conjunto de datos Boston
data(Boston)

# Ver la estructura del conjunto de datos
str(Boston)


####################
## a) Ajustar los modelos de regresión polinómica
####################

# Modelo lineal simple
model_linear <- lm(medv ~ lstat, data = Boston)

# Modelo polinómico de grado 2
model_poly2 <- lm(medv ~ poly(lstat, 2), data = Boston)

# Modelo polinómico de grado 5
model_poly5 <- lm(medv ~ poly(lstat, 5), data = Boston)

# Modelo polinómico de grado 10
model_poly10 <- lm(medv ~ poly(lstat, 10), data = Boston)



####################
## b) Comparar los modelos utilizando el criterio de R²
####################


# Obtener los valores de R² para cada modelo
r2_linear <- summary(model_linear)$r.squared        # 0.54
r2_poly2 <- summary(model_poly2)$r.squared          # 0.64
r2_poly5 <- summary(model_poly5)$r.squared          # 0.68
r2_poly10 <- summary(model_poly10)$r.squared        # 0.68  





####################
## c) Añadir otra variable al modelo seleccionado
####################


# Modelo polinómico de grado 5 con la variable rm
model_poly5_rm <- lm(medv ~ poly(lstat, 5) + rm, data = Boston)

# Obtener el valor de R² para el nuevo modelo
r2_poly5_rm <- summary(model_poly5_rm)$r.squared

# Imprimir el valor de R² del nuevo modelo
cat("R² del modelo polinómico de grado 5 con rm: ", r2_poly5_rm, "\n")    # 0.72

