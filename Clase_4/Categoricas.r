rm(list = ls())
# Cargar librerías necesarias
library(tidyverse)

# Usar el conjunto de datos mtcars
data(mtcars)
head(mtcars)

# Crear una variable categórica a partir de la variable 'cyl' (número de cilindros)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
head(mtcars)
summary(mtcars)

# Ajustar un modelo de regresión lineal con predictores categóricos
model <- lm(mpg ~ cyl + wt + hp, data = mtcars)
model1 <- lm(mpg ~ cyl + vs + am + gear + carb, data = mtcars)
summary(model1)

# Resumen del modelo
summary(model)

# Definir una variable categórica con "B" como nivel 0
categorica <- factor(c("B", "A", "C", "A", "B", "C"), levels = c("B", "A", "C"))

# Mostrar la variable
print(categorica)

# Comprobar los niveles
levels(categorica)

library(carData)
library(dplyr)
levels(Salaries$sex)
str(Salaries)
mod1 <- lm(salary ~ sex, data = Salaries)
summary(mod1)
# Hacer que el nivel por defecto sea Male para sex
Salaries$sex <- relevel(Salaries$sex,ref='Female')
levels(Salaries$sex)
mod1 <- lm(salary ~ sex, data = Salaries)
summary(mod1)
contrasts(Salaries$sex)

mod3 <- lm(salary ~ rank, data = Salaries)
summary(mod3)

contr <- model.matrix(~ rank, data = Salaries)
head(contr[, -1])

mod4 <- lm(salary ~ rank + discipline , data = Salaries)
#clear terminal
cat("\014")
system("clear")
summary(mod4)
levels(Salaries$discipline)

contr <- model.matrix(~ rank + discipline, data = Salaries)
head(contr[, -1],100)
levels(Salaries$discipline)
levels(Salaries$rank)

mod5 <- lm(salary ~ yrs.since.phd + discipline, data = Salaries)
summary(mod5)

# mostrar los valores numéricos en formato entero con dos decimales
options(digits = 2)
library(readxl)
antro <- read_excel("antropom.xlsx")
head(antro)
# Convertir la variable sexo a factor
antro$Sexo <- factor(antro$Sexo)
antro$Sexo
mod6 <- lm(Peso ~ Estatura * Sexo + Edad_meses, data = antro)
summary(mod6)
