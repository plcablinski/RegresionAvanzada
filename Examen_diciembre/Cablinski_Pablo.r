rm(list=ls())
setwd('C:/RegresionAvanzada/Examen_diciembre')
library(readxl)
library(car)
library(lmtest)
library(leaps)
library(dplyr)
library(MASS)
library(caret)
library(ggplot2)
library(ResourceSelection)

# Cargamos los datos
datos <- read.table("salarios.csv", header = TRUE, sep = ";")
dim(datos)
str(datos)
# mostrar las columnas junto con el indice
for (i in 1:ncol(datos)) {
  cat(i, names(datos)[i], "\n")
}

# Eliminar de datos las siguientes columnas: northcen, ndurman,trcommpu, trade, services, profserv
datos <- datos[, -c(10, 14, 15, 16, 17, 18, 22,23,24)]
str(datos)


options(width=200)
# Por cada columna buscar datos faltantes
sapply(datos, function(x) sum(is.na(x)))

head(datos) 
dim(datos)
str(datos)

# Convertir a factores nonwhite, female, married, smsa, south, west, construc, clerocc, servocc,profocc
datos$nonwhite <- as.factor(datos$nonwhite)
datos$female <- as.factor(datos$female)
datos$married <- as.factor(datos$married)
datos$smsa <- as.factor(datos$smsa)
datos$ northcen <- as.factor(datos$northcen)
datos$south <- as.factor(datos$south)
datos$west <- as.factor(datos$west)
datos$construc <- as.factor(datos$construc)
datos$ndurman <- as.factor(datos$ndurman)
datos$trcommpu <- as.factor(datos$trcommpu)
datos$trade <- as.factor(datos$trade)
datos$services <- as.factor(datos$services)
datos$profserv <- as.factor(datos$profserv)
datos$profocc <- as.factor(datos$profocc)
datos$clerocc <- as.factor(datos$clerocc)
datos$servocc <- as.factor(datos$servocc)
str(datos)
dim(datos)
# a) Construya el mejor modelo lineal simple y realice el análisis diagnóstico del mismo. En caso de ser necesario transforme la variable respuesta. Concluya.

# Realizar la selección de variables usando regsubsets 
regfit <- regsubsets(salario ~ educ + exper + tenure + numpdep + expersq + tenuresq, data = datos, nvmax = 1, method = "exhaustive",nbest =50)
# Obtener el resumen del ajuste
reg_summary <- summary(regfit)
reg_summary

# Mostrar las variables seleccionadas y el R^2 ajustado para cada posible selección
for (i in 1:length(reg_summary$adjr2)) {
  selected_vars <- names(which(reg_summary$which[i, ]))
  adjr2 <- reg_summary$adjr2[i]
  cat("Modelo", i, ": Variables seleccionadas:", paste(selected_vars, collapse = ", "), "\n")
  cat("R^2 ajustado:", adjr2, "\n\n")
}

modelo_simple <- lm(salario ~ educ, data = datos)
summary(modelo_simple)

modelo_simple2 <- lm(salario ~ educ + exper + tenure + numdep , data = datos)
summary(modelo_simple2)

modelo_simple3 <- lm(salario ~ educ  , data = datos)
summary(modelo_simple3)

modelo_simple4 <- lm(salario ~ tenure  , data = datos)
summary(modelo_simple4)



# Análisis de diagnóstico
# Normalidad de los residuos
shapiro.test(modelo_simple$residuals)
# Rechazo normalidad
# Homocedasticidad
ncvTest(modelo_simple)
# Rechazo homocedasticidad
# Durbin-Watson para independencia
dwtest(modelo_simple)
par(mfrow=c(2,2))
plot(modelo_simple)

# Transformación de la variable respuesta
summary(powerTransform(salario ~ educ , data = datos))
modelo_transformado <- lm(bcPower(salario,-0.18) ~ educ, data = datos)
summary(modelo_transformado)
# Análisis de diagnóstico
# Normalidad de los residuos
shapiro.test(modelo_transformado$residuals)
# Rechazo normalidad
# Homocedasticidad
ncvTest(modelo_transformado)
# breusch pagan para homocedasticidad
bptest(modelo_transformado)
# No rechazo homocedasticidad
# Durbin-Watson para independencia
dwtest(modelo_transformado)
par(mfrow=c(2,2))
plot(modelo_transformado)

b)

library(leaps)

# Realizar la selección de variables usando regsubsets
str(datos)

# b) Mediante selección de variables elija el mejor modelo multivariado para explicar salario. 
# Estudie la presencia de multicolinealidad. Si la hubiera aplique alguna metodología para evitarla.
regfit <- regsubsets( salario ~ ., data = datos, nvmax = 50, method = "exhaustive")
# Obtener el resumen del ajuste
reg_summary <- summary(regfit)
reg_summary
# Mostrar las variables seleccionadas y el R^2 ajustado para cada posible selección
for (i in 1:length(reg_summary$adjr2)) {
  selected_vars <- names(which(reg_summary$which[i, ]))
  adjr2 <- reg_summary$adjr2[i]
  cat("Modelo", i, ": Variables seleccionadas:", paste(selected_vars, collapse = ", "), "\n")
  cat("R^2 ajustado:", adjr2, "\n\n")
}

# Identificar las variables seleccionadas en el modelo 16
selected_vars_model_16 <- names(which(reg_summary$which[16, ]))
selected_vars_model_16 <- selected_vars_model_16[selected_vars_model_16 != "(Intercept)"]

# Crear la fórmula del modelo
formula_model_16 <- as.formula(paste("salario ~", paste(selected_vars_model_16, collapse = " + ")))
formula_model_16

# Ajustar el modelo lineal
modelo_16 <- lm(formula_model_16, data = datos_sin_lwage)

# Resumen del modelo
summary(modelo_16)

modelo_seleccion <- lm(salario ~ educ + exper + tenure + female + smsa + northcen +
    south + west + ndurman + trcommpu + trade + services +
    profserv + profocc + expersq + tenursq, data = datos_sin_lwage) 
summary(modelo_seleccion)
vif(modelo_seleccion)
modelo_seleccion_1 <- lm(salario ~ educ + exper + tenure + female + smsa + northcen +
    south + west + ndurman + trcommpu + trade + services +
    profserv + profocc, data = datos_sin_lwage) 
vif(modelo_seleccion_1)
summary(modelo_seleccion_1)
modelo_seleccion_2 <- lm(salario ~ educ + tenure + female + smsa + northcen + trade + services +
    profocc, data = datos_sin_lwage) 
summary(modelo_seleccion_2)
vif(modelo_seleccion_2)

AIC(modelo_seleccion)
AIC(modelo_seleccion_2)

# d) Divida los salarios en dos grupos: Grupo 1: menores al cuartil 3 Grupo 2: iguales o mayores al cuartil 3
cuartil_3_salario <- quantile(datos_sin_lwage$salario, 0.75)
cuartil_3_salario
datos <- datos %>% mutate(salario_grupo = ifelse(salario <= 6.88, 0, 1))
datos$salario_grupo

mod_l <- glm(salario_grupo ~ educ + tenure + female + smsa + profocc, data=datos, family=binomial)	
summary(mod_l)
coef(mod_l)
confint(mod_l)

hoslem.test(datos$salario_grupo, fitted(mod_l))

# Cargar el paquete necesario
library(rpart)
library(caret)

# Entrenar un árbol de decisión
arbol <- rpart(salario_grupo ~ educ + tenure + female + smsa + profocc, data = datos, method = "class")

# Evaluar el rendimiento del modelo logístico
pred_log <- predict(mod_l, type = "response")
pred_log_class <- ifelse(pred_log > 0.5, 1, 0)
conf_matrix_log <- confusionMatrix(as.factor(pred_log_class), as.factor(datos$salario_grupo))

# Evaluar el rendimiento del árbol de decisión
pred_arbol <- predict(arbol, type = "class")
conf_matrix_arbol <- confusionMatrix(pred_arbol, as.factor(datos$salario_grupo))

# Mostrar las matrices de confusión
print("Matriz de confusión del modelo logístico:")
print(conf_matrix_log)

print("Matriz de confusión del árbol de decisión:")
print(conf_matrix_arbol)

# Comparar las métricas de rendimiento
accuracy_log <- conf_matrix_log$overall['Accuracy']
accuracy_arbol <- conf_matrix_arbol$overall['Accuracy']

print(paste("Exactitud del modelo logístico:", accuracy_log))
print(paste("Exactitud del árbol de decisión:", accuracy_arbol))
