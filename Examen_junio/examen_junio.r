rm(list=ls())
setwd('C:/RegresionAvanzada/Examen_junio')
library(readxl)
library(car)
library(lmtest)
library(leaps)

# Cargamos los datos
datos <- read_excel('internet_examen.xlsx')
head(datos) 
dim(datos)
# Convertir a factores Nacionalidad, Sexo, Interés
datos$Nacionalidad <- as.factor(datos$Nacionalidad)
datos$Sexo <- as.factor(datos$Sexo)
datos$Interes <- as.factor(datos$Interes)
str(datos)

# Por cada columna buscar datos faltantes
sapply(datos, function(x) sum(is.na(x)))

# 1-1. Construya un modelo lineal simple para explicar el tiempo de uso de internet en función de alguna de las restantes variables numéricas 
# y evalúe la bondad del ajuste. Seleccione la variable de mayor poder explicativo entre las disponibles en la base.

# Realizar la selección de variables usando regsubsets 
regfit <- regsubsets(Tiempo ~ Edad + Estatura + Temperatura + Autos + Cigarrillos + Trabajo, data = datos, nvmax = 1, method = "exhaustive", 
nbest = 6)
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

# La variable de mayor poder explicativo es Trabajo con un R^2 ajustado de 0.7097832
modelo_simple <- lm(Tiempo ~ Trabajo,  data=datos)
summary(modelo_simple)
qqPlot(modelo_simple, main="Q-Q Plot")
par(mfrow=c(2,2))
plot(modelo_simple)
par(mfrow=c(1,1))
influenceIndexPlot(modelo_simple, id.method="identify", main="Influence Index Plot", id.n=4)

# Identificar los valores atípicos cuyos valores absolutos de residuos estandarizados sean mayores a 3
res_stu_1<-rstudent(modelo_simple)
res_stu_1[abs(res_stu_1)>3]

# Otros puntos influyentes: dfbetas y dffits
length(summary(influence.measures(model = modelo_simple)))

# Calcular dfbetas para el modelo
dfb <- dfbetas(modelo_simple)
dfb <- dfb[, 2]
# Ordenar de mayor a menor
dfb <- dfb[order(abs(dfb), decreasing = TRUE), ]
head(dfb)

# Filtrar y mostrar solo las observaciones donde dfbetas para la variable Trabajo es mayor que 1
dfbetas_greater <- dfb > 1
dfbetas_greater
dfb[dfbetas_greater]
which(dfbetas(modelo_simple)[,2]>1)
# NO hay valores dfbetas mayores a 1 

n<-length(datos$Tiempo)
p<-length(coef(modelo_simple))
which(dffits(modelo_simple)>2 * sqrt(p / n))
# Contar los valores dffits mayores a 2*sqrt(p/n)
length(which(dffits(modelo_simple)>2 * sqrt(p / n)))
# Hay 84 valores dffits mayores a 2*sqrt(p/n) siendo posibles valores influyentes

influencePlot(model = modelo_simple)

hatvalues(modelo_simple)
outlierTest(modelo_simple)
hist(hatvalues(modelo_simple))

lev<-hatvalues(modelo_simple)
#un criterio (mayores que 0.2) 
which(lev>0.2)

#un criterio mas exigente
which(lev>2*p/n)

# distancias de Cook
dcook<-cooks.distance(modelo_simple)
influenceIndexPlot(modelo_simple, vars='Cook', las=1,col='blue')
which(dcook>4/n)
hist(dcook)

#punto de corte
corted<-qf(0.5,2,n-2)
which(dcook>corted)

# Análisis diagnóstico de modelo simple
# Normalidad de los residuos
shapiro.test(residuals(modelo_simple))
# Rechaza normalidad

# Homocedasticidad
ncvTest(modelo_simple)
# Otro test homocedasticidad
bptest(modelo_simple)
# Rechaza homocedasticidad

# Independencia
durbinWatsonTest(modelo_simple)
# No rechaza independencia

# No se cumplen los supuestos de normalidad de los residuos y homocedasticidad por lo que el modelo no es adecuado

#3  Realice una transformación de la variable respuesta, en caso de ser necesario, 
# para intentar lograr normalidad en la distribución de los residuos. 
# Indique si el modelo con esta transformación resulta adecuado.
tr <- summary(powerTransform(modelo_simple))
lambda <- tr$result[2] # lambda redondeado 

# Aplicar la transformación de Box-Cox con el parámetro estimado
datos$transformed_Tiempo <- bcPower(datos$Tiempo, lambda = lambda)
head(datos)
# Ajustar el nuevo modelo con la variable transformada
modelo_transformado <- lm(transformed_Tiempo ~ Trabajo, data = datos)
summary(modelo_transformado)

# Evaluar la normalidad de los residuos del nuevo modelo
shapiro_test_transformado <- shapiro.test(residuals(modelo_transformado))
shapiro_test_transformado
# Sigue sin cumplir el supuesto de normalidad

# Evaluar la homocedasticidad de los residuos del nuevo modelo
ncvTest(modelo_transformado)
# sigue sin cumplir el supuesto de homocedasticidad

qqPlot(modelo_transformado, main="Q-Q Plot")

# Graficar los residuos estandarizados del nuevo modelo
par(mfrow=c(2,2))
plot(modelo_transformado)
par(mfrow=c(1,1))

# No resulta adecuado el modelo con la transformación de la variable respuesta ya que siguen sin cumplirse 
# los supuestos de normalidad y homocedasticidad. Los residuos no siguen una distribución normal y 
# no son homocedásticos lo que implica que el modelo no es adecuado.

# 4 Ajuste un modelo multivariado usando una regresión polinómica de grado 2 para explicar el tiempo 
# de uso de internet y estime el error absoluto medio cometido.


# Ajustar el modelo de regresión polinómica de grado 2
modelo_polinomico <- lm(Tiempo ~ poly(Trabajo, 2, raw = TRUE) + poly(Edad, 2, raw = TRUE) + poly(Estatura, 2, raw = TRUE) + poly(Temperatura, 2, raw = TRUE) + poly(Autos, 2, raw = TRUE) + poly(Cigarrillos, 2, raw = TRUE), data = datos)
summary(modelo_polinomico)
# Ajustar el modelo de regresión polinómica de grado 2 usando poly
modelo_polinomico2 <- lm(Tiempo ~ poly(Trabajo, 2) + poly(Edad, 2) + poly(Estatura, 2) + poly(Temperatura, 2) + poly(Autos, 2) + poly(Cigarrillos, 2), data = datos)
summary(modelo_polinomico2)
# Quedarme solo con los coeficientes significativos
modelo_polinomico3 <- lm(Tiempo ~ poly(Trabajo, 2) + poly(Edad, 2) + poly(Cigarrillos, 2), data = datos)
summary(modelo_polinomico3)

# Hacer el análisis de diagnóstico del modelo polinómico 3
# Normalidad de los residuos
shapiro.test(residuals(modelo_polinomico3))

# Homocedasticidad
ncvTest(modelo_polinomico3)

# Independencia
durbinWatsonTest(modelo_polinomico3)

par(mfrow=c(2,2))
plot(modelo_polinomico3)
par(mfrow=c(1,1))

# Calcular el error absoluto medio
predicciones <- predict(modelo_polinomico3, datos)
mean(abs(datos$Tiempo - predicciones))

# 5. Utilice un método de selección de variables para proponer un nuevo modelo multivariado 
# que explique el tiempo de uso de internet, considerando como máximo 8 variables. 
# Estudie el cumplimiento de los supuestos.

datos_orig_sin_id <- datos[,2:11]
head(datos_orig_sin_id)

# Realizar la selección de variables usando regsubsets 
options(width = 180)

library(leaps)

# Realizar la selección de variables usando regsubsets
regfit <- regsubsets(Tiempo ~ ., data = datos_orig_sin_id, nvmax = 50, method = "exhaustive")

# Obtener el resumen del ajuste
reg_summary <- summary(regfit)
reg_summary
reg_summary$adjr2
# se identifica qué modelo tiene el valor máximo de R ajustado 
which.max(reg_summary$adjr2)
# Identificar que variables se seleccionaron en el 11
selected_vars <- names(which(reg_summary$which[11, ]))
selected_vars

modelo_seleccionado <- lm(Tiempo ~ Nacionalidad + Edad + Sexo + Interes + Cigarrillos + Trabajo , data = datos)
summary(modelo_seleccionado)

# Evaluar los supuestos del modelo seleccionado
# Normalidad de los residuos
shapiro_test_seleccionado <- shapiro.test(residuals(modelo_seleccionado))
cat("Prueba de Shapiro-Wilk para normalidad de los residuos: p-valor =", shapiro_test_seleccionado$p.value, "\n")

# Homocedasticidad
ncvTest(modelo_seleccionado)
bptest(modelo_seleccionado)
durbinWatsonTest(modelo_seleccionado)

# Graficar los residuos estandarizados del nuevo modelo
par(mfrow=c(2,2))
plot(modelo_seleccionado)
par(mfrow=c(1,1))

# No se cumplen los supuestos de normalidad de los residuos y homocedasticidad 
# por lo que el modelo no es adecuado

# El intercepto representa el tiempo de uso de internet cuando todas las variables explicativas son iguales a cero.
# En este caso sería una mujer argentina cuyo interes por Internet son los buscadores.
# Si es brasilera se incrementa en 1.27 minutos el tiempo de uso de internet
# Si es Canadiense se incrementa en 4 minutos el tiempo de uso de internet
# Si es Uruguaya decrece en 23 minutos el tiempo de uso de internet
# Por cada año de edad decrementa 0.4 minutos el tiempo de uso de internet
# Si es hombre se incrementa en 4.3 minutos el tiempo de uso de internet
# Si el interes es por el chat se incrementa en 48 minutos el tiempo de uso de internet
# Si el interes es por el correo se incrementa en 24 minutos el tiempo de uso de internet
# Si el interes es por la música se incrementa en 10 minutos el tiempo de uso de internet
# Si el interes es por otras cosas se incrementa en 13 minutos el tiempo de uso de internet
# Por cada cigarrillo que fuma se incrementa en 1.3 minutos el tiempo de uso de internet
# Por cada minuto de trabajo se incrementa en 0.2 minutos el tiempo de uso de internet

# 6 - Estime los errores de predicción de los 4 modelos previos y compárelos. ¿Cuál elegiría?
library(caret)
# Utilizar CV 10 folds para estimar los errores de predicción
# Modelo simple
# modelo_simple <- lm(Tiempo ~ Trabajo,  data=datos)
cv_modelo_simple <- train(Tiempo ~ Trabajo, data = datos, method = "lm", trControl = trainControl(method = "cv", number = 10))
# Modelo transformado
cv_modelo_transformado <- train(transformed_Tiempo ~ Trabajo, data = datos, method = "lm", trControl = trainControl(method = "cv", number = 10))
# Modelo polinomico 3
cv_modelo_polinomico3 <- train(Tiempo ~ poly(Trabajo, 2) + poly(Edad, 2) + poly(Cigarrillos, 2), data = datos, method = "lm", trControl = trainControl(method = "cv", number = 10))
# modelo seleccionado
cv_modelo_seleccionado <- train(Tiempo ~ Nacionalidad + Edad + Sexo + Interes + Cigarrillos + Trabajo , data = datos, method = "lm", trControl = trainControl(method = "cv", number = 10))

print(cv_modelo_simple$results)
print(cv_modelo_transformado$results)
print(cv_modelo_polinomico3$results)
print(cv_modelo_seleccionado$results)

# El modelo transformado por Box Cox es el que tiene el menor error de predicción, y además es el más estable 
# por lo que sería el modelo elegido

# 7. Le parece adecuado un modelo GAMLSS en este caso? Justifique.
# Un modelo GAMLSS puede ser adecuado en este caso debido a su flexibilidad para manejar distribuciones no normales, 
# modelar parámetros adicionales de la distribución y capturar relaciones no lineales. 
# Dado que los modelos lineales simples y polinómicos han mostrado problemas con la normalidad 
# de los residuos y la homocedasticidad, GAMLSS ofrece una alternativa robusta para mejorar el ajuste 
# y la interpretación del modelo.

library(gamlss)
# Verificar si hay valores NA en las variables utilizadas en el modelo
na_check <- sapply(datos[, c("Tiempo", "Trabajo", "Edad", "Sexo", "Interes", "Cigarrillos", "Nacionalidad")], function(x) sum(is.na(x)))
print(na_check)

sum(is.na(datos$Trabajo))
sum(is.na(datos$Tiempo))

datos <- na.omit(datos)

datos$Trabajo[is.na(datos$Trabajo)] <- mean(datos$Trabajo, na.rm = TRUE)
datos$Tiempo[is.na(datos$Tiempo)] <- mean(datos$Tiempo, na.rm = TRUE)

# Eliminar filas con valores NA
datos_clean <- na.omit(datos[, c("transformed_Tiempo","Tiempo", "Trabajo", "Edad", "Sexo", "Interes", "Cigarrillos", "Nacionalidad")])
# Eliminar filas en las que Trabajo = 0
datos_clean <- datos_clean[datos_clean$Trabajo != 0, ]

# Ajustar un modelo GAMLSS
modelo_gamlss <- gamlss(formula = Tiempo ~ Nacionalidad + pb(Edad) + Sexo + Interes + Cigarrillos + pb(Trabajo), 
                        sigma.formula = ~ Nacionalidad + pb(Edad) + Sexo + Interes + Cigarrillos + pb(Trabajo), 
                        family = GA, data = datos_clean, trace = TRUE)
# Mostrar el resumen del modelo
summary(modelo_gamlss)

# Ajustar un modelo GAMLSS usando tiempo transformado
modelo_gamlss_transformado <- gamlss(formula = transformed_Tiempo ~ Nacionalidad + pb(Edad) + Sexo + Interes + Cigarrillos + pb(Trabajo), 
                        sigma.formula = ~ Nacionalidad + pb(Edad) + Sexo + Interes + Cigarrillos + pb(Trabajo), 
                        family = GA, data = datos_clean, trace = TRUE)
# Mostrar el resumen del modelo
summary(modelo_gamlss)



# Definir una función para realizar la validación cruzada de 10 folds
gamlss_cv <- function(data) {
  # Crear índices para los folds
  set.seed(123)  # Para reproducibilidad
  folds <- createFolds(data$Tiempo, k = 10, list = TRUE, returnTrain = TRUE)
  
  # Inicializar vectores para almacenar las métricas
  rmse_values <- c()
  mae_values <- c()
  r2_values <- c()
  
  # Realizar la validación cruzada
  for (i in 1:length(folds)) {
    # Dividir los datos en entrenamiento y prueba
    train_indices <- folds[[i]]
    test_indices <- setdiff(1:nrow(data), train_indices)
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]

    # Predecir en los datos de prueba
    predictions <- predict(modelo_gamlss, newdata = test_data, type = "response")
    
    # Calcular las métricas de desempeño
    rmse <- sqrt(mean((test_data$Tiempo - predictions)^2))
    mae <- mean(abs(test_data$Tiempo - predictions))
    r2 <- cor(test_data$Tiempo, predictions)^2
    
    # Almacenar las métricas
    rmse_values <- c(rmse_values, rmse)
    mae_values <- c(mae_values, mae)
    r2_values <- c(r2_values, r2)
  }
  
  # Calcular las métricas promedio
  metrics <- list(
    RMSE = mean(rmse_values),
    MAE = mean(mae_values),
    R2 = mean(r2_values)
  )
  
  return(metrics)
}

# Realizar la validación cruzada de 10 folds
metrics <- gamlss_cv(datos_clean) 

# Mostrar las métricas de desempeño
print(metrics)
print(cv_modelo_transformado$results)


























