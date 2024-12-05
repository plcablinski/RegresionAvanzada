rm(list = ls())
library(caret)
library(leaps)

file<-"low birth weight infants.txt"
setwd("C:/RegresionAvanzada/Clase_4")
data <- read.table(file = file, header = TRUE)
head(data)

# Divido en conjunto de train y test
set.seed(1) 
indices_entrenamiento <- sample(x = 1:nrow(data), size = round(nrow(data) * (2/3))) 
# 2/3 de las observaciones 
indices_test <- (1:nrow(data))[-indices_entrenamiento] 
data_1 <- data[indices_entrenamiento, ] 
data_2 <- data[indices_test, ]

# Validación simple
#Ordinary least square (regresión por mínimos cuadrados)
modelo_OLS <- lm(formula = headcirc ~ ., data = data_1) 
MSE_OLS_simple <- mean((predict(modelo_OLS, data_2) - data_2$headcirc)^2) 
MSE_OLS_simple

# Validación cruzada 10-fold usando la función train
control <- trainControl(method = "cv", number = 10)
modelo_OLS_cv <- train(headcirc ~ ., data = data_1, method = "lm", trControl = control)
modelo_OLS_cv$results$RMSE^2 
MSE_OLS_cv <- mean((predict(modelo_OLS_cv$finalModel, data_2) - data_2$headcirc)^2)
MSE_OLS_cv

# Validación Leave-one-out
control <- trainControl(method = "LOOCV")
modelo_OLS_loo <- train(headcirc ~ ., data = data_1, method = "lm", trControl = control)
modelo_OLS_loo$results$RMSE^2
MSE_OLS_LOO <- mean((predict(modelo_OLS_loo, data_2) - data_2$headcirc)^2)
MSE_OLS_LOO

# Validación cruzada 10-fold 3 repetición
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelo_OLS_cv3 <- train(headcirc ~ ., data = data_1, method = "lm", trControl = control)
modelo_OLS_cv3$results$RMSE^2
MSE_OLS_cv3 <- mean((predict(modelo_OLS_cv3, data_2) - data_2$headcirc)^2)  
MSE_OLS_cv3

# Usando los datos de entrenamiento en base a la formula= headcirc ~ . hacer un CV 10 folds 
# para encontrar el mejor subconjunto tomando como criterio de selección AIC

select_best_subset <- function(data, formula, method = "cv", number = 10) {
  # Configurar la validación cruzada
  control <- trainControl(method = method, number = number)
  
  # Realizar la selección de subconjuntos usando el criterio AIC
  regsubsets_cv <- regsubsets(
    formula,
    data = data,
    nvmax = ncol(data) - 1,
    method = "seqrep"
  )
  
  # Obtener el mejor modelo basado en AIC
  best_model <- summary(regsubsets_cv)$which[which.min(summary(regsubsets_cv)$bic), ]
  
  return(best_model)
}

# Supongamos que tus datos de entrenamiento están en un data frame llamado data_1
# y que la variable dependiente es headcirc

# Aplicar la función de selección de subconjuntos
best_subset_model <- select_best_subset(data_1, headcirc ~ .)

# Mostrar los resultados
print(best_subset_model)
