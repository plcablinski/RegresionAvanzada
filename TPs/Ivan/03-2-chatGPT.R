# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection


library(faraway)
library(leaps)
library(boot)

# Cargar los datos
data(fat, package = "faraway")

# Ver los primeros registros del dataset
head(fat)


####################
## a) CP de Mallows y R2 ajustado
####################
# Hallar el mejor modelo de regresión lineal con variable respuesta brozek
# Vamos a utilizar el método de regresión paso a paso para seleccionar el mejor modelo utilizando el criterio Cp de Mallows y R² ajustado.


# Ajustar todos los modelos posibles usando la función regsubsets del paquete leaps
regfit_full <- regsubsets(brozek ~ age + weight + height + adipos + free + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, data = fat, nvmax = 14)

# Resumen de los modelos ajustados
reg_summary <- summary(regfit_full)

# Mostrar los criterios Cp de Mallows y R² ajustado para cada modelo
plot(reg_summary$cp, xlab = "Número de variables predictoras", ylab = "Cp de Mallows", type = "l")
points(which.min(reg_summary$cp), min(reg_summary$cp), col = "red", cex = 2, pch = 20)

plot(reg_summary$adjr2, xlab = "Número de variables predictoras", ylab = "R² ajustado", type = "l")
points(which.max(reg_summary$adjr2), max(reg_summary$adjr2), col = "red", cex = 2, pch = 20)

# Mejor modelo según Cp de Mallows
best_model_cp <- which.min(reg_summary$cp)
best_model_cp_vars <- names(coef(regfit_full, best_model_cp))
print(paste("Mejor modelo según Cp de Mallows: ", best_model_cp_vars))

# Mejor modelo según R² ajustado
best_model_adjr2 <- which.max(reg_summary$adjr2)
best_model_adjr2_vars <- names(coef(regfit_full, best_model_adjr2))
print(paste("Mejor modelo según R² ajustado: ", best_model_adjr2_vars))





####################
## b) Minimización del Error Cuadrático Medio usando validación cruzada leave-one-out
####################





# loocv_mse <- function(formula, data) {
#   model <- glm(formula, data = data)
#   cv_error <- cv.glm(data, model)
#   return(cv_error$delta[1])  # LOOCV error
# }

loocv_mse <- function(formula, data) {
  
  set.seed(12345)
  train_control2 <- trainControl(method = "LOOCV")
  
  model <- train(formula, data = data,method = "lm", trControl = train_control2)
  
  #model <- glm(formula, data = data)
  #cv_error <- cv.glm(data, model)
  return(model$results$MAE)  # LOOCV error
}


# Ajustar todos los modelos posibles usando la función regsubsets del paquete leaps
regfit_full <- regsubsets(brozek ~ age + weight + height + adipos + free + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, 
                          data = fat, nvmax = 14)

# Resumen de los modelos ajustados
reg_summary <- summary(regfit_full)

# Calcular el MSE usando LOOCV para cada modelo de 1 a 14 variables predictoras
loocv_errors <- sapply(1:14, function(nv) {
  formula <- as.formula(paste("brozek ~", paste(names(coef(regfit_full, nv))[-1], collapse = "+")))
  loocv_mse(formula, fat)
})

# Mejor modelo según LOOCV
best_loocv_model <- which.min(loocv_errors)
best_loocv_vars <- names(coef(regfit_full, best_loocv_model))[-1]
print(paste("Mejor modelo según LOOCV: ", paste(best_loocv_vars, collapse = ", ")))

# Graficar el MSE para cada modelo
plot(1:14, loocv_errors, type = "b", pch = 19, col = "blue", xlab = "Número de variables predictoras", ylab = "LOOCV MSE")
points(best_loocv_model, loocv_errors[best_loocv_model], col = "red", cex = 2, pch = 20)

best_formula_loocv <- as.formula(paste("brozek ~", paste(best_loocv_vars, collapse = "+")))
best_model_loocv <- lm(best_formula_loocv, data = fat)
summary(best_model_loocv)



####################
## c) Inspeccionar gráficamente el MSE y decidir cuál es el mejor modelo
####################

# Graficar el MSE para cada modelo
plot(1:10, loocv_errors, type = "b", pch = 19, col = "blue", xlab = "Número de variables predictoras", ylab = "LOOCV MSE")
points(best_loocv_model, loocv_errors[best_loocv_model], col = "red", cex = 2, pch = 20)

# Interpretar los coeficientes del mejor modelo según LOOCV
best_formula_loocv <- as.formula(paste("brozek ~", paste(best_loocv_vars, collapse = "+")))
best_model_loocv <- lm(best_formula_loocv, data = fat)
summary(best_model_loocv)




# Graficar el MSE para cada modelo
plot(1:10, loocv_errors, type = "b", pch = 19, col = "blue", xlab = "Número de variables predictoras", ylab = "LOOCV MSE")
points(best_loocv_model, loocv_errors[best_loocv_model], col = "red", cex = 2, pch = 20)

# Interpretar los coeficientes del mejor modelo según LOOCV
best_formula_loocv <- as.formula(paste("brozek ~", paste(best_loocv_vars, collapse = "+")))
best_model_loocv <- lm(best_formula_loocv, data = fat)
summary(best_model_loocv)




####################
## d) Coincidencia de las variables seleccionadas por diferentes criterios
####################
# Comparar las variables seleccionadas
print(paste("Variables del mejor modelo según Cp de Mallows: ", paste(best_model_cp_vars[-1], collapse = ", ")))
print(paste("Variables del mejor modelo según R² ajustado: ", paste(best_model_adjr2_vars[-1], collapse = ", ")))
print(paste("Variables del mejor modelo según LOOCV: ", paste(best_loocv_vars, collapse = ", ")))
