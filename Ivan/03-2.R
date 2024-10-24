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
library(faraway)
require(leaps) 

library(sp)
library(glmnet)
library(pls)
library(ISLR)
library(ggpubr)


library(tidyverse)
library(caret) 
library(boot)



####################
## Load
####################

data("fat")
head(fat)


####################
## Debora
####################

#--------<debora>-------------
# consideramos todos los modelos hasta 14 predictoras
regfit.todos <- regsubsets(brozek ~ ., data = fat, nvmax = 14)
reg.summary <- summary(regfit.todos)
reg.summary
# definimos puntos óptimos para c/ criterio, guardamos su posición
max_adjr2=which.max(reg.summary$adjr2)
min_cp=which.min(reg.summary$cp)
min_bic=which.min(reg.summary$bic)
which.min(reg.summary$aic)
sal_reg=data.frame(orden=1:14,adjr2=reg.summary$rsq,Cp=reg.summary$cp,Bic=reg.summary$bic)





# graficamos los valores de c/ criterio en función de la cant. de predictores
pl1=ggplot(sal_reg,aes(orden,adjr2))+geom_point(size=2,col="darkblue")+
  theme_bw()+xlab("cantidad de predictores")+ylab("R-2ajust")+
  geom_point(aes(max_adjr2,reg.summary$adjr2[max_adjr2]),
             size=3,color="coral")

pl2=ggplot(sal_reg,aes(orden,Cp))+geom_point(size=2,col="darkblue")+
  theme_bw()+xlab("cantidad de predictores")+ylab("Cp")+
  geom_point(aes(min_cp,reg.summary$cp[min_cp]),size=3,color="coral")


pl3=ggplot(sal_reg,aes(orden,Bic))+geom_point(size=2,col="darkblue")+
  theme_bw()+xlab("cantidad de predictores")+ylab("BIC")+
  geom_point(aes(min_bic,reg.summary$bic[min_bic]),size=3,color="coral")

ggarrange(pl1,pl2,pl3,ncol=1)

#--------</debora>-------------



####################
## a) CP de Mallows y R2 ajustado
####################

# Hallar el mejor modelo de regresion lineal con variable respuesta brozek
# utilizando entre 1 y 14 variables predictoras. Elegir el mejor considerando 
# el criterio CP de Mallows y R2 ajustado.


mejores_modelos <- regsubsets(brozek ~ ., data = fat, nvmax = 14) 
summary(mejores_modelos)
summary(mejores_modelos)$adjr2
which.max(summary(mejores_modelos)$adjr2)       # Mejor modelo R2adj: 10. 
summary(mejores_modelos)$cp
which.min(summary(mejores_modelos)$cp)          # Mejor modelo CP: 7


# buscamos los coeficientes del modelo con 7 predictores
coef(mejores_modelos, 7)        # IMPORTANTISIMO




#<-----<chatgpt>------
leaps_result <- regsubsets(brozek ~ ., data = fat, nvmax = 14)
leaps_summary <- summary(leaps_result)
best_cp_model <- which.min(leaps_summary$cp)
leaps_summary$which[best_cp_model, ]        # me muestra los predictores del mejor modelo segun CP.
#-----</chatgpt>------!>


#### Graficamos r2 ajustado
p <- ggplot(data = data.frame(n_predictores = 1:14, R_ajustado = summary(mejores_modelos)$adjr2), aes(x = n_predictores, y = R_ajustado)) + 
  geom_line() + 
  geom_point() 
# Se identifica en rojo el maximo 
p <- p + geom_point(aes(x=n_predictores[which.max(summary(mejores_modelos)$adjr2)], y=R_ajustado[which.max(summary(mejores_modelos)$adjr2)]), colour = "red", size = 3) 
p <- p + scale_x_continuous(breaks = c(0:15)) + theme_bw() + 
  labs(title = "R2_ajustado vs numero de predictores", x = "n?mero predictores") 
p



#### Graficamos cp
q <- ggplot(data = data.frame(n_predictores = 1:14, cp = summary(mejores_modelos)$cp), aes(x = n_predictores, y = cp)) + 
  geom_line() + 
  geom_point() 
# Se identifica en rojo el m?ximo 
q <- q + geom_point(aes(x=n_predictores[which.max(summary(mejores_modelos)$cp)], y=cp[which.max(summary(mejores_modelos)$cp)]), colour = "red", size = 3) 
q <- q + scale_x_continuous(breaks = c(0:15)) + theme_bw() + 
  labs(title = "CP vs n?mero de predictores", x = "n?mero predictores") 
q





####################
## b) Error Cuadratico Medio - validacion cruzada leave one out.
####################



# Repetir considerando ahora la minimizacion del Error Cuadratico Medio
# del modelo usando validacion cruzada leave one out.



set.seed(12345)
train_control2 <- trainControl(method = "LOOCV")
model2 <- train(brozek ~., data = fat,method = "lm",trControl = train_control2)
print(model2)
model2$results$MAE
# RMSE       Rsquared   MAE       
# 0.2204408  0.9991899  0.09269793
summary(model2)
      


#<-----chatgpt------

# Funcion para calcular el MSE con LOOCV
loocv_mse <- function(model_formula, data) {
  fit <- glm(model_formula, data = data)
  cv <- cv.glm(data, fit, K = nrow(data))
  return(cv$delta[1])
}

# Calcular el MSE para cada modelo
formulas <- as.formula(paste("brozek ~", paste(names(fat)[leaps_summary$which[best_cp_model, -1]], collapse = "+")))
loocv_mse_cp <- loocv_mse(formulas, fat)

formulas <- as.formula(paste("brozek ~", paste(names(fat)[leaps_summary$which[best_adjr2_model, -1]], collapse = "+")))
loocv_mse_adjr2 <- loocv_mse(formulas, fat)

cat("MSE CP Model:", loocv_mse_cp, "\n")
cat("MSE AdjR2 Model:", loocv_mse_adjr2, "\n")
#-----chatgpt------!>

#  <Preguntar> como se hace este ejercicio.   </Preguntar> 









####################
## c) Graficar MSE - Mejor modelo -  Coeficientes
####################

# Inspeccionar graficamente el MSE y decidir cual es el mejor modelo. 
# Interpretar los coeficientes del mismo.


# Visualizamos graficamente el MSE para cada modelo utilizando los resultados de LOOCV:
# Grafico del MSE para los diferentes modelos
best_cp_model = which.min(leaps_summary$cp)
best_adjr2_model = which.max(leaps_summary$adjr2)
mse_values <- sapply(1:14, function(i) {
  model_formula <- as.formula(paste("brozek ~", paste(names(fat)[leaps_summary$which[i, -1]], collapse = "+")))
  loocv_mse(model_formula, fat)
})

plot(1:14, mse_values, type = "b", xlab = "Numero de variables", ylab = "MSE LOOCV", main = "MSE por numero de variables")
abline(v = best_cp_model, col = "red", lty = 2)
abline(v = best_adjr2_model, col = "blue", lty = 2)
legend("topright", legend = c("Mejor CP", "Mejor AdjR2"), col = c("red", "blue"), lty = 2)






# Seleccionamos el modelo con el menor MSE y analizamos sus coeficientes:
# Seleccionar el mejor modelo basado en el MSE
best_model <- ifelse(loocv_mse_cp < loocv_mse_adjr2, best_cp_model, best_adjr2_model)
final_formula <- as.formula(paste("brozek ~", paste(names(fat)[leaps_summary$which[best_model, -1]], collapse = "+")))
final_model <- lm(final_formula, data = fat)
summary(final_model)

  



# Finalmente, comparamos las variables seleccionadas por los diferentes criterios:
# Comparar las variables seleccionadas por CP y AdjR2
variables_cp <- names(fat)[leaps_summary$which[best_cp_model, -1]]
variables_adjr2 <- names(fat)[leaps_summary$which[best_adjr2_model, -1]]

cat("Variables CP Model:", variables_cp, "\n")
cat("Variables AdjR2 Model:", variables_adjr2, "\n")


# Conclusiones
# Interpretaci?n del mejor modelo: Basado en el gr?fico del MSE y los valores de CP y 
# r2 ajustado, seleccionamos el mejor modelo y analizamos sus coeficientes.
# Consistencia entre criterios: Comparamos si las variables seleccionadas por los diferentes criterios coinciden.