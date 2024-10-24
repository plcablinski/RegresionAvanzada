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
library(lars)
library(glmnet)
library(caret)
library(coefplot)
library(readxl)
library(corrplot)



####################
## Load
####################
archivo = "prostata.csv"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-fread(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3



####################
## Dataframe
####################
#attach(dataset)
prostata <- dataset
head(prostata)
str(prostata)
#summary(crime_data





####################
## Train y Test
####################

# set.seed(12345)
# train_ind <- sample(1:nrow(prostata), size = 0.75*(nrow(prostata)))
# train <- prostata[train_ind, ]   # Nuesto  X
# test <-  prostata[-train_ind, ]   # Nuestro Y
# std  <-  TRUE
# 
# 
# X = model.matrix(log_psa ~ .-1, data=prostata) # matriz de diseño
# y = prostata$log_psa # nuestra variable dependiente


set.seed(12345) 
indices_entrenamiento <- sample(x = 1:nrow(prostata), size = 0.75*(nrow(prostata))) 
indices_test <- (1:nrow(prostata))[-indices_entrenamiento] 
data_1 <- prostata[indices_entrenamiento, ] 
data_2 <- prostata[indices_test, ]
x_data_1 <- model.matrix(log_psa ~ ., data = data_1)[, -1] 
y_data_1 <- data_1$log_psa 
x_data_2 <- model.matrix(log_psa ~ ., data = data_2)[, -1] 
y_data_2 <- data_2$log_psa
##### Lo que seria el _1 es el train y lo que es _2 es el test.




####################
## a) Modelo Lineal
####################


# Ordinary least square (regresión por mínimos cuadrados)
modelo_OLS <- lm(log_psa ~ ., data = data_1)

# Resumen del modelo completo
summary(modelo_OLS)

# Inconvenientes del modelo completo:
# -  Multicolinealidad: Las variables predictoras pueden estar altamente correlacionadas entre si, 
#    lo que puede causar inestabilidad en las estimaciones de los coeficientes.
# -  Sobreajuste: Ajustar un modelo con muchas variables puede llevar a un modelo que se ajuste 
#    demasiado bien a los datos de entrenamiento pero no generalice bien a nuevos datos.



M=cor(prostata)
corrplot(M, tl.col = "red", bg = "White", tl.srt = 35, addCoef.col
         = "black", type = "full")

# Calculamos el VIF (valor de inflación de la varianza) de cada variable.
car::vif(modelo_OLS)


# MSE
# modelo.pred <- predict(modelo_completo, new_data=test)
# lm_mse <- mean((modelo.pred - test$log_psa)**2)
# lm_mse   # 1.39


test_MSE_OLS <- mean((predict(modelo_OLS, data_2) - data_2$log_psa)^2) 
test_MSE_OLS   # 0.54







####################
## b) Mejor Modelo segun BIC
####################


mejores_modelos <- regsubsets(log_psa  ~ ., data = prostata, nvmax = 8) 
names(summary(mejores_modelos))
summary(mejores_modelos)$bic
reg_summary <- summary(mejores_modelos)


plot(reg_summary$bic, xlab = "Número de variables predictoras", ylab = "BIC", type = "l")
points(which.min(reg_summary$bic), min(reg_summary$bic), col = "red", cex = 2, pch = 20)


best_bic_model <- which.min(reg_summary$bic)
best_bic_model    # 3
reg_summary$which[best_bic_model, ]      
coef(mejores_modelos, 3)    







####################
## c) Metodos de regularizacion
####################

tmatrix <- model.matrix(log_psa~.,test)[,-1]


# Ajustamos un modelo Ridge
ridge.cv  <- cv.glmnet(x=x_data_1, y = y_data_1, alpha = 0, type="mse", family="gaussian", standardize = std, nfolds = 10)
ridge_mdl <- glmnet(x=x_data_1, y = y_data_1, alpha=0,family="gaussian", standardize=std,lambda = ridge.cv$lambda.1se)
#ridge_mdl = glmnet(x=X,y=y,alpha=0,family="gaussian", standardize=std, nlambda=100)
best_lambda_ridge <- ridge.cv$lambda.min
best_lambda_ridge       # 0.334
ridge_coefs <- coef(ridge_mdl, s = best_lambda_ridge)
ridge_coefs 
# MSE
predicciones_ridge <- predict(object = ridge_mdl, newx = x_data_2, s = ridge.cv$lambda.1se, exact = TRUE) 
test_MSE_ridge <- mean((predicciones_ridge - data_2$log_psa)^2) 
test_MSE_ridge    # 0.468



# Ajustamos un modelo Lasso
lasso.cv  = cv.glmnet(x=x_data_1, y = y_data_1, alpha=1, type="mse", family="gaussian", standardize=std, nfolds=10)
# lasso_mdl = glmnet(x=x_data_1, y = y_data_1, alpha=1,family="gaussian", standardize=std, nlambda=100)
lasso_mdl = glmnet(x=x_data_1, y = y_data_1, alpha=1,family="gaussian", standardize=std, lambda = lasso.cv$lambda.1se)
best_lambda_lasso <- lasso.cv$lambda.min
best_lambda_lasso       # 0.029
lasso_coefs <- coef(lasso_mdl,s=lasso.cv$lambda.min)
lasso_coefs
# MSE
predicciones_lasso <- predict(object = lasso.cv, newx = x_data_2, s = lasso.cv$lambda.1se, exact = TRUE) 
test_MSE_lasso <- mean((predicciones_lasso - data_2$log_psa)^2) 
test_MSE_lasso        # 0.480


# Ajustamos un modelo Elastic Net
enet.cv=cv.glmnet(x=x_data_1, y = y_data_1, alpha=0.5, type="mse", family="gaussian", standardize=std, nfolds=10)
# enet_mdl = glmnet(x=X,y=y,alpha=0.5,standardize=std,nlambda=100)
enet_mdl = glmnet(x=x_data_1, y = y_data_1,alpha=0.5,standardize=std,lambda=enet.cv$lambda.1se)
# Extraemos los coeficientes del modelo con lambda óptimo
enet.cv$lambda.min     # 0.0042
# Coeficientes del modelo de Elastic Net
elastic_net_coefs <- coef(enet_mdl,s=enet.cv$lambda.min)
elastic_net_coefs
# MSE
predicciones_enet <- predict(object = enet.cv, newx = x_data_2, s = enet.cv$lambda.1se, exact = TRUE) 
test_MSE_enet     <- mean((predicciones_enet - data_2$log_psa)^2) 
test_MSE_enet    # 0.459



coeficientes <- data.frame(elastic_net_coefs)

elastic_net_coefs



# Ajustar modelos regularizados y comparar los resultados utilizando validacion cruzada
# Ajuste del modelo Ridge

# Preparaci?n de los datos para glmnet
X <- as.matrix(data[, -which(names(data) == "log_psa")])
y <- data$log_psa

# Ajustar el modelo de Ridge con validaci?n cruzada
ridge_model <- cv.glmnet(X, y, alpha = 0, standardize = TRUE)

# Obtener el mejor lambda
best_lambda_ridge <- ridge_model$lambda.min
ridge_coefs <- coef(ridge_model, s = best_lambda_ridge)
print(ridge_coefs)



# Ajuste del modelo Lasso
# Ajustar el modelo de Lasso con validaci?n cruzada
lasso_model <- cv.glmnet(X, y, alpha = 1, standardize = TRUE)

# Obtener el mejor lambda
best_lambda_lasso <- lasso_model$lambda.min
lasso_coefs <- coef(lasso_model, s = best_lambda_lasso)
print(lasso_coefs)



# Ajuste del modelo Elastic Net
# Configurar una b?squeda de hiperpar?metros para Elastic Net
elastic_net_model <- train(
  log_psa ~ ., data = data,
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# Obtener el mejor modelo
best_elastic_net <- elastic_net_model$bestTune
elastic_net_coefs <- coef(elastic_net_model$finalModel, 
                          s = elastic_net_model$bestTune$lambda)
print(elastic_net_coefs)








####################
## Conclusiones
####################





# Comparacion de resultados: COEFICIENTES

# Cargar las librerías necesarias
library(glmnet)
library(dplyr)



ols_df <- as.data.frame(as.matrix(modelo_OLS$coefficients))
ols_df$Name <- rownames(ols_df)
colnames(ols_df) <- c("OLS", "Name")

bic_df <- as.data.frame(as.matrix(coef(mejores_modelos, 3)))
bic_df$Name <- rownames(bic_df)
colnames(bic_df) <- c("BIC", "Name")

# Convertir los coeficientes en dataframes
lasso_df <- as.data.frame(as.matrix(lasso_coefs))
lasso_df$Name <- rownames(lasso_df)
colnames(lasso_df) <- c("Lasso", "Name")

elastic_net_df <- as.data.frame(as.matrix(elastic_net_coefs))
elastic_net_df$Name <- rownames(elastic_net_df)
colnames(elastic_net_df) <- c("ElasticNet", "Name")

ridge_df <- as.data.frame(as.matrix(ridge_coefs))
ridge_df$Name <- rownames(ridge_df)
colnames(ridge_df) <- c("Ridge", "Name")

# Combinar los dataframes en uno solo
combined_df <- lasso_df %>%
  full_join(elastic_net_df, by = "Name") %>%
  full_join(ridge_df, by = "Name")  %>%
  full_join(ols_df, by = "Name")  %>%
  full_join(bic_df, by = "Name")

# Reordenar columnas
combined_df <- combined_df %>% select(Name, Lasso, ElasticNet, Ridge, OLS, BIC)

# Mostrar el dataframe combinado
print(combined_df)





# Comparacón: MSE
metodo <- c("OLS",  "Ridge", "LASSO", "ElasticNET") 
test_MSE <- c(test_MSE_OLS, test_MSE_ridge, test_MSE_lasso, test_MSE_enet) 
resultados <- data.frame(metodo, test_MSE) 
resultados



ggplot(data = resultados, aes(x = reorder(metodo, test_MSE), y = sqrt(test_MSE))) + 
  geom_bar(stat = "identity") + 
  labs(x = "Método de regresión", y = expression(sqrt("test MSE"))) + theme_bw()







# Conclusiones
# - Modelo completo: Puede presentar problemas de multicolinealidad y sobreajuste.
# - Seleccion de variables por BIC: Reduce el numero de variables, haciendo el modelo mas 
#   interpretable y posiblemente mejor en terminos de generalizacion.
# - Modelos regularizados: Ayudan a manejar la multicolinealidad y el sobreajuste. 
# - Comparar los coeficientes y el desempeño (medido por RMSE) de los modelos de Ridge, 
# Lasso y Elastic Net proporciona una vision clara de cual modelo generaliza mejor a nuevos datos.
# Estos pasos completos te permitiran ajustar y comparar diferentes modelos de regresion 
# utilizando el conjunto de datos de prostata.