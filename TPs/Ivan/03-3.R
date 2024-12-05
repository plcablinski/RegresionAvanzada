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

####################
## Load
####################

data("longley", package = "lars")
head(longley)


####################
## Train y Test
####################

# y <- longley$Employed
# X <- as.matrix(longley[, -7])  # Excluir la columna 'Employed'. Este X es train.
# X_test <- longley[-X, ]
# std <- TRUE

set.seed(12345)
train_ind <- sample(1:nrow(longley), size = 0.75*(nrow(longley)))
train <- longley[train_ind, ]   # Nuesto  X
test <- longley[-train_ind, ]   # Nuestro Y
std <- TRUE


X = model.matrix(Employed ~ .-1, data=longley) # matriz de diseño
y = longley$Employed # nuestra variable dependiente


####################
## MSE Completo
####################

lm_comp <- lm(Employed~., data=train)
summary(lm_comp)
lm_comp.pred <- predict(lm_comp, new_data=test)
lm_mse <- mean((lm_comp.pred - test$Employed)**2)
lm_mse   # 29.75



####################
## a) modelo de Ridge
####################


# Utilizamos la funcion cv.glmnet para ajustar el modelo de Ridge con validacion cruzada:
ridge.cv <- cv.glmnet(X, y, alpha = 0, type="mse", family="gaussian", standardize = std, 
                         nfolds = 10)
ridge_mdl = glmnet(x=X,y=y,alpha=0,family="gaussian", standardize=std, nlambda=100)
# Obtener el mejor lambda
best_lambda_ridge <- ridge.cv$lambda.min
best_lambda_ridge       # 0.334

# Coeficientes del modelo de Ridge
ridge_coefs <- coef(ridge_mdl, s = best_lambda_ridge)
ridge_coefs    

plot(ridge_mdl)
coefpath(ridge_mdl)



####################
## b) modelo Lasso
####################


# Utilizamos la funcion cv.glmnet para ajustar el modelo de Lasso con validacion cruzada:

# Ajustar el modelo de Lasso
# Valor óptimo de lambda por 10-fold CV
lasso.cv=cv.glmnet(x=X,y=y,alpha=1, type="mse", family="gaussian",
                   standardize=std, nfolds=10)
lasso_mdl = glmnet(x=X,y=y,alpha=1,family="gaussian", standardize=std, nlambda=100)


# Obtener el mejor lambda
best_lambda_lasso <- lasso.cv$lambda.min
best_lambda_lasso       # 0.004


# Coeficientes del modelo de Lasso: Extraemos los coeficientes del modelo con lambda óptimo
lasso_coefs <- coef(lasso_mdl,s=lasso.cv$lambda.min)
lasso_coefs
# (Intercept)  -1.965218e+03
# GNP.deflator  .           
# GNP           .           
# Unemployed   -1.437822e-02
# Armed.Forces -7.770258e-03
# Population   -6.514378e-02
# Year          1.046202e+00


coefpath(lasso_mdl)
coefplot(lasso_mdl, lambda=lasso.cv$lambda.min,
         col="dodgerblue3",sort="magnitude")+theme_bw()






####################
## c) Elastic Net
####################


# Ajustar un modelo de Elastic Net
# Para el modelo de Elastic Net, ajustamos el modelo variando el parametro alpha entre 0 y 1:
# Configurar una busqueda de hiperparametros para Elastic Net


# Valor óptimo de lambda por 10-fold CV
enet.cv=cv.glmnet(x=X,y=y,alpha=0.5, type="mse", family="gaussian",
                  standardize=std, nfolds=10)
enet_mdl = glmnet(x=X,y=y,alpha=0.5,standardize=std,nlambda=100)
# Extraemos los coeficientes del modelo con lambda óptimo
enet.cv$lambda.min     # 0.0042
# Coeficientes del modelo de Elastic Net
elastic_net_coefs <- coef(enet_mdl,s=enet.cv$lambda.min)
elastic_net_coefs
# <chatgpt>
# elastic_net_model <- train(
#   Employed ~ ., data = longley,
#   method = "glmnet",
#   trControl = trainControl("cv", number = 10),
#   tuneLength = 10
# )
# </chatgpt>


# Graficos
coefplot(enet_mdl, lambda=enet.cv$lambda.min,
         col="dodgerblue3",sort="magnitude")+theme_bw()
coefpath(enet_mdl)




####################
## d) Comparacion resultados 
####################


# Comparar los resultados
# Finalmente, comparamos los coeficientes y el desempe?o de los tres modelos:

# Comparar los coeficientes de los tres modelos
cat("Coeficientes del modelo de Ridge:\n")
print(ridge_coefs)

cat("Coeficientes del modelo de Lasso:\n")
print(lasso_coefs)

cat("Coeficientes del modelo de Elastic Net:\n")
print(elastic_net_coefs)

# Comparar el desempe?o de los tres modelos
ridge_rmse <- sqrt(mean((y - predict(ridge_mdl, X, s = best_lambda_ridge))^2))
lasso_rmse <- sqrt(mean((y - predict(lasso_mdl, X, s = best_lambda_lasso))^2))
elastic_net_rmse <- sqrt(mean((y - predict(enet_mdl, X, s = best_elastic_net$lambda))^2))

cat("RMSE del modelo de Ridge:", ridge_rmse, "\n")
cat("RMSE del modelo de Lasso:", lasso_rmse, "\n")
cat("RMSE del modelo de Elastic Net:", elastic_net_rmse, "\n")





tmatrix <- model.matrix(Employed~.,test)[,-1]

ridge.pred = predict(ridge_mdl, s=ridge.cv$lambda.min,
                     newx=tmatrix)
ridge_mse = mean((ridge.pred-test$Employed)**2)    # 0.062
ridge_mse


lasso.pred = predict(lasso_mdl, s=lasso.cv$lambda.min,
                     newx=tmatrix)
lasso_mse = mean((lasso.pred-test$Employed)**2)    # 0.040
lasso_mse


enet.pred = predict(enet_mdl, s=enet.cv$lambda.min,
                    newx=tmatrix)
enet_mse = mean((enet.pred-test$Employed)**2)      # 0.046
enet_mse

# Interpretaci?n de los resultados
# Coeficientes:
#   Ridge: Reduce la magnitud de todos los coeficientes, pero ninguno es exactamente cero.
#   Lasso: Algunos coeficientes pueden ser exactamente cero, lo que implica selecci?n de variables.
#   Elastic Net: Combinaci?n de ambos, dependiendo del par?metro alpha.
# Desempe?o:
#   Comparamos el RMSE de cada modelo para evaluar cu?l tiene mejor capacidad predictiva.
# Este enfoque completo te permitir? ajustar y comparar modelos de Ridge, Lasso y Elastic Net utilizando el conjunto de datos longley.
