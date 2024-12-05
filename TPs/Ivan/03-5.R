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
archivo = "winequality-red.csv"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
df_winered<-fread(paste(ruta,archivo,sep = ""))
dim(df_winered)        #30 3
head(df_winered)

archivo = "winequality-white.csv"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
df_winewhite<-fread(paste(ruta,archivo,sep = ""))
dim(df_winewhite)        #30 3
head(df_winewhite)


####################
## NaN
####################

sum(is.na(df_winered))   # 9582
sum(is.na(df_winewhite)) # 0  




####################
## Arreglar nombres de columnas
####################

names(df_winered) <- iconv(names(df_winered), to = "ASCII", sub = "")
head(df_winered)


names(df_winewhite) <- iconv(names(df_winewhite), to = "ASCII", sub = "")
head(df_winewhite)


####################
## a) Correlograma
####################

cor_red=cor(df_winered[,-c("V13","V14","V15","V16","V17","V18","V19")])
corrplot(cor_red, tl.col = "red", bg = "White", tl.srt = 35, addCoef.col
         = "black", type = "full")

# acidezFija vs Ph = -0.68
# acidezFija vs densidad = 0.67
# acidezFija vs acidoCitrico = 0.67
# acidoCitrico vs pH = -0.54

cor_white <- cor(df_winewhite)
cor_white
corrplot(cor_white, tl.col = "red", bg = "White", tl.srt = 35, addCoef.col
         = "black", type = "full")
# alcohol vs densidad = -0.78
# densidad vs auzarResidual = 0.84


# Conclusion: si tiene sentido aplicar PCA porque hay correlación alta entre variables.


# Realizar un PCA
pca <- prcomp(df_winewhite[,1:11], scale. = TRUE)

# Resumen del PCA
summary(pca)

# Graficar las componentes principales
biplot(pca, scale = 0)





####################
## b) Partir la base en train-test.
####################
set.seed(12345) 
indices_entrenamiento <- sample(x = 1:nrow(df_winewhite), size = 0.75*(nrow(df_winewhite))) 
indices_test <- (1:nrow(df_winewhite))[-indices_entrenamiento] 
data_1 <- df_winewhite[indices_entrenamiento, ] 
data_2 <- df_winewhite[indices_test, ]
x_data_1 <- model.matrix(calidad ~ ., data = data_1)[, -1] 
y_data_1 <- data_1$calidad 
x_data_2 <- model.matrix(calidad ~ ., data = data_2)[, -1] 
y_data_2 <- data_2$calidad









####################
## c) PCR
####################
library(pls) 
set.seed(12345)
# Ajustar el modelo PCR
modelo_pcr <- pcr(calidad ~ ., data = data_1, scale = TRUE, validation = "CV") 
# Resumen del modelo PCR
summary(modelo_pcr)
# Número óptimo de componentes principales
validationplot(modelo_pcr, val.type = "RMSEP")
which.min(x = modelo_pcr$validation$PRESS) # 11


# Obtener puntuaciones originales y ajustadas
train_scores <- predict(modelo_pcr, data_1[,1:11], ncomp = 11)
test_scores <-  predict(modelo_pcr, data_2[,1:11], ncomp = 11)


# Graficar las puntuaciones originales vs ajustadas
plot(data_1$calidad, train_scores, xlab = "Calidad Original", ylab = "Calidad Ajustada (PCR)")




####################
## d) MSE
####################
train_data <- data_1
test_data  <- data_2 

# Calcular el MSE para el subconjunto de componentes
train_pred <- predict(modelo_pcr, train_data, ncomp = 10)
test_pred <-  predict(modelo_pcr, test_data, ncomp = 10)

train_mse <- mean((train_data$calidad - train_pred)^2)
test_mse <-  mean((test_data$calidad - test_pred)^2)

train_mse  # 0.56
test_mse   # 0.58





####################
## e) PLS
####################

# Ajustar el modelo PLS
pls_model <- plsr(calidad ~ ., data = train_data, scale = TRUE, validation = "CV")

# Resumen del modelo PLS
summary(pls_model)

# Validación y comparación
validationplot(pls_model, val.type = "MSEP")
which.min(x = pls_model$validation$PRESS) # 9

# Calcular MSE para el modelo PLS
train_pls_pred <- predict(pls_model, train_data, ncomp = 9)
test_pls_pred <- predict(pls_model, test_data, ncomp = 9)

train_pls_mse <- mean((train_data$calidad - train_pls_pred)^2)
test_pls_mse <- mean((test_data$calidad - test_pls_pred)^2)

train_pls_mse   # 0.55
test_pls_mse    # 0.58







####################
## f) Regresion Logistica
####################

# Re-codificar la variable de calidad
wine_data$quality_binary <- ifelse(wine_data$quality < 5, 0, 1)

# Partición de datos en train y test
set.seed(123)
trainIndex <- createDataPartition(wine_data$quality_binary, p = 0.8, list = FALSE)
train_data <- wine_data[trainIndex, ]
test_data <- wine_data[-trainIndex, ]

# Ajustar el modelo de regresión logística
logistic_model <- glm(quality_binary ~ ., data = train_data[, -12], family = binomial)

# Resumen del modelo
summary(logistic_model)

# Predicciones y evaluación del modelo
test_pred <- predict(logistic_model, test_data[, -c(12, 13)], type = "response")
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)

confusionMatrix(factor(test_pred_class), factor(test_data$quality_binary))


