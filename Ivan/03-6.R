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
library(corrplot)
library(coefplot)
library(AppliedPredictiveModeling)
library(pls)
library(naniar)




####################
## Load
####################

# Cargar los datos
data(ChemicalManufacturingProcess)
dim(ChemicalManufacturingProcess)     # 176  58
head(ChemicalManufacturingProcess)
df = ChemicalManufacturingProcess






####################
## a) 
####################


# Visualizar las primeras filas del dataset
head(df)

# Resumen de los datos
summary(df)
str(df)   # 58 variables numericas.


# Identificación de valores faltantes
sum(is.na(df))    # 106 valores 
vis_miss(df)     # Tenemos un 1% de datos faltantes: 176 - 152 = 24 valores faltantes.
#### qué columnas son las que tienen datos faltantes??? método de imputación????




# Reemplazar o eliminar valores faltantes
# En este ejemplo, simplemente eliminamos las filas con valores faltantes
# pero podríamos considerar imputar valores faltantes
clean_data <- na.omit(df)



# Verificar nuevamente los datos después de la limpieza
vis_miss(clean_data)
dim(clean_data)     # 152  58
summary(clean_data)
str(clean_data)
head(clean_data)


### aca hacer analisis de correlacion de variables para ver si tiene sentido aplicar pca
### aplicar pca si es necesario y quedarse con componentes principaales


cor_matrix <- cor(clean_data[, -which(names(clean_data) == "Yield")], use = "complete.obs")

# Filtrar las correlaciones que superan el umbral de 0.7
high_cor_matrix <- cor_matrix
high_cor_matrix[abs(high_cor_matrix) < 0.7] <- NA


high_cor_matrix

# Visualizar la matriz de correlaciones filtrada
corrplot(high_cor_matrix, method = "color", type = "upper", 
         na.label = " ", tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         diag = FALSE)



# Realizar un PCA
pca <- prcomp(clean_data[, -which(names(clean_data) == "Yield")], scale. = TRUE)
# Resumen del PCA
summary(pca)  # con 15 xeplicas mas del 80%
# Graficar las componentes principales
biplot(pca, scale = 0)




####################
## b) 
####################
# Separar los datos en variables predictoras y respuesta
# X <- clean_data[, -which(names(clean_data) == "Yield")]     # Me quedo con los predictores.
# y <- clean_data$Yield         # Me quedo con la variable respuesta
# 
# # Partición de datos en train y test
# set.seed(12345)
# trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
# train_predictors <-  X[trainIndex, ]
# test_predictors  <-  X[-trainIndex, ]
# train_response   <-  y[trainIndex]
# test_response    <-  y[-trainIndex]

set.seed(12345) 
indices_entrenamiento <- sample(x = 1:nrow(clean_data), size = 0.8*(nrow(clean_data))) 
indices_test <- (1:nrow(clean_data))[-indices_entrenamiento] 
train <- clean_data[indices_entrenamiento, ] 
test <- clean_data[indices_test, ]
x_train <- model.matrix(Yield ~ ., data = train)[, -1] 
y_train <- train$Yield 
x_test <- model.matrix(Yield ~ ., data = test)[, -1] 
y_test <- test$Yield
### <Pregunta> Por qué usa model.matrix ???????? y no directamente obtener el x  </Pregunta>



# Ajustar el modelo PCR
modelo_pcr <- pcr(Yield ~ ., data = train, scale = TRUE, validation = "CV")
# Resumen del modelo PCR
summary(modelo_pcr)
# Número óptimo de componentes principales
validationplot(modelo_pcr, val.type = "RMSEP")
which.min(x = modelo_pcr$validation$PRESS) # 44

# Calcular el MSE para el modelo PCR
pcr_pred <- predict(modelo_pcr, test, ncomp = 44)
pcr_mse  <- mean((y_test - pcr_pred)^2)
pcr_mse    # 1.21





# Ajustar el modelo PLS
pls_model <- plsr(Yield ~ ., data = train, scale = TRUE, validation = "CV")
# Resumen del modelo PLS
summary(pls_model)
# Validación y número óptimo de componentes para PLS
validationplot(pls_model, val.type = "RMSEP")
which.min(x = pls_model$validation$PRESS) # 1
# Calcular el MSE para el modelo PLS
pls_pred <- predict(pls_model, test, ncomp = 10)
pls_mse  <- mean((y_test - pls_pred)^2)
pls_mse    # 1.23

# Comparación de resultados
pcr_mse
pls_mse







# Interpretación y comparación de resultados:
# - PCR (Principal Component Regression): Este método reduce la dimensionalidad de los datos
# transformando las variables predictoras originales en un nuevo conjunto de componentes principales 
# que son ortogonales entre sí. El número óptimo de componentes se puede determinar usando la validación cruzada.
# 
# - PLS (Partial Least Squares): Este método también reduce la dimensionalidad, pero a diferencia de PCR, 
# encuentra componentes que tienen en cuenta tanto las variables predictoras como la variable de respuesta. 
# Esto a menudo conduce a un mejor ajuste y predicciones más precisas.
# 
# 
# La comparación de los modelos PCR y PLS se realiza principalmente observando el Mean Squared Error (MSE) 
# de las predicciones en el conjunto de prueba. El método con el menor MSE generalmente se considera el mejor 
# en términos de precisión de predicción.
# 
# Asegúrate de ajustar el número de componentes principales (ncomp) en función de los resultados de la validación cruzada,
# ya que el número óptimo puede variar en función de los datos específicos.





