rm(list=ls())
library(glmnet)
library(coefplot)

# Crear un conjunto de datos de ejemplo con 10 regresores
set.seed(123)
n <- 100
p <- 10
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("X", 1:p)
Y <- rnorm(n)

# Estandarizar los datos
std <- TRUE

# Ajustar un modelo de Ridge Regression con 10-fold cross-validation
ridge.cv <- cv.glmnet(X, Y, alpha = 0, type.measure = 'mse', family = 'gaussian', standardize = std, nfolds = 10)

# Mostrar los resultados de la validación cruzada
print(ridge.cv)

# Obtener los coeficientes del modelo con el valor óptimo de lambda
coef(ridge.cv, s = 'lambda.min')

# Ajustar el modelo de Ridge Regression con el valor óptimo de lambda
ridge.model <- glmnet(X, Y, alpha = 0, lambda = ridge.cv$lambda.min, standardize = std)

# Mostrar los coeficientes del modelo ajustado
print(coef(ridge.model))


set.seed(100)

train_ind <- sample(1:nrow(mtcars),size = nrow(mtcars)*0.75)
train <- mtcars[train_ind,]
test <- mtcars[-train_ind,]
lm_comp <- lm(mpg~.,data = train)
summary(lm_comp)

lm_comp.pred <- predict(lm_comp,newdata = test)
lm_mse <- mean((lm_comp.pred - test$mpg)^2)
lm_mse

X <- model.matrix(mpg ~ .-1,data = mtcars)
Y <- mtcars$mpg

std <- TRUE
# ajustamos un Regresión de Ridge
#Valor óptimo de lambda por 10 fold cross validation
ridge.cv <- cv.glmnet(X,Y,alpha = 0,type = 'mse', family = 'gaussian', standardize = std, nfolds = 10)
? cv.glmnet()

ridge.cv$lambda.min
print(ridge.cv)

coef(ridge.cv)
# Ajustamos Ridge con 100 opciones de valores para lambda
