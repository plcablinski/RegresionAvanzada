rm(list=ls())
library(glmnet)
library(readxl)
library(dplyr)

setwd('C:/RegresionAvanzada/Clase_5')
bajo <- read_excel('bajo_peso.xlsx')
head(bajo)
# Regresion logistica estimar LOW con SMOKE ambas variables categoricas
model <- glm(LOW ~ SMOKE, data = bajo, family = binomial) 
summary(model)
exp(coef(model))

# Esto significa que las chances de que un bebé tenga bajo peso al nacer son aproximadamente 2.089 veces mayores si la madre fuma 
# en comparación con si no fuma.
# En resumen, la salida del modelo de regresión logística indica 
# que fumar es un predictor significativo de la probabilidad de que un bebé tenga bajo peso al nacer, 
# con un aumento significativo en las chances de bajo peso al nacer si la madre fuma. 
# El modelo ajustado proporciona un buen ajuste a los datos, como lo indican la deviance residual y el AIC.
# La oportunidad de que un bebé tenga bajo peso al nacer es aproximadamente 2.089 veces mayor si la madre fuma en comparación con si no fuma.
# Se estima un aumento del 102.2% del odds de bajo peso al nacer si la madre fuma en comparación con si no fuma.

model2 <- glm(LOW ~ LWT, data = bajo, family = binomial) 
summary(model2)
exp(coef(model2))
# Es decir que la oportunidad de que un bebé tenga bajo peso al nacer es aproximadamente 0.986 veces por cada unidad que aumenta (kilo, libra)
# en el peso de la madre.
anova(model2, test = "Chisq")
1 - pchisq(236.99 - 231.02,1)

model3 <- glm(LOW ~ LWT + SMOKE, data = bajo, family = binomial) 
summary(model3)
exp(coef(model3))
anova(model3)

coef(model)
coef(model3)

anova(model, model3)
anova(model2, model3)

