# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection


library(readxl)
require("data.table")
library(ggplot2)
library(dplyr)
library(MVN)
library(aod)
library(lmtest) 
library(MASS)
library(car)
library(carData)
library(nortest)
library(ggplot2)
library(GGally)

####################
## Load
####################
data(Salaries)         ### Es una dataset que viene por defecto.




####################
## Visualizar asociacion
####################

# Visualizar la asociación entre las variables de a pares
pairs(Salaries)
# O, usando GGally
ggpairs(Salaries)




####################
## a) Ajustar un modelo lineal salary ~ sex
####################


model_sex <- lm(salary ~ sex, data = Salaries)
summary(model_sex)




####################
## b) Ajustar un modelo lineal salary ~ yrs.service
####################



model_service <- lm(salary ~ yrs.service, data = Salaries)
summary(model_service)



####################
## c) Encontrar el mejor modelo lineal con dos variables
####################

# Ajustar modelos con dos variables
model_sex_service <- lm(salary ~ sex + yrs.service, data = Salaries)
model_sex_phd <- lm(salary ~ sex + yrs.since.phd, data = Salaries)
model_service_phd <- lm(salary ~ yrs.service + yrs.since.phd, data = Salaries)

# Resúmenes de los modelos
summary(model_sex_service)
summary(model_sex_phd)
summary(model_service_phd)

# Comparar modelos con interacción
model_sex_service_interaction <- lm(salary ~ sex * yrs.service, data = Salaries)
model_sex_phd_interaction <- lm(salary ~ sex * yrs.since.phd, data = Salaries)
model_service_phd_interaction <- lm(salary ~ yrs.service * yrs.since.phd, data = Salaries)

# Resúmenes de los modelos con interacción
summary(model_sex_service_interaction)
summary(model_sex_phd_interaction)
summary(model_service_phd_interaction)

# Comparar AIC de los modelos
AIC(model_sex_service, model_sex_phd, model_service_phd,
    model_sex_service_interaction, model_sex_phd_interaction, model_service_phd_interaction)




####################
## d) Ajustar el modelo completo
####################

# Ajustar el modelo completo
model_full <- lm(salary ~ rank + discipline + yrs.since.phd + yrs.service + sex, data = Salaries)
summary(model_full)



####################
## e) Proponer un modelo y justificar su superioridad
####################

# Supongamos que seleccionamos el modelo con sex y yrs.service sin interacción
selected_model <- model_sex_service

# Realizar diagnóstico del modelo seleccionado
par(mfrow = c(2, 2))
plot(selected_model)

# Estimar intervalos de confianza para los coeficientes del modelo seleccionado
confint(selected_model, level = 0.95)

# Justificación
# Basado en el AIC y la significancia de los coeficientes, elegimos el modelo con sex y yrs.service
# porque proporciona un buen equilibrio entre simplicidad y ajuste.
# También observamos los gráficos de diagnóstico para evaluar la adecuación del modelo.


