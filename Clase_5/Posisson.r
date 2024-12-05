rm(list=ls())
library(MASS)
library(dplyr)

data(ships)
str(ships)
ships
? ships
datos_ships <- ships %>% filter(service != 0)
fit <- glm(incidents ~ ., datos_ships, family = poisson) 
summary(fit)

coef(fit)# podemos pedir solamente los coeficientes del modelo

# comparamos la deviance del modelo con la del modelo nulo, para ver si es signficativo
dev<- fit$deviance
dev
nullDev <- fit$null.deviance
nullDev
modelChi <- nullDev - dev
modelChi # calculamos la diferencia de deviances
chidf <- fit$df.null - fit$df.residual # calculamos los grados de libertad de la diferencia
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob

# Caluclar la deviance residual y los grados de libertad
resDev <- fit$deviance
resDev
gl <- fit$df.residual
gl

r2pseudo <- (nullDev - resDev) / nullDev
r2pseudo

stepAIC(fit)

fit_hint<- glm(incidents ~ offset(log(service))+type+year+period,data = datos_ships, family = poisson)

summary(fit)
summary(fit_hint)
