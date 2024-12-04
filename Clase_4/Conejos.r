rm(list=ls())
par(mfrow=c(1,1))

# Dieta conejos
# Identificar si las dietas son diferentes

dieta <- factor(c(rep("1", 8), rep("2", 8), rep("3", 8)))
dieta
colesterol <- c(13.4, 11, 15.3, 16.7, 13.4, 20.1, 13.6,18.3, 10.4, 14.2, 20.5, 19.6, 18.5, 24, 23.4, 13.6, 7.5, 7.2, 6.7, 7.6, 11.2, 9.6,6.8, 8.5)
datos <- data.frame(dieta, colesterol)
modelo <- lm(colesterol ~ dieta, data=datos)
summary(modelo)
anova.modelo <- anova(modelo)
anova.modelo

# Por cada dieta, calcular el promedio y la desviación estándar
aggregate(colesterol ~ dieta, data=datos, mean)
aggregate(colesterol ~ dieta, data=datos, sd)
mean(colesterol)
sd(colesterol)
library(ggplot2)
# Hacer un boxplot con colores por dieta
ggplot(datos, aes(x=dieta, y=colesterol, fill=dieta)) + geom_boxplot()

# Verificar los supuestos del ANOVA
# 1. Normalidad
shapiro.test(residuals(modelo))

library(nortest)
ad.test(residuals(modelo))
qqPlot(modelo)
# Normalidad se cumple

# 2. Homocedasticidad
par(mfrow=c(2,2))
plot(modelo)
par(mfrow=c(1,1))
# Test de Levene
library(car)
leveneTest(modelo)
# Test de Bartlett
bartlett.test(colesterol ~ dieta, data=datos)

# Hago una transformación de Box Cox sobre la variable colesterol
library(MASS)
boxcox(modelo)
powerTransform(modelo)

modelo1 <- lm(colesterol^-0.5 ~ dieta, data=datos)
summary(modelo1)
anova(modelo1)

# Verificar los supuestos del ANOVA
# 1. Normalidad
shapiro.test(residuals(modelo1))
ad.test(residuals(modelo1))
qqPlot(modelo1)
# Normalidad se cumple

# 2. Homocedasticidad
par(mfrow=c(2,2))
plot(modelo1)
par(mfrow=c(1,1))
# Test de Levene
leveneTest(modelo1)
# Test de Bartlett
bartlett.test(colesterol^-0.5 ~ dieta, data=datos)

# Test de Tukey
v_aov <- aov(colesterol^-0.5 ~ dieta, data=datos)
TukeyHSD(v_aov)

# 