rm(list=ls())
library(ggplot2)
library(car)
library(carData)
library(corrplot)
library(dplyr)
library(GGally)
data(Duncan)
Duncan
? Duncan
# Generar un gráfico de análisis de cada variable
ggpairs( Duncan, mapping = aes(color = type) )

duncan_lm <- lm(income ~ education, data = Duncan)
summary(duncan_lm)

# Análisis diagnóstico normalidad, heterocedasticidad, no correlacion
# Normalidad
shapiro.test(residuals(duncan_lm))

# Heterocedasticidad
bptest(duncan_lm)

# No correlación
dwtest(duncan_lm)

ggplot(Duncan, aes(education, income)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)


par(mfrow=c(1,1))

qqPlot(duncan_lm, id=0.05, main="QQ Plot")

infIndexPlot(duncan_lm, main="Influence Index Plot")

# Hallar outliers con ajuste Bonferroni
outlierTest(duncan_lm, cutoff = 0.05, order = TRUE, digits = 3)
