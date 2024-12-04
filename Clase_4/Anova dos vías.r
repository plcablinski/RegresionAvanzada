rm(list=ls())
# Anova de dos factores
resistencia <- c(15.29, 15.89, 16.02, 16.56, 15.46, 16.91, 16.99,17.27, 16.85, 16.35, 17.23, 17.81, 17.74, 18.02, 18.37, 12.07, 12.42,
12.73, 13.02, 12.05, 12.92, 13.01, 12.21, 13.49, 14.01, 13.30, 12.82,12.49, 13.55, 14.53)
templado <- (rep(c('rapido', 'lento'), c(15,15)))
grosor<- rep(c(8, 16, 24), each = 5, times = 2)
datos<- data.frame(templado = as.factor(templado), grosor = as.factor(grosor), resistencia = resistencia)
head(datos)
# Graficar los datos con boxplot por templado y grosor incorporando intervalos de confianza
par(mfrow = c(1,2))
# Calcular los intervalos de confianza
library(dplyr)
library(gridExtra)

p1 <- ggplot(data = datos, aes(x = templado, y = resistencia,fill=templado)) + geom_boxplot() + theme_bw()+scale_fill_brewer(palette='Dark2')
p2 <- ggplot(data = datos, aes(x = grosor, y = resistencia, fill =grosor)) + geom_boxplot() + theme_bw()+scale_fill_brewer(palette='Dark2')
p3 <- ggplot(data = datos, aes(x = templado, y = resistencia, colour = grosor)) + geom_boxplot() + theme_bw()+scale_fill_brewer(palette='Dark2')
grid.arrange(p1, p2, ncol = 2)
p3

with(data = datos,expr = tapply(resistencia, templado, mean))
with(data = datos,expr = tapply(resistencia, templado, sd))
with(data = datos,expr = tapply(resistencia, grosor, mean))
with(data = datos,expr = tapply(resistencia, grosor, sd))
with(data = datos,expr = tapply(resistencia, list(templado,grosor), mean))
with(data = datos,expr = tapply(resistencia, list(templado,grosor), sd))

ggplot(data = datos, aes(x = templado, y = resistencia, colour = grosor, group = grosor)) + 
stat_summary(fun = mean, geom = 'point') + stat_summary(fun = mean, geom = 'line') + labs(y = 'mean (resistencia)') + theme_bw()

# Hacer un grafico de interaccion 
ggplot(data = datos, aes(x = grosor, y = resistencia, colour = templado, group = templado)) +
stat_summary(fun = mean, geom = 'point') + stat_summary(fun = mean, geom = 'line') + labs(y = 'mean (resistencia)') + theme_bw()

interaction.plot(datos$grosor, datos$templado, datos$resistencia, col = c('red', 'blue'), lty = c(1,2), pch = c(1,2), xlab = 'Grosor', ylab = 'Resistencia', trace.label = 'Templado')
interaction.plot(datos$templado, datos$grosor, datos$resistencia, col = c('red', 'blue'), lty = c(1,2), pch = c(1,2), xlab = 'Templado', ylab = 'Resistencia', trace.label = 'Grosor')

modelo_lineal <- lm(resistencia ~ templado*grosor, data = datos)
anova(modelo_lineal)
anova <- aov(resistencia ~ templado*grosor, data = datos)
summary(anova)

library(lsr)
etaSquared(anova)
