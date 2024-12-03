# Crear datos de ejemplo
rm(list = ls())
grupo <- factor(rep(1:4, each = 10))
set.seed(123)
respuesta <- c(rnorm(10, mean = 5), rnorm(10, mean = 5.3), rnorm(10, mean = 5.6), rnorm(10, mean = 6))
# Ajustar un modelo lineal
modelo <- lm(respuesta ~ grupo)
summary(modelo)
# Realizar ANOVA
resultado_anova <- anova(modelo)

# Mostrar resultados
print(resultado_anova)

# Ejemplo del té
# Se desea comparar el contenido de cafeína en 4 tipos de té
# Se mide el contenido de cafeína en 10 muestras de cada tipo de té
# Se desea saber si hay diferencias significativas en el contenido de cafeína entre los tipos de té
# Crear datos de ejemplo
# el primer factor tiene 7 elementos, el segundo 6, el tercero 6 y el cuarto 6
# Crear la variable grupo
grupo <- factor(c(rep(1, 7), rep(2, 6), rep(3, 6), rep(4, 6)))
datos <- c(7.9, 6.2, 6.6, 8.6, 8.9, 10.1, 9.6, 5.7, 7.5,9.8, 6.1, 8.4, 7.2, 6.8, 7.8, 5.1, 7.4, 5.3, 6.1, 6.4, 7.1, 7.9, 4.5, 5.0, 4.0)
tabla <- data.frame(grupo, datos)
tabla
# Calcular la media y el desvío estándar de cada grupo
aggregate(datos ~ grupo, data = tabla, FUN = function(x) c(media = mean(x), desvio = sd(x)))
modelo <- lm(datos ~ grupo)
summary(modelo)
anova(modelo)

# Diagnosticar el modelo
library(ggplot2)
# Graficar boxplot con colores para cada grupo
ggplot(tabla, aes(x = grupo, y = datos, fill = grupo)) + geom_boxplot()

# Inspección analitica de la Homocedasticidad
# Test de Bartlett
bartlett.test(datos ~ grupo, data = tabla)
# El test de Bartlett no es significativo, por lo que no se rechaza la hipótesis nula de igualdad de varianzas
# Ante falta de normalidad Bartlett tiende a rechazar

# El test de  Levenne es robusto por lo que es preferible
# Test de Levene
library(car)
leveneTest(datos ~ grupo, data = tabla)

# Realizar test de normalidad sobre los residuos
# Shapiro Wilk
shapiro.test(residuals(modelo))

# Anderson Darling
library(nortest)
ad.test(residuals(modelo))

# D'Agostino
library(moments)
agostino.test(residuals(modelo))

# No se rechaza normalidad

# Dibujar qqPlot
qqnorm(residuals(modelo))
qqline(residuals(modelo))
qqPlot(residuals(modelo))

