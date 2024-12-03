rm(list = ls())
# Cargar las librerías necesarias
library(car)
library(ggplot2)

# Crear un conjunto de datos
set.seed(123)
data <- data.frame(
  group = factor(rep(c("A", "B", "C"), each = 20)),
  value = c(rnorm(20, mean = 100, sd = 2), 
            rnorm(20, mean = 100, sd = 5), 
            rnorm(20, mean = 100, sd = 10))
)
summary(data)

# Graficar los datos para visualizar la distribución
ggplot(data, aes(x = group, y = value)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", color = "red") +
  theme_minimal()

modelo <- lm(value ~ group, data = data)
summary(modelo)
anova(modelo)

# Comprobar la normalidad de los residuos
shapiro.test(residuals(modelo))
# Hacer un histograma de los residuos
hist(residuals(modelo))
# Graficar los residuos
plot(modelo, which = 1)

# Comprobar la homogeneidad de varianzas
leveneTest(value ~ group, data = data)

# Realizar ANOVA
anova_result <- aov(value ~ group, data = data)
summary(anova_result)

powerTransform(value ~ group, data = data)
modelo1 <- lm(value^-0.73 ~ group, data = data)
summary(modelo1)
# Comprobar la normalidad de los residuos
shapiro.test(residuals(modelo1))
# Hacer un histograma de los residuos
hist(residuals(modelo1))
# Graficar los residuos
plot(modelo1, which = 1)
# Comprobar la homogeneidad de varianzas
leveneTest(value^-0.73 ~ group, data = data)


# Dado que los supuestos de ANOVA no se cumplen, usar la prueba Kruskal-Wallis
kruskal.test(value ~ group, data = data)
