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

# Ejemplo puntajes de estudiantes
# Cargar las librerías necesarias
rm(list = ls())
library(car)
library(ggplot2)
# El grupo A tiene las siguientes calificaciones A 13 27 26 22 28 27
# El grupo B tiene las siguientes calificaciones B 43 35 47 32 31 37
# El grupo C tiene las siguientes calificaciones C 33 33 33 26 44 33 54
# Crear un conjunto de datos tener en cuenta que los grupos deben ser factores y que a y b tienen 6 calificaciones y c tiene 7
# Crear las calificaciones para cada grupo
calificaciones_A <- c(13, 27, 26, 22, 28, 27)
calificaciones_B <- c(43, 35, 47, 32, 31, 37)
calificaciones_C <- c(33, 33, 33, 26, 44, 33, 54)

# Crear un vector de grupos correspondiente
grupos <- factor(c(rep("A", length(calificaciones_A)), 
                   rep("B", length(calificaciones_B)), 
                   rep("C", length(calificaciones_C))))

# Combinar las calificaciones en un solo vector
calificaciones <- c(calificaciones_A, calificaciones_B, calificaciones_C)

# Crear un data frame con los grupos y las calificaciones
datos <- data.frame(Grupo = grupos, Calificaciones = calificaciones)

# Mostrar el data frame
print(datos)
# Graficar los datos con un boxplot
ggplot(datos, aes(x = Grupo, y = Calificaciones, fill = Grupo)) + 
  geom_boxplot() +
  theme_minimal()

modelo <- aov(Calificaciones ~ Grupo, data = datos)
modelo
summary(modelo)
# Testear normalidad de los residuos
shapiro.test(residuals(modelo))
#Testear heterocedasticidad
library(car)
leveneTest(modelo)
#Test de Kruskal-Wallis
kruskal.test(Calificaciones ~ Grupo, data = datos)
# Hacer prueba de comparaciones múltiples test after Kruskal-Wallis 

library(pgirmess)
kruskalmc(calificaciones ~ grupos)
