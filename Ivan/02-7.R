# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# Instalar y cargar la librer?a necesaria
library(Brq)
library(readxl)
require("data.table")
library(ggplot2)
library(dplyr)
library(MVN)
library(aod)
library(lmtest) 
library(MASS)
library(car)
library(nortest)
library(ggplot2)
library(GGally)
library(quantreg)
library(corrplot)

####################
## Load
####################

# Cargar los datos de USgirl
data("USgirl")
head(USgirl)



####################
## a) Peso vs Edad
####################

result <- mvn(USgirl , mvnTest = "hz")
result
cor(USgirl$Age, USgirl$Weight, method = "spearman")


M=cor(USgirl)
corrplot(M, tl.col = "red", bg = "White", tl.srt = 35, addCoef.col
         = "black", type = "full")


# Graficar peso vs edad
ggplot(USgirl, aes(x = Age, y = Weight)) +
  geom_point(alpha = 0.3) +
  labs(title = "Peso vs Edad de Mujeres en EE.UU.",
       x = "Edad",
       y = "Peso") +
  theme_minimal()


plot(USgirl$Age, USgirl$Weight, 
     xlab = "Edad", ylab = "Peso", 
     main = "Diagrama de dispersión de Peso vs Edad",
     pch = 19, col = "blue")

# En este diagrama de dispersión, se puede apreciar cómo se distribuyen 
# los pesos en función de las edades de las mujeres en la base de datos.



####################
## b) Modelo para mediana
####################

# Para ajustar un modelo de regresión cuantilica para la mediana (cuantil 0.5),
# usaremos la libreria quantreg.

# Ajustar el modelo de regresion cuantilica para la mediana
modelo_mediana <- quantreg::rq(Weight ~ Age, tau = 0.5, data = USgirl)
# Otra forma:
fit_mediana <- rq(Weight ~ Age, data = USgirl, tau = 0.5)



# Graficar los datos y la mediana ajustada
plot(USgirl$Age, USgirl$Weight, 
     xlab = "Edad", ylab = "Peso", 
     main = "Diagrama de dispersión con mediana ajustada",
     pch = 19, col = "blue")
# Añadir la línea de la mediana ajustada
abline(fit_mediana, col = "red", lwd = 2)



# Graficar los datos y la regresión cuantilica para la mediana
# ggplot(USgirl, aes(x = Age, y = Weight)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "rq", formula = Weight ~ Age, data = USgirl, method.args = list(tau = 0.5), col = "blue") +
#   labs(title = "Peso vs Edad con Regresion Cuantilica para la Mediana",
#        x = "Edad",
#        y = "Peso") +
#   theme_minimal()







####################
## c) Modelo para cuartiles
####################

# Ajustaremos modelos de regresión cuantilica para los cuartiles (0.25 y 0.75).
# El parámetro tau indica el cuantil que se desea estimar, que ha de ser, 
# por lo tanto, un valor entre 0 y 1.

# Ajustar modelos de regresión cuantilica para los cuartiles
modelo_cuartil_25 <- rq(Weight ~ Age, tau = 0.25, data = USgirl)
modelo_cuartil_75 <- rq(Weight ~ Age, tau = 0.75, data = USgirl)

# Otra forma
fit_q1 <- rq(Weight ~ Age, data = USgirl, tau = 0.25)
fit_q3 <- rq(Weight ~ Age, data = USgirl, tau = 0.75)


# Graficar los datos y los cuartiles ajustados
plot(USgirl$Age, USgirl$Weight, 
     xlab = "Edad", ylab = "Peso", 
     main = "Diagrama de dispersión con cuartiles ajustados",
     pch = 19, col = "blue")

# Añadir las líneas de los cuartiles ajustados
abline(fit_q1, col = "green", lwd = 2, lty = 2)
abline(fit_q3, col = "yellow", lwd = 2, lty = 2)



# Graficar los datos y las regresión cuantilicas para los cuartiles
# ggplot(USgirl, aes(x = Age, y = Weight)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "rq", formula = y ~ x, method.args = list(tau = 0.25), col = "red") +
#   geom_smooth(method = "rq", formula = y ~ x, method.args = list(tau = 0.75), col = "green") +
#   labs(title = "Peso vs Edad con Regresion Cuantilica para los Cuartiles",
#        x = "Edad",
#        y = "Peso") +
#   theme_minimal()





####################
## d) Modelo para deciles
####################

# Ajustaremos modelos de regresi?n cuant?lica para los deciles (0.1, 0.2, ..., 0.9).

# Ajustar modelos de regresi?n cuant?lica para los deciles
taus <- seq(0.1, 0.9, by = 0.1)
modelos_deciles <- lapply(taus, function(tau) rq(Weight ~ Age, tau = tau, data = USgirl))

# Graficar los datos y las regresiones cuant?licas para los deciles
# ggplot(USgirl, aes(x = Age, y = Weight)) +
#   geom_point(alpha = 0.3) +
#   lapply(1:length(taus), function(i) {
#     geom_smooth(method = "rq", formula = y ~ x, method.args = list(tau = taus[i]), col = "grey", se = FALSE)
#   }) +
#   labs(title = "Peso vs Edad con Regresi?n Cuant?lica para los Deciles",
#        x = "Edad",
#        y = "Peso") +
#   theme_minimal()



fit_d1 <- rq(Weight ~ Age, data = USgirl, tau = 0.1)
fit_d9 <- rq(Weight ~ Age, data = USgirl, tau = 0.9)


# Graficar los datos y los deciles ajustados
plot(USgirl$Age, USgirl$Weight, 
     xlab = "Edad", ylab = "Peso", 
     main = "Diagrama de dispersión con deciles ajustados",
     pch = 19, col = "blue")

# Añadir las líneas de los deciles ajustados
abline(fit_d1, col = "orange", lwd = 2, lty = 3)
abline(fit_d9, col = "brown", lwd = 2, lty = 3)


# Resultados esperados
# Diagrama de dispersion: Mostrara la relacion entre la edad y el peso de las mujeres en el conjunto de datos.
# Modelo para la mediana: Incluira una linea de regresion que represente la mediana del peso en funcion de la edad.
# Modelos para los cuartiles: Incluira lineas de regresion que representen los cuartiles (25% y 75%) del peso en funcion de la edad.
# Modelos para los deciles: Incluira lineas de regresion que representen los deciles (10%, 20%, ..., 90%) del peso en funcion de la edad.
# Estos pasos proporcionan una manera completa y detallada de ajustar y visualizar modelos de regresion cuantilica para analizar
# la relacion entre la edad y el peso en el conjunto de datos de USgirl.


# Interpretación
# Diagrama de dispersión inicial: Muestra la relación general entre edad y peso.
# Es posible observar si hay alguna tendencia o patrón visible, como un aumento del peso con la edad.

# Mediana ajustada: La línea roja en el gráfico indica la mediana del peso para cada edad, 
# mostrando la tendencia central.

# Cuartiles ajustados: Las líneas verde y púrpura representan el primer y tercer cuartil 
# respectivamente, proporcionando información sobre la dispersión y la variabilidad 
# de los datos alrededor de la mediana.

# Deciles ajustados: Las líneas naranja y marrón muestran los deciles 10 y 90, 
# proporcionando una visión más detallada de la distribución de los pesos en 
# relación con la edad y permitiendo identificar posibles valores atípicos.

# Estos modelos de regresión cuantílica permiten una mejor comprensión de cómo 
# se distribuyen los pesos de las mujeres en función de sus edades, proporcionando 
# una visión más completa que un modelo de regresión lineal simple.
