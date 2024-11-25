# Testear normalidad de las variables X y W multivariadas.
# Si hay normalidad multivariada testeada con Henze-Zirkler's Multivariate Normality Test
# entonces se puede usar la correlación de Pearson.
library(MVN)
X = c(60,61,61,62,63,64,65,68,70)
W = c(125,130,120,135,130,140,140,160,169)
data = data.frame(X,W)
result = mvn(data, mvnTest = "hz") # Henze-Zirkler's Multivariate Normality Test
result$multivariateNormality
cor(X,W)
cor.test(X,W)

# Si no hay normalidad multivariada, se puede usar la correlación de Spearman.
x <- c(196.5, 199.1, 199.9, 204.2, 204.2, 207.4, 234.1, 181.7, 183, 192.8)
y <- c(0.76, 1.11, 1.66, 0.96, 1.21, 1.14, 1.53, 1.51, 1.28, 0.84)
data = data.frame(x,y)
result = mvn(data, mvnTest = "hz") # Henze-Zirkler's Multivariate Normality Test
result$multivariateNormality
cor.test(x, y, method = "spearman")

# Cuando los valores de ciertas variables que denominaremos regresoras o 
# explicativas o predictoras nos permite aproximar el valor de una variable de
# interés que denominaremos variable objetivo o respuesta pero, aún así, no
# nos permite determinarlo con exactitud tendrá sentido definir un modelo
# de regresión

# el residuo es la diferencia entre el valor observado y el valor ajustado
# es positivo si el valor observado es mayor que el valor ajustado
# es negativo si el valor observado es menor que el valor ajustado
# ei = valor observadoi − valor ajustado i 

# La suma de cuadrados totales es la suma de los cuadrados de las diferencias
# entre los valores observados y el promedio de los valores observados.