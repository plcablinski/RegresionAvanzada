# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection


library(readxl)
require("data.table")
library(ggplot2)
library(dplyr)
library(MVN)

####################
## Load
####################
dataset<-read_excel("c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/anscombe.xlsx")
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
attach(dataset)
dt <- as.data.frame(dataset)


#########################
## Dispersión 
#########################
par(bg="white")
pairs(dt) # representa todos los diagramas de dispersión de a pares



par(mfrow=c(2,2),oma=c(0,0,2,0)) # personaliza el espacio de gráfico
plot(as.numeric(dt$x1),as.numeric(dt$y1),pch=16,col=1,xlab="x1",ylab="y1")
title("x1 vs y1")
plot(as.numeric(dt$x2),as.numeric(dt$y2),pch=16,col=1,xlab="x2",ylab="y2")
title("x2 vs y2")
plot(as.numeric(dt$x3),as.numeric(dt$y3),pch=16,col=1,xlab="x3",ylab="y3")
title("x3 vs y3")
plot(as.numeric(dt$x4),as.numeric(dt$y4),pch=16,col=1,xlab="x4",ylab="y4")
title("x4 vs y4")



par(mfrow=c(2,2)) # Configurar la ventana gráfica para 4 gráficos>
plot(dt$x1, dt$y1, main="Dataset 1", xlab="x1", ylab="y1")
plot(dt$x2, dt$y2, main="Dataset 2", xlab="x2", ylab="y2")
plot(dt$x3, dt$y3, main="Dataset 3", xlab="x3", ylab="y3")
plot(dt$x4, dt$y4, main="Dataset 4", xlab="x4", ylab="y4")





#########################
## Valores medios 
#########################

# Hallar los valores medios de las variables para cada par de datos
means <- data.table(
  dt = 1:4,
  x_mean = c(mean(dt$x1), mean(dt$x2), mean(dt$x3), mean(dt$x4)),
  y_mean = c(mean(dt$y1), mean(dt$y2), mean(dt$y3), mean(dt$y4))
)

# Mostrar los valores medios
print(means)



####################
## Análisis de normalidad de las variables 
####################
shapiro_dt <- data.table(
  x1 = c(shapiro.test(dt$x1)$p.value),       # p-valor=0.8006124 - es normal.
  x2 = c(shapiro.test(dt$x2)$p.value),       # p-valor=0.8006124 - es normal.
  x3 = c(shapiro.test(dt$x3)$p.value),       # p-valor=0.8006124 - es normal.
  #x4 = c(shapiro.test(dt$x4)$p.value),      # No es aplicable shapiro porque tiene todos los valores constantes, ya que no se puede calcular una distribución normal con valores idénticos. 
  y1 = c(shapiro.test(dt$y1)$p.value),       # p-valor=0.9558554 - es normal.
  y2 = c(shapiro.test(dt$y2)$p.value),       # p-valor=0.3313717 - es normal.
  y3 = c(shapiro.test(dt$y3)$p.value),       # p-valor=0.03252205 - NO ES NORMAL.
  y4 = c(shapiro.test(dt$y4)$p.value)        # p-valor=0.8954071 - es normal.
)

head(shapiro_dt)


#########################
## Correleaciones 
#########################

# Hallar los valores medios, dispersión y coeficiente de correlación
results <- data.table(
  dataset = 1:4,
  x_mean = c(mean(dt$x1), mean(dt$x2), mean(dt$x3), mean(dt$x4)),
  y_mean = c(mean(dt$y1), mean(dt$y2), mean(dt$y3), mean(dt$y4)),
  x_var = c(var(dt$x1), var(dt$x2), var(dt$x3), var(dt$x4)),
  y_var = c(var(dt$y1), var(dt$y2), var(dt$y3), var(dt$y4)),
  x_sd = c(sd(dt$x1), sd(dt$x2), sd(dt$x3), sd(dt$x4)),
  y_sd = c(sd(dt$y1), sd(dt$y2), sd(dt$y3), sd(dt$y4)),
  correlation = c(cor(dt$x1, dt$y1,method = "pearson"), cor(dt$x2, dt$y2, method = "pearson"), cor(dt$x3, dt$y3, method = "spearman"), cor(dt$x4, dt$y4,method = "spearman"))
)

# Mostrar los resultados
print(results)



# Conclusion: 
# - fuerte relacion positiva entre x3 y y3. 
# - No se pudo calcular la correlacion de x4 y y4 porque sd de x4 es 0.