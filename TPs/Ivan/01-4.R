# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection


library(readxl)
require("data.table")
library(ggplot2)
library(dplyr)
library(MVN)
library(aod)
library(lmtest) 
library(MASS)

####################
## Load
####################
archivo = "energia.xlsx"
setwd("C:/Users/pablo/OneDrive - Universidad Austral/Regresion_Avanzada/Clase 1/practica")
dataset<-read_excel(archivo)
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
attach(dataset)
dt <- as.data.table(dataset)





#########################
## Dispersión 
#########################

par(mfrow=c(2,2)) # Configurar la ventana gráfica para 4 gráficos>
ggplot(data = dataset, aes(x = hora, y = energia)) +
  geom_point(colour = "red4") +
  ggtitle("Hora vs Energia") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(data = dataset, aes(x = energia, y = hora)) +
  geom_point(colour = "red4") +
  ggtitle("Energia vs Hora") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#########################
## Modelo lineal 
#########################

model_1 <- lm(energia ~ hora, data = dt)
summary(model_1)          # Coeficientes NO significativos, R chico


model_3 <- lm(log10(energia) ~ hora, data = dt)
summary(model_3)  

model_4 <- lm(energia ~ log10(hora), data = dt)
summary(model_4)  

model_5 <- lm(log10(energia) ~ log10(hora), data = dt)
summary(model_5)  


# Aplicar la transformación Box-Cox
#boxcox_result <- boxcox(model_1, lambda = seq(-2, 2, by = 0.1))
boxcox_result<- boxcox(energia ~ hora, lambda = seq(-2,2,1/10), plotit = TRUE, data = dt)#lambda cerca de 0
#boxcox_result<- boxcox(Hora ~ Energía, lambda = seq(-2,2,1/10), plotit = TRUE, data = dt)#lambda cerca de 0
# Encontrar el mejor lambda
lambda_optimo <- boxcox_result$x[which.max(boxcox_result$y)]
lambda_optimo # -1.353535

# Cómo el lambda me dió negativo, entonces la transformacion aplico funcion inversa.





model_7 <- lm((1 / (energia ^ lambda_optimo)) ~ hora, data = dt)
summary(model_7)  




dt$hora_transformada <- 1/(cos(dt$hora) ^ 0.5)
dt$energia_transformada <- 1 / (dt$energia ^ (lambda_optimo*-1) )
# model_8 <- lm(energia_transformada ~ Hora, data = dt)
model_8 <- lm(energia_transformada ~ hora_transformada, data = dt)
summary(model_8)          


hist(dt$hora_transformada)

### ME RE CONTRA TRABE ACA TRATANDO DE ENCONTRAR LA FUNCION ADECUADA CON BOXCOX

model_9 <- lm(Energía ~ Hora, data = dt)
summary(model_9)
