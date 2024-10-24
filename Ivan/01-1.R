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
dataset<-read_excel("c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/grasacerdos.xlsx")
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
attach(dataset)
dt <- as.data.table(dataset)
dt[, EGD := gsub(",", ".", EGD)]
dt[, PV := gsub(",", ".", PV)]

dt[, EGD := gsub(",", ".", EGD)]
dt[, PV := gsub(",", ".", PV)]

dt[, EGD := as.numeric(EGD)]
dt[, PV := as.numeric(PV)]
head(dt)

#########################
## Dispersión 
#########################


plot(as.numeric(dt$EGD),as.numeric(dt$PV),pch=16,col=1,xlab="EGD",ylab="PV")
title("EGD vs PV")

ggplot(data = dataset, aes(x = EGD, y = PV)) +
  geom_point(colour = "red4") +
  ggtitle("PV vs EGD") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


# No se observa correlacion lineal.
# Al observar el diagrama de dispersión, podemos ver la relación entre el peso vivo (PV) 
# y el espesor de grasa dorsal (EGD) de los lechones. Si los puntos en el gráfico muestran
# una tendencia ascendente (es decir, a medida que aumenta el peso vivo, también aumenta el espesor de grasa dorsal),
# podemos sospechar una asociación positiva entre estas dos variables. 
# Si no se observa una tendencia clara, puede que no haya una relación lineal entre las dos variables.



####################
## Correllacion
####################

cor(dt$EGD,dt$PV)     # 0.2543434


cor(dt$EGD,dt$PV,method="pearson")        # 0.2543434
cor.test(dt$EGD,dt$PV,method="pearson")   # 0.2543434


# El coeficiente es muy bajo y positivo por lo que indica asociacion positiva. 




####################
## Análisis de normalidad de las variables 
####################

par(mfrow = c(1, 2)) 
hist(dt$EGD, breaks = 10, main = "", xlab = "EGD", border = "darkred") 
hist(dt$PV, breaks = 10, main = "", xlab = "PV", border = "blue")



qqnorm(dt$EGD, main = "EGD", col = "darkred") 
qqline(dt$EGD) 
qqnorm(dt$PV, main = "PV", col = "blue") 
qqline(dt$PV)



        



# Verificar los supuestos de normalidad
shapiro_pv <- shapiro.test(dt$PV)       # p-valor=0.9395 - es normal.
shapiro_egd <- shapiro.test(dt$EGD)     # p-valor=0.6925 - es normal.





# Evaluar el resultado de las pruebas de normalidad
if (shapiro_pv$p.value > 0.05 & shapiro_egd$p.value > 0.05) {
  cat("Ambas variables son normales. Utilizaremos el coeficiente de correlación de Pearson.\n")
  
  # Realizar la prueba de correlación de Pearson
  cor_test <- cor.test(dt$PV, dt$EGD, method = "pearson")
} else {
  cat("Alguna de las variables no es normal. Utilizaremos el coeficiente de correlación de Spearman.\n")
  
  # Realizar la prueba de correlación de Spearman
  cor_test <- cor.test(dt$PV, dt$EGD, method = "spearman")
}



# Interpretar el resultado: 
# If the p-value is < 5%, then the correlation between x and y is significant.
if (cor_test$p.value < 0.05) {
  cat("Hay suficiente evidencia para admitir una asociación significativa entre el peso vivo y el espesor de grasa dorsal (α = 0.05).\n")
} else {
  cat("No hay suficiente evidencia para admitir una asociación significativa entre el peso vivo y el espesor de grasa dorsal (α = 0.05).\n")
}





#Usamos Test Henze-Zirkler para evaluar normalidad multivariada (bivariada en este caso)
respuesta_testHZ<-mvn(dt[,c("EGD", "PV")] , mvnTest = "hz")
respuesta_testHZ              # p-valor=0.9049686 por lo tanto es normal bivariada.
