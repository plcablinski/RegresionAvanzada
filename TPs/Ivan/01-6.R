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
library(car)
library(nortest)


####################
## Load
####################
archivo = "estudio.csv"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-fread(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
#attach(dataset)
dt <- dataset



#########################
## Dispersión 
#########################
ggplot(data = dataset, aes(x = horas_estudio, y = puntaje)) +
  geom_point(colour = "red4") +
  ggtitle("Edad vs Colesterol") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))




####################
## Análisis de normalidad de las variables 
####################

par(mfrow = c(1, 2)) 
hist(dt$horas_estudio, breaks = 10, main = "", xlab = "EGD", border = "darkred") 
hist(dt$puntaje, breaks = 10, main = "", xlab = "PV", border = "blue")



qqnorm(dt$horas_estudio, main = "EGD", col = "darkred") 
qqline(dt$horas_estudio) 
qqnorm(dt$puntaje, main = "PV", col = "blue") 
qqline(dt$puntaje)





# Verificar los supuestos de normalidad
shapiro.test(dt$horas_estudio)       # p-valor=0.1146 - es normal.
shapiro.test(dt$puntaje)             # p-valor=0.8375 - es normal.



#Usamos Test Henze-Zirkler para evaluar normalidad multivariada (bivariada en este caso)
respuesta_testHZ<-mvn(dt[,c("horas_estudio", "puntaje")] , mvnTest = "hz")
respuesta_testHZ              # p-valor=0.00368416 por lo tanto es NO normal bivariada.
respuesta_testHZ$multivariateNormality

# Como no es normal bivariada, debemos usar correlación de SPEARMAN


####################
## Correllacion
####################



cor(dt$horas_estudio,dt$puntaje,method="pearson")        # 0.7205223
cor(dt$horas_estudio,dt$puntaje,method="spearman")       # 0.664846




#########################
## Modelo Lineal
#########################



model_1 <- lm(puntaje ~ horas_estudio, data = dt)
summary(model_1)          # Coeficientes significativos, R grande.




###########################
## Supuesto 1 : Normalidad
###########################

residuos=residuals(model_1)

shapiro.test(residuos)         # p-value = 0.01196     No es normal
ad.test(residuos)              # p-value = 0.09429     Es normal
lillie.test(residuos)          # p-value = 0.241       Es normal

 

qqPlot(residuos, pch=19,
       main="QQplot para los residuos del Modelo Lineal",
       xlab="Cuantiles teóricos",
       ylab="Cuantiles muestrales")

# Segun el grafico... hay varios puntos ...




##############################
## Supuesto 2 : Independencia
##############################

dwtest(model_1,alternative
       ="two.sided",iterations = 1000)            # p-value = 0.477  Hay independencia




ggplot(dt,aes(x=horas_estudio,y=puntaje))+
  geom_point(color = "#013ADF" ,
             fill = "#013ADF" , size = 4, shape = 18, alpha = 0.5)+xlab("orden")+
  geom_abline(slope = 0)


# Se observa cierto patrron..... estonces es dependiente?

acf(model_1$residuals, main="hola")



#################################
## Supuesto 3 : Homocedasticidad
#################################


bptest(model_1)        # p-value = 0.02877    Se rechaza homocedasticidad.
gqtest(model_1, order.by = dt$puntaje, data=dt)      # p-value = 0.6513   No se rechaza homocedasticidad


ajustados=model_1$fitted.values
databp=data.frame(ajustados,residuos)
ggplot(databp,aes(x=ajustados,y=residuos))+
  geom_point(color = "#013ADF", fill = "#013ADF", size = 4,
             shape = 18, alpha = 0.5)+xlab("ajustados")+
  geom_abline(slope = 0,linetype="dashed")


# Si bien el test no rechaza la homocedasticidad de los errores el p valor es
# cercano al nivel de significación y se aprecia una leve estructura en la
# gráfica.






##########################################
## Modelo de mínimos cuadrados ponderados
##########################################


fited=lm(abs(model_1$residuals) ~ model_1$fitted.values)$residuals
pesos=I(1/model_1$fitted.values ^ 2) # definimos el modelo con los
pesos
wls_inspec<- lm(puntaje ~ horas_estudio,weights = pesos,data=dt)
summary(wls_inspec)

# Este modelo también es adecuado, el valor del R2 adj es mas grande (0.6455) en el modelo 2 versous
# 0.5054  del modelo 1, pero el valor residual en el primero es 12.24, mientras que en el segundo es 0.1663, lo cual dice...(preguntar a Debora)






# Ejemplo que vimos en la practica
ww <-1 / lm(abs(model_1$residuals) ~ model_1$fitted.values)$fitted.values^2
www <-1 / (abs(model_1$residuals))#^2

par(mfrow = c(1, 1)) 
plot(dt$horas_estudio,dt$puntaje,xlab="horas_estudio",ylab="puntaje",
     main="horas_estudio vs puntaje", ylim=c(0,200000))

abline(model_1,col="darkviolet",lwd=2)

linMod_ww <- lm(puntaje ~ horas_estudio, data = dt,weights =ww)
linMod_www<- lm(puntaje ~ horas_estudio, data = dt,weights =www)
abline(linMod_ww,col="hotpink",lwd=2)
abline(linMod_www,col="gold",lwd=2)








##########################################
## Diagnostico modelo con pesos
##########################################


#### Normalidad ####

residuos=residuals(wls_inspec)

shapiro.test(residuos)         # p-value = 0.0008394     No es normal
ad.test(residuos)              # p-value = 0.004388      No es normal
lillie.test(residuos)          # p-value = 0.01911       No es normal



qqPlot(residuos, pch=19,
       main="QQplot para los residuos del Modelo Lineal",
       xlab="Cuantiles teóricos",
       ylab="Cuantiles muestrales")





#### Independencia ####

dwtest(wls_inspec,alternative
       ="two.sided",iterations = 1000)            # p-value = 0.477  Hay independencia



ggplot(dt,aes(x=horas_estudio,y=puntaje))+
  geom_point(color = "#013ADF" ,
             fill = "#013ADF" , size = 4, shape = 18, alpha = 0.5)+xlab("orden")+
  geom_abline(slope = 0)


# Se observa cierto patrron..... estonces es dependiente?




#### Homocedasticidad ####

bptest(wls_inspec)        # p-value =  0.9709    No se rechaza homocedasticidad.
gqtest(wls_inspec, order.by = dt$puntaje, data=dt)      # p-value = 0.6513   No se rechaza homocedasticidad


ajustados=wls_inspec$fitted.values
databp=data.frame(ajustados,residuos)
ggplot(databp,aes(x=ajustados,y=residuos))+
  geom_point(color = "#013ADF", fill = "#013ADF", size = 4,
             shape = 18, alpha = 0.5)+xlab("ajustados")+
  geom_abline(slope = 0,linetype="dashed")


# Si bien el test no rechaza la homocedasticidad de los errores el p valor es
# cercano al nivel de significación y se aprecia una leve estructura en la
# gráfica.
