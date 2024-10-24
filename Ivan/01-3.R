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


####################
## Load
####################
archivo = "peso_edad_colest.xlsx"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-read_excel(paste(ruta,archivo,sep = ""))
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
ggplot(data = dataset, aes(x = edad, y = colest)) +
  geom_point(colour = "red4") +
  ggtitle("Edad vs Colesterol") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(data = dataset, aes(x = peso, y = colest)) +
  geom_point(colour = "red4") +
  ggtitle("Peso vs Colesterol") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))



par(mfrow=c(2,2),oma=c(0,2,0,0)) # personaliza el espacio de gráfico
plot(dt$edad,dt$colest,pch=16,col=1,xlab="Edad",ylab="Colesterol")
title("Edad vs Colesterol")
plot(as.numeric(dt$peso),as.numeric(dt$colest),pch=16,col=1,xlab="Peso",ylab="Colesterol")
title("Peso vs Colesterol")


#########################
## Modelo Lineal
#########################



model_1 <- lm(colest ~ edad, data = dt)
summary(model_1)          # Coeficientes significativos, R grande 
# Hay un 76% de la variabilidad de Y que es explicada por la variabilidad de X a través de la recta de regresión.


model_2 <- lm(colest ~ peso, data = dt)
summary(model_2)        # la pendiente no es significativa y R recnotra chico. No aplicar ML.

#########################
## Coeficientes ML - Wald
#########################

# En el contexto del test de Wald aplicado a un modelo lineal en R, 
# el argumento Terms en la función wald.test especifica cuál de los 
# coeficientes del modelo queremos probar. Aquí, Terms = 2 se refiere 
# al segundo coeficiente en el modelo.


# Cuando ajustamos un modelo lineal en R usando lm, 
# los coeficientes se enumeran en el orden en que aparecen en el modelo. 
# En el ejemplo lm(colesterol ~ edad, data = datos), los coeficientes del modelo son:
#   1.El intercepto (constante)
#   2.El coeficiente para la variable edad


# En la función wald.test, Terms = 2 significa que estamos probando la hipótesis nula 
# de que el segundo coeficiente (edad) es igual a cero. Si quisiéramos probar el coeficiente
# del intercepto, usaríamos Terms = 1.

wald.test(Sigma = vcov(model_1), b = coef(model_1), Terms = 2) # 0.0
wald.test(Sigma = vcov(model_1), b = coef(model_1), Terms = 1) # 0.00028



#########################
## Intervalos de Conf.
#########################
confint(model_1, level = 0.95) #por default el nivel es 0.95
#             2.5 %     97.5 %
# (Intercept) 41.190390 149.813618
# edad        4.358216  6.983467

# proporciona los límites inferior y superior del intervalo de confianza 
# del 95% para cada uno de los coeficientes del modelo.


# El valor del estadistico del modelo es 79.9, al igual que el estadistico del test de Wald. Esto es así
# porque estimamos una sola variable.


# Como ambos métodos (modelo y test de Wald) indican que el coeficiente de edad es
# significativamente diferente de cero, podemos concluir que hay una asociación 
# entre la edad y el colesterol.




#########################
## Recta de regresión.
#########################

ggplot(data = dt, mapping = aes(x = edad, y = colest)) + 
  geom_point(color = "firebrick", size = 2) + 
  labs(title = "Colesterol ~ Edad", x = "Edad") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))



#########################
## Valores
#########################

# y = 5.6708*x + 95.5020

# Mostrar los coeficientes del modelo
coeficientes <- coef(model_1)
print(coeficientes)


# Estimar E(Y) para x = 25 y x = 48
x_25 <- 25
x_48 <- 48
y_25 <- coeficientes[1] + coeficientes[2] * x_25
y_48 <- coeficientes[1] + coeficientes[2] * x_48

print(paste("E(Y) para x = 25 años: ", y_25))        # 237.27
print(paste("E(Y) para x = 48 años: ", y_48))        # 367.70

# Estimar E(Y) para x = 80 (con precaución por extrapolación)
x_80 <- 80
y_80 <- coeficientes[1] + coeficientes[2] * x_80
print(paste("E(Y) para x = 80 años: ", y_80))        # 549.169







#########################
## Residuos
#########################
# normalidad de forma teorica
pvalue <- shapiro.test(model_1$residuals)$p.value ### 
pvalue    # 0.51 Es NORMAL
# if( pvalue > 0.05 ) { 
#   print("Cumple supuesto de normalidad.")
# } else { 
#   print("No cumple supuesto de normalidad.") 
# }

ad.test(model_1$residuals)$p.value         # 0.45
lillie.test(model_1$residuals)$p.value     # 0.17

# normalidad de forma grafica
# ggplot(data = dt, aes(x = prediccion, y = residuos)) + 
#   geom_point(aes(color = residuos)) + 
#   scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
#   geom_hline(yintercept = 0) + geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) + 
#   labs(title = "Distribución de los residuos", x = "predicción modelo", y = "residuo") + 
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

residuos=residuals(model_1)
qqPlot(residuos, pch=19,
       main="QQplot para los residuos del Modelo Lineal",
       xlab="Cuantiles teóricos",
       ylab="Cuantiles muestrales")
# En el qqPlot vemos que todos los puntos están dentro del intervalo de predicción.

par(mfrow=c(2,2),oma=c(0,0,2,0)) # personaliza el espacio de gráfico
qqnorm(model_1$residuals) 
qqline(model_1$residuals)



# Homocedasticidad de forma grafica
dt_2<-dt
dt_2$prediccion <- model_1$fitted.values 
dt_2$residuos <- model_1$residuals
ggplot(data = dt_2, aes(x = prediccion, y = residuos)) + 
  geom_point(aes(color = residuos)) + 
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") + 
  geom_hline(yintercept = 0) + geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) + 
  labs(title = "Distribución de los residuos", x = "predicción modelo", y = "residuo") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")



ajustados=model_1$fitted.values
databp=data.frame(ajustados,residuos)
ggplot(databp,aes(x=ajustados,y=residuos)) +
  geom_point(color = "red", fill = "blue", size = 4,shape = 18, alpha = 0.5) + 
  geom_abline(slope = 0, linetype="dashed" )

# Vemos en el gráfico que no hay estructura.

# Homocedasticidad de forma teorica
pvalue <- bptest(model_1)$p.value       # p-value = 0.6908  Hay Homocedasticidad.
# if( pvalue > 0.05 ) { 
#   print("Cumple supuesto de homocedasticidad")
# } else { print("No cumple supuesto de homocedasticidad") }
