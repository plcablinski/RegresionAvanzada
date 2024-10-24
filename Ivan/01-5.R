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



####################
## Load
####################
archivo = "inmobiliaria.csv"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-fread(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
#attach(dataset)
dt <- dataset




####################
## Precio
####################
shapiro_dt <- data.table(
  edad = c(shapiro.test(dt$edad)$p.value),                 # No normal.
  distancia = c(shapiro.test(dt$distancia)$p.value),       # No normal.
  negocios = c(shapiro.test(dt$negocios)$p.value),         # No normal.
  latitud = c(shapiro.test(dt$latitud)$p.value),           # No normal.
  longitud = c(shapiro.test(dt$longitud)$p.value),         # No normal.
  precio = c(shapiro.test(dt$precio)$p.value)              # No normal.
)

head(shapiro_dt)




# Como ninguna variable es normal, debo aplicar Spearman en vez de Pearson.
spearman_dt <- data.table(
  precio_edad = c(cor.test(dt$precio, dt$edad, method = "spearman")$p.value),           # -0.283698 
  precio_distancia = c(cor.test(dt$precio, dt$distancia, method = "spearman")$p.value), # -0.7753983 
  precio_negocios = c(cor.test(dt$precio, dt$negocios, method = "spearman")$p.value),   #  0.6196517 
  precio_latitud = c(cor.test(dt$precio, dt$latitud, method = "spearman")$p.value),     #  0.5867155 
  precio_longitud = c(cor.test(dt$precio, dt$longitud, method = "spearman")$p.value)    #  0.4288781 
)

head(spearman_dt)

# If the p-value is < 5%, then the correlation between x and y is significant.

cor.test(dt$longitud, dt$precio, method = "spearman")  








####################
## Precio vs Dist.
####################

# Análisis de normalidad multivariada - Test de Henze Zirkler
respuesta_testHZ<-mvn(dt[,c("precio","distancia")] , mvnTest = "hz")
respuesta_testHZ      # No se cumple normalidad multivariada

# Correlacion
cor.test(dt$precio, dt$distancia, method = "spearman") # Alta correlacion negativa.

# Modelo lineal
model_1 <- lm(precio ~ distancia, data = dt)
summary(model_1) # Hay linealidad.





####################
## coeficientes ML
####################

# Test de Wald
waldo <- wald.test(Sigma = vcov(model_1), b = coef(model_1), Terms = 2)      # pvalor chico, es significativo.
waldo        # Chi2=335.5 y pvalue=0 ----> coeficiente sumamente significativo.




#################################
## Gráfico de recta de regresión
#################################

ggplot(data = dt, mapping = aes(x = distancia, y = precio)) + 
  geom_point(color = "firebrick", size = 2) + 
  labs(title = "Precio ~ Distancias", x = "Distancias") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))



##########################
## análisis diagnóstico de
## los residuos del mismo
##########################

 

dt$prediccion <- model_1$fitted.values 
dt$residuos   <- model_1$residuals



ggplot(data = dt, aes(x = residuos)) + geom_histogram(aes(y = ..density..)) + 
  labs(title = "Histograma de los residuos") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))




qqnorm(model_1$residuals) 
qqline(model_1$residuals)


par(mfrow=c(2,2))
plot(model_1)










##########################
## Tests 
##########################


# Normalidad
shapiro.test(model_1$residuals)     # pvalue chico, rechaza normalidad

# Durbin-Watson
dwt(model_1)                        # pvalue chico, hay autocorrelación entre variables, no hay independencia

# Breusch-Pagan
bptest(model_1)                     # pvalue grande, Concluimos que no se rechaza homocedasticidad.






##########################
## Outliers - Influyentes 
##########################


par(mfrow=c(1,1))
plot(model_1$fitted.values,model_1$residuals)




outlierTest(model_1)      # metodo analitico para identificar outliers
# Me dice que el punto 266 es un outlier.
influenceIndexPlot(model_1, vars="Bonf", las=1,col="blue")    # metodo grafico para identificar outliers.
# Me dice que los outliers son el punto 109 y 266.





# LEVERAGE : Influyentes
cota=3 * mean(hatvalues(model_1))
leverage <- hatvalues(model_1) > cota
cbind(hatvalues(model_1),cota,leverage)


# LEVERAGE :Influyentes
cota=3 * mean(hatvalues(model_1))
leverage <- hatvalues(model_1) > cota
sum(leverage)
## hay 34 valores influyentes




#LEVERAGE : un criterio (mayores que 0.2) 
lev<-hatvalues(model_1)
which(lev>0.2)




# LEVERAGE : un criterio mas exigente
n<-length(dt$precio)
p<-length(model_1$coefficients)
which(lev>2*p/n)
# Ufff aca salieron un monton.






# Influyentes: distancia de Cook -  Metodo analitico
dcook<-cooks.distance(model_1)
which(dcook>4/n)

#punto de corte
corted<-qf(0.5,2,n-2)
which(dcook>corted)
# no devuelve niguno!

# Influyentes: distancia de Cook -  Metodo grafico
influencePlot(model = model_1)
influenceIndexPlot(model_1, vars= "Cook" , las=1, col="blue")
# Me dice que los outliers son el 245 y 266
hist(dcook)





# DFFITS: metodo analitico
n<-length(dt$precio)
p<-length(model_1$coefficients)
which(dffits(model_1)>2 * sqrt(p / n))
# Los valores influyentes son: 9  12  31 101 112 144 162 216 224 245 251 266 308 340 343 375 385

#  DFFITS : metodo grafico
df <- model_1$df.residual
p <- length(model_1$coeffcients)
n <- nrow(model_1$model_1)
dffts_crit = 2 * sqrt(p / n)
dffts <- dffits(model_1)
df <- data.frame(obs = names(dffts), dffts = dffts)
ggplot(df, aes(y = dffts, x = obs)) + geom_point(color="#013ADF" ) +
                                                 geom_hline(yintercept = c(dffts_crit, -dffts_crit), linetype="dashed" ) + labs(title = "DFFITS",subtitle = "Observaciones Influyentes" ,
                                                                             x = "Orden de Observación" ,y = "DFFITS" )+theme_bw()





# DFBETA
dfbetas(model_1)[,2]> 1
which(dfbetas(model_1)[,2]>1)
# Ningun valor influyente




# plot(model_1)     # me permite ver los puntos influyentes!
# res_stu_1<-rstudent(model_1)
# res_stu_1[abs(res_stu_1)>3]      # puntos 109, 206, 266, 308


 



# otros puntos influyentes
# influence.measures(model = model_1)
# summary(influence.measures(model = model_1))
# 
# dfbetas(model_1)[,2]> 1
# which(dfbetas(model_1)[,2]>1)







