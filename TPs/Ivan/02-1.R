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
library(ggplot2)
library(GGally)

####################
## Load
####################
data(trees)         ### Es una dataset que viene por defecto.


####################
## a) Visualizar asociacion
####################

# Visualizar la asociación entre las variables de a pares
pairs(trees)
# O, usando GGally
ggpairs(trees)






####################
## b) Modelo Lineal simple
####################


# Verificar los supuestos de normalidad
shapiro.test(trees$Height)            # p-valor=0.4034 - es normal.
shapiro.test(trees$Girth)             # p-valor=0.0889 - es normal.
shapiro.test(trees$Volume)            # p-valor=0.0035 - NO es normal.


#Usamos Test Henze-Zirkler para evaluar normalidad multivariada (bivariada en este caso)
respuesta_testHZ<-mvn(trees[,c("Height", "Volume")] , mvnTest = "hz")
respuesta_testHZ              
respuesta_testHZ$multivariateNormality      # p-valor=0.026 por lo tanto es NO normal bivariada.

# Test Henze-Zirkler para evaluar normalidad multivariada (bivariada en este caso)
respuesta_testHZ<-mvn(trees[,c("Girth", "Volume")] , mvnTest = "hz")
respuesta_testHZ              
respuesta_testHZ$multivariateNormality      # p-valor=0.007 por lo tanto es NO normal bivariada.



# Como no son normales bivariadas, usamos test de cor de spearman
cor(trees$Height,trees$Volume,method="spearman")       # 0.578
cor(trees$Girth,trees$Volume,method="spearman")        # 0.954


# Creo modelo 1
model_1 <- lm(Volume ~ Girth, data = trees)
summary(model_1)          # Coeficientes significativos, R grande.



# Creo modelo 1
model_2 <- lm(Volume ~ Height, data = trees)
summary(model_2)          # Coeficientes significativos, R chico.





####################
## c) Analisis diagnostico - Normalidad
####################

residuos_1=residuals(model_1)

shapiro.test(residuos_1)         # p-value = 0.078       Es normal
ad.test(residuos_1)              # p-value = 0.653       Es normal
lillie.test(residuos_1)          # p-value = 0.615       Es normal

qqPlot(residuos_1, pch=19,
       main="QQplot para los residuos del Modelo Lineal",
       xlab="Cuantiles teóricos",
       ylab="Cuantiles muestrales")

# Viendo el grafico vemos que hay dos puntos que atentan contra la normalidad.




residuos_2=residuals(model_2)

shapiro.test(residuos_2)         # p-value = 0.164       Es normal
ad.test(residuos_2)              # p-value = 0.081       Es normal
lillie.test(residuos_2)          # p-value = 0.143       Es normal

qqPlot(residuos_2, pch=19,
       main="QQplot para los residuos del Modelo Lineal",
       xlab="Cuantiles teóricos",
       ylab="Cuantiles muestrales")


# Vemos que hay cierta normalidad aunque hay dos puntos que atentan contra la normalidad



###########################
## c) Analisis diagnostico - Independencia
###########################

dwtest(model_1,alternative
       ="two.sided",iterations = 1000)            # p-value = 0.069  Hay independencia

ggplot(trees,aes(x=Girth,y=Volume))+
  geom_point(color = "#013ADF" ,
             fill = "#013ADF" , size = 4, shape = 18, alpha = 0.5)+xlab("orden")+
  geom_abline(slope = 0)





dwtest(model_2,alternative
       ="two.sided",iterations = 1000)            # p-value = chico  NO hay independencia

ggplot(trees,aes(x=Height,y=Volume))+
  geom_point(color = "#013ADF" ,
             fill = "#013ADF" , size = 4, shape = 18, alpha = 0.5)+xlab("orden")+
  geom_abline(slope = 0)


# Se observa cierto patron....


###########################
## c) Analisis diagnostico - Homocedasticidad
###########################



bptest(model_1)                                        # p-value = 0.017    Se rechaza homocedasticidad.
gqtest(model_1, order.by = trees$Volume, data=dt)      # p-value = 0.000    Se rechaza homocedasticidad.


ajustados=model_1$fitted.values
databp=data.frame(ajustados,residuos_1)
ggplot(databp,aes(x=ajustados,y=residuos_1))+
  geom_point(color = "#013ADF", fill = "#013ADF", size = 4,
             shape = 18, alpha = 0.5)+xlab("ajustados")+
  geom_abline(slope = 0,linetype="dashed")



# El test rechaza la homocedasticidad de los errores, el p valor es
# más chico que el nivel de significación y se aprecia una leve estructura en la
# gráfica.




bptest(model_2)                                        # p-value = 0.000    Se rechaza homocedasticidad.
gqtest(model_2, order.by = trees$Volume, data=dt)      # p-value = 0.000    Se rechaza homocedasticidad


ajustados=model_2$fitted.values
databp=data.frame(ajustados,residuos)
ggplot(databp,aes(x=ajustados,y=residuos_2))+
  geom_point(color = "#013ADF", fill = "#013ADF", size = 4,
             shape = 18, alpha = 0.5)+xlab("ajustados")+
  geom_abline(slope = 0,linetype="dashed")


# Análisis idem anterior...



###########################
## c) Outliers | Influyentes | model_1
###########################

plot(model_1$fitted.values,model_1$residuals)

outlierTest(model_1)      # Obs 31 es outlier
influenceIndexPlot(model_1, vars="Bonf", las=1,col="blue")     # Obs 1 y 31 son outliers.


# LEVERAGE : Influyentes
cota=3 * mean(hatvalues(model_1))
leverage <- hatvalues(model_1) > cota
sum(leverage)     # hay 1 valor influyente


#LEVERAGE : un criterio (mayores que 0.2) 
lev<-hatvalues(model_1)
which(lev>0.2)   # Obs 31 es influyente


# LEVERAGE : un criterio mas exigente
n<-length(trees$Volume)
p<-length(model_1$coefficients)
which(lev>2*p/n)   # Obs 31 es influyente


# Influyentes: distancia de Cook -  Metodo analitico
dcook<-cooks.distance(model_1)
which(dcook>4/n)   # Obs 31 es influyente

#punto de corte
corted<-qf(0.5,2,n-2)
which(dcook>corted)       # Obs 31 es influyente


# Influyentes: distancia de Cook -  Metodo grafico
influencePlot(model = model_1)                                        # Obs 31 es influyente
influenceIndexPlot(model_1, vars= "Cook" , las=1, col="blue")         # Obs 1 y 31 son influyentes
hist(dcook)





# DFFITS: metodo analitico
n<-length(trees$Volume)
p<-length(model_1$coefficients)
which(dffits(model_1)>2 * sqrt(p / n))       # Obs 31 es influyente


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
which(dfbetas(model_1)[,2]>1) # Obs 31 es influyente



###########################
## c) Outliers | Influyentes | model_2
###########################


plot(model_2$fitted.values,model_2$residuals)

outlierTest(model_2)      # Obs 31 es outlier
influenceIndexPlot(model_2, vars="Bonf", las=1,col="blue")     # Obs 1 y 31 son outliers.


# LEVERAGE : Influyentes
cota= 3 * mean(hatvalues(model_2))
leverage <- hatvalues(model_2) > cota
sum(leverage)     # No hay influyente


#LEVERAGE : un criterio (mayores que 0.2) 
lev<-hatvalues(model_2)
which(lev>0.2)   # No hay influyente


# LEVERAGE : un criterio mas exigente
n<-length(trees$Volume)
p<-length(model_2$coefficients)
which(lev>2*p/n)   # Obs 2,3,20,31 es influyente


# Influyentes: distancia de Cook -  Metodo analitico
dcook<-cooks.distance(model_2)
which(dcook>4/n)   # Obs 18 y 31 es influyente

#punto de corte
corted<-qf(0.5,2,n-2)
which(dcook>corted)       # No hay influyente


# Influyentes: distancia de Cook -  Metodo grafico
influencePlot(model = model_2)                                        # Obs 3,18,20,28,31 es influyente
influenceIndexPlot(model_2, vars= "Cook" , las=1, col="blue")         # Obs 18 y 31 son influyentes
hist(dcook)





# DFFITS: metodo analitico
n<-length(trees$Volume)
p<-length(model_2$coefficients)
which(dffits(model_2)>2 * sqrt(p / n))       # Obs 31 es influyente


#  DFFITS : metodo grafico
df <- model_2$df.residual
p <- length(model_2$coeffcients)
n <- nrow(model_2$model_2)
dffts_crit = 2 * sqrt(p / n)
dffts <- dffits(model_2)
df <- data.frame(obs = names(dffts), dffts = dffts)
ggplot(df, aes(y = dffts, x = obs)) + geom_point(color="#013ADF" ) +
  geom_hline(yintercept = c(dffts_crit, -dffts_crit), linetype="dashed" ) + labs(title = "DFFITS",subtitle = "Observaciones Influyentes" ,
                                                                                 x = "Orden de Observación" ,y = "DFFITS" )+theme_bw()



# DFBETA
dfbetas(model_2)[,2]> 1
which(dfbetas(model_2)[,2]>1)       # No hay influyente



#########################
## d) Intervalos de Conf.
#########################
confint(model_1, level = 0.95) #por default el nivel es 0.95
#               2.5 %     97.5 %
# (Intercept) -43.825953 -30.060965
# Girth         4.559914   5.571799



confint(model_2, level = 0.95) #por default el nivel es 0.95
#                2.5 %     97.5 %
# (Intercept) -146.993871 -27.253357
# Height         0.758249   2.328451




#########################
## e) Sin influyentes
#########################

# Nuevo modelo sin observaciones influyentes para Circunferencia
trees_no_influential_circumference <- trees[-31, ]
model_circumference_no_influential <- lm(Volume ~ Girth, data = trees_no_influential_circumference)
summary(model_circumference_no_influential)



# Nuevo modelo sin observaciones influyentes para Altura
trees_no_influential_height <- trees[-c(18,31), ]
model_height_no_influential <- lm(Volume ~ Height, data = trees_no_influential_height)
summary(model_height_no_influential)




#########################
## f) Diametro 16.1
#########################
# Nuevo dato
new_data <- data.frame(Girth = 16.1)

# Intervalo de confianza del 95%
conf_interval <- predict(model_circumference_no_influential, new_data, interval = "confidence", level = 0.95)

# Intervalo de predicción del 95%
pred_interval <- predict(model_circumference_no_influential, new_data, interval = "prediction", level = 0.95)

conf_interval
# fit      lwr      upr
# 43.35598 41.24025 45.47171
pred_interval
# fit      lwr      upr
# 43.35598 35.26296 51.44899






#########################
## g) Diametro 16.1
#########################

# Ajustar el modelo con ambas variables predictoras
model_both <- lm(Volume ~ Girth + Height, data = trees)
summary(model_both)



# Comparar R² ajustados
adj_r_squared_model_1 <- summary(model_1)$adj.r.squared
adj_r_squared_model_2 <- summary(model_2)$adj.r.squared
adj_r_squared_model_both <- summary(model_both)$adj.r.squared

cat("Adjusted R² for model with circumference:", adj_r_squared_model_1, "\n")         # 0.93
cat("Adjusted R² for model with height:", adj_r_squared_model_2, "\n")                # 0.33
cat("Adjusted R² for model with both predictors:", adj_r_squared_model_both, "\n")    # 0.94




# Comparar modelos con el test ANOVA
anova(model_1, model_both)

# Interpretación del test ANOVA
anova_result <- anova(model_1, model_both)
anova_result

if (anova_result$`Pr(>F)`[2] < 0.05) {
  cat("El modelo con ambas variables predictoras (Girth y Height) es significativamente mejor que el modelo con solo la circunferencia.\n")
} else {
  cat("No hay evidencia suficiente para afirmar que el modelo con ambas variables predictoras es significativamente mejor que el modelo con solo la circunferencia.\n")
} 


# Conclusion: Si el valor p del test ANOVA es menor que 0.05, concluyes que el modelo con ambas 
# variables predictoras es significativamente mejor. De lo contrario, no hay suficiente evidencia
# para decir que el modelo con ambas variables es mejor que el modelo con solo la circunferencia.
