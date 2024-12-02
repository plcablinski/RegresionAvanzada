rm(list=ls())
setwd('C:/RegresionAvanzada/Clase_3')
library(readxl)
library(ggplot2)
library(ggpubr)
chicos <- read_excel('antropom.xlsx')
str(chicos)
# Convertir la variable Sexo a factor
chicos$Sexo <- as.factor(chicos$Sexo)

head(antropom)
dim(antropom)
ggpairs( chicos, 
    columns = c( 2:6 ), 
    mapping = aes( colo = Sexo ), 
    upper = list( continuous = 'density', combo = 'box_no_facet' ), 
    lower = list( continuous = 'points', combo = 'dot_no_facet' ) ) +
    theme_bw()

mod_chicos <- lm(Rodilla_a_nalga ~ Peso + Estatura, data = chicos )
summary(mod_chicos)
residuos <- residuals(mod_chicos)
par(mfrow = c(1, 2))
# plot donde x es Peso e y son los residuos
plot(chicos$Peso, residuos, xlab = 'Peso', ylab = 'Residuos', main = 'Residuos vs Peso')
# plot donde x es Estatura e y son los residuos
plot(chicos$Estatura, residuos, xlab = 'Estatura', ylab = 'Residuos', main = 'Residuos vs Estatura')
par(mfrow = c(1, 1))

mod_chicos1 <- lm(Rodilla_a_nalga ~ sqrt(Peso) + Estatura, data = chicos )
summary(mod_chicos1)
residuos1 <- residuals(mod_chicos1)

p4<-ggplot(chicos,aes(x=sqrt(Peso),y=residuos1))+
geom_point(pch=18,color='darkblue')+theme_bw()
p5<-ggplot(chicos,aes(x=Estatura,y=residuos1))+
geom_point(pch=18,color='darkblue')+theme_bw()
p6 = ggarrange(p4,p5)
p6

mod_chicos2 <- lm(Rodilla_a_nalga ~ log(Peso) + Estatura, data = chicos )
summary(mod_chicos2)
residuos2 <- residuals(mod_chicos2)

p7<-ggplot(chicos,aes(x=log(Peso),y=residuos2))+
geom_point(pch=18,color='darkblue')+theme_bw()
p8<-ggplot(chicos,aes(x=Estatura,y=residuos2))+
geom_point(pch=18,color='darkblue')+theme_bw()
p9 = ggarrange(p7,p8)
p9

boxCox(mod_chicos2, lambda = seq(-2, 2, by = 0.1))
powerTransform(mod_chicos1)

mod_chicos3 <- lm(I(Rodilla_a_nalga**1.33) ~ sqrt(Peso) + Estatura, data = chicos )
residuos3 <- residuals(mod_chicos3)
summary(mod_chicos3)

p10<-ggplot(chicos,aes(x=sqrt(Peso),y=residuos3))+
geom_point(pch=18,color='darkblue')+theme_bw()
p11<-ggplot(chicos,aes(x=Estatura,y=residuos3))+
geom_point(pch=18,color='darkblue')+theme_bw()
p12 = ggarrange(p10,p11)
p12

# Normalidad residuos
shapiro.test(residuos3)
#Hetereocedasticidad
bptest(mod_chicos3)
#Durbin Watson
durbinWatsonTest(mod_chicos3)

# Dividir a chicos en dos grupos uno de entrenamiento y otro de prueba 80% y 20%
set.seed(123)
n <- (nrow(chicos))
ind <- sample(1:n, 0.8*n)
chicos_train <- chicos[ind, ]
chicos_test <- chicos[-ind, ]
nrow(chicos)
nrow(chicos_train)
nrow(chicos_test)

# Econtrar el mejor subconjunto de predictores para Rodilla_a_nalga
library(leaps)
# Ajustar el modelo con regsubsets
head(chicos)
# mostrar chicos sin la columna Individuo
head(chicos[,-1])
fit <- regsubsets(Rodilla_a_nalga ~ ., data = chicos[,-1], really.big = TRUE)
fit 
summary(fit)
