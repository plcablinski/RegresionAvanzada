rm(list=ls())
library(ISLR)
library(leaps); library(sp); library(glmnet); library(pls)
library(ggpubr)
library(car)
data(Carseats)
head(Carseats)
# consideramos todos los modelos hasta 10 predictoras
str(Carseats)
regfit.todos <- regsubsets(Sales ~ ., Carseats, nvmax = 10)
reg.summary <- summary(regfit.todos)
reg.summary
# definimos puntos óptimos para c/ criterio, guardamos su posición
max_adjr2<-which.max(reg.summary$adjr2)
min_cp<-which.min(reg.summary$cp)
min_bic<-which.min(reg.summary$bic)
min_aic <- which.min(reg.summary$aic)
sal_reg<-data.frame(orden=1:10,adjr2=reg.summary$rsq,Cp=reg.summary$cp,Bic=reg.summary$bic)

# graficamos los valores de c/ criterio en función de la cant. de predictores
pl1<-ggplot(sal_reg,aes(orden,adjr2))+geom_point(size=2,col='darkblue') +theme_bw()+xlab('cantidad de predictores')+ylab('R-2ajust')+
geom_point(aes(max_adjr2,reg.summary$adjr2[max_adjr2]),size=3,color='coral')

pl2<-ggplot(sal_reg,aes(orden,Cp))+geom_point(size=2,col='darkblue')+theme_bw()+xlab('cantidad de predictores')+ylab('Cp')+
geom_point(aes(min_cp,reg.summary$cp[min_cp]),size=3,color='coral')

pl3<-ggplot(sal_reg,aes(orden,Bic))+geom_point(size=2,col='darkblue')+theme_bw()+xlab('cantidad de predictores')+ylab('BIC')+
geom_point(aes(min_bic,reg.summary$bic[min_bic]),size=3,color='coral')

ggarrange(pl1,pl2,pl3,ncol=1)

# buscamos los coeficientes del modelo con 7 predictores
coef(regfit.todos, 7)

# fijamos una semilla y elegimos un 80% de los datos para entrenar
# el modelo y el 20% para validarlo
set.seed(1123)
n = nrow(Carseats)
index = sample(n, n*0.80, replace = FALSE)
train_set = Carseats[index,]
dim(train_set)
test_set = Carseats[-index,]
dim(test_set)

# modelo completo con todas las variables
model_full = lm(Sales ~ ., data = train_set)
S(model_full)
# modelo sólo con intercepto
model_int = lm(Sales ~ -., data = train_set)
S(model_int)

scopeformula <- formula(model_full)
scopeformula

fwd_sel <- step(object=model_int, scope=scopeformula, direction='forward')
summary(fwd_sel)
FwdSelection_AIC <- AIC(fwd_sel)
FwdSelection_AIC

back_sel <- step(object=model_full, scope=scopeformula, direction='backward')
summary(back_sel)
BackSelection_AIC = AIC(back_sel)
BackSelection_AIC

both_sel <- step(object=model_full, scope=scopeformula, direction='both')
summary(both_sel)
BidirSelection_AIC = AIC(both_sel)
BidirSelection_AIC

AIC_df <- data.frame(FwdSelection=FwdSelection_AIC, BackSelection=BackSelection_AIC, BidirSelection=BidirSelection_AIC)
rownames(AIC_df) = c('AIC')
AIC_df

