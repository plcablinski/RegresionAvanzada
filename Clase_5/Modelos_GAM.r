rm(list=ls())
library(gamlss)
library(tidyverse)
library(ggpubr)
library(skimr)
data('rent')
datos <- rent %>% select(R, Fl, A, H, loc)
datos <- datos %>%setNames(c('valor', 'metros', 'anio', 'calef', 'local'))
head(datos)
str(datos)

mod_OLS <- gamlss( formula = valor ~ metros + anio + calef + local, family = NO, data = datos, trace = FALSE)
summary(mod_OLS)
plot(mod_OLS)

mod_GAM_sin_pb <- gamlss( formula = valor ~ metros + anio + calef +local, family = GA, data = datos, trace = FALSE)
summary(mod_GAM_sin_pb)
plot(mod_GAM_sin_pb)

mod_GAM_con_pb <- gamlss( formula = valor ~ pb(metros) + pb(anio) + calef +local, family = GA, data = datos, trace = FALSE)
summary(mod_GAM_con_pb)
plot(mod_GAM_con_pb)
c
wp(mod_OLS)
wp(mod_GAM_sin_pb)
wp(mod_GAM_con_pb)
par(mfrow = c(1,1))

drop1(mod_GAM_con_pb, parallel = 'multicore', ncpus = 6)
term.plot(mod_GAM_con_pb, , pages = 1, ask = FALSE, rug = TRUE)

mod_GAMLSS <- gamlss(formula = valor ~ pb(metros)+ pb(anio)+calef+local, sigma.formula = ~ pb(metros)+pb(anio)+calef+local, 
family = GA, data = datos, trace = TRUE)
summary(mod_GAMLSS)
par(mfrow = c(2,2))
term.plot(mod_GAMLSS, parameter = 'sigma',ask = FALSE, rug = TRUE)
par(mfrow = c(1,1))
drop1(mod_GAMLSS, parameter = 'sigma', parallel = 'multicore', ncpus = 6)

mod_GAMLSS_sin_calef <- gamlss(formula = valor ~ pb(metros)+ pb(anio)+calef+local, sigma.formula = ~ pb(metros)+pb(anio)+local, 
family = GA, data = datos, trace = TRUE)
summary(mod_GAMLSS_sin_calef)   
par(mfrow = c(2,2))
term.plot(mod_GAMLSS_sin_calef, parameter = 'sigma',ask = FALSE, rug = TRUE)
par(mfrow = c(1,1))
drop1(mod_GAMLSS_sin_calef, parameter = 'sigma', parallel = 'multicore', ncpus = 6)

par(mfrow = c(1,3))
wp(mod_GAM_con_pb, ylim.all = 0.5)
wp(mod_GAMLSS, ylim.all = 0.5)
wp(mod_GAMLSS_sin_calef, ylim.all = 0.5)
par(mfrow = c(1,1))
wp(mod_GAMLSS_sin_calef, ylim.all = 0.5)

# Comparación de modelos ajustados
GAIC(mod_OLS, mod_GAMLSS_sin_calef, mod_GAM_con_pb)
