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
library(tidyr)




####################
## Load
####################
archivo = "suplementos.xlsx"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-read_excel(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3
head(dataset,20)





####################
## Dataframe
####################
# attach(dataset)
dt <- as.data.table(dataset)

suplementos_data <- dt %>%
  pivot_longer(cols = everything(), names_to = "suplemento", values_to = "eficiencia")

head(suplementos_data)






####################
## a) Analisis grafico y descriptivo
####################


str(suplementos_data)
summary(suplementos_data)



# Boxplot para visualizar la eficiencia de conversión por suplemento
ggplot(suplementos_data, aes(x = suplemento, y = eficiencia, fill = suplemento)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.2) +
  labs(title = "Eficiencia de Conversión por Suplemento", x = "Suplemento", y = "Eficiencia de Conversión (kg MS/kg Ganancia de Peso)") +
  theme_minimal()




####################
## b) Hipotesis
####################

# Las hipótesis nulas y alternativas para este problema son:
#   
# - Hipótesis nula (H0): No hay diferencias significativas en la eficiencia de conversión entre los diferentes suplementos.
# - Hipótesis alternativa (H1): Hay al menos una diferencia significativa en la eficiencia de conversión entre los suplementos.


# Los supuestos necesarios para el ANOVA son:
#   
# - Independencia de las observaciones.
# - Normalidad de los residuos.
# - Homocedasticidad (igualdad de varianzas) entre los grupos.




####################
## c) Testear la Hipotesis
####################


# Ajustar el modelo ANOVA
anova_model <- aov(eficiencia ~ suplemento, data = suplementos_data)
summary(anova_model)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# suplemento   3  18.93    6.31   21.76 3.26e-08 ***
# Residuals   36  10.44    0.29 

# Conclusion: hay al menos un suplemento con distinta eficeincia.




####################
## d) Análisis de los supuestos
####################

# Normalidad de los residuos
shapiro.test(residuals(anova_model))        # p-value = 0.6414 es Normal.

# Homocedasticidad
leveneTest(eficiencia ~ suplemento, data = suplementos_data)
#       Df F value Pr(>F)
# group  3  0.8581 0.4716        # Es homocedastico.



# Diagnóstico gráfico
# par(mfrow = c(2, 2))
plot(anova_model)





####################
## e) Conclusiones
####################

# Como rechazamos H0, la idea es hacer varias comparaciones 
# con el test de Tukey para identificar cuáles suplementos son diferentes.



# Comparaciones múltiples de Tukey
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# Gráfico de las diferencias significativas
plot(tukey_result)


# <Pregunta>Como leemos la salida de Tukey?</Pregunta>
