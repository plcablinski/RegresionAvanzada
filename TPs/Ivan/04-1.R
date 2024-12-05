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
archivo = "cervezas.xlsx"
ruta = "c:/Users/Usuario/Documents/Universidad/austral/2024/regresion_avanzada/datasets/RegresionAvanzada/"
dataset<-read_excel(paste(ruta,archivo,sep = ""))
dim(dataset)        #30 3
head(dataset,20)


####################
## Dataframe
####################
attach(dataset)
dt <- as.data.table(dataset)

cerveza_data <- dt %>%
  pivot_longer(cols = everything(), names_to = "marca", values_to = "sodio")

head(cerveza_data)




####################
## a) Boxplot
####################


# Crear el boxplot con ggplot2
ggplot(cerveza_data, aes(x = marca, y = sodio, fill=marca)) +
  geom_boxplot(outlier.colour = "black", outlier.shape = 16,
               outlier.size = 2, notch = FALSE) +
  geom_jitter(width = 0.2, aes(color = marca)) +
  theme_minimal() +
  labs(title = "Contenido en sodio por marca de cerveza",
       x = "Marca de cerveza",
       y = "Contenido en sodio (mg)") +
  theme(legend.position = "none")





####################
## b) Media y Desvio
####################


# Calcular media y desvío estándar por grupo
stats <- cerveza_data %>%
  group_by(marca) %>%
  summarise(
    media = mean(sodio),
    desviacion_estandar = sd(sodio)
  )
print(stats)





####################
## c) Hipótesis
####################

# Las hipótesis nulas y alternativas son:
#   
# Hipótesis nula (H0): Las medias de sodio de todas las marcas son iguales.
# Hipótesis alternativa (H1): Al menos una de las medias de sodio de las marcas es diferente.




####################
## d) Contrastar Hipótesis
####################

# Realizar ANOVA
anova_result <- aov(sodio ~ marca, data = cerveza_data)
summary(anova_result)
#             Df Sum Sq Mean Sq F value Pr(>F)    
# marca        5  854.5  170.91   238.7 <2e-16 ***
# Residuals   42   30.1    0.72

# El pvalor es chico, por lo que rechazamos H0, lo que quiere decir que hay almenos una media distinta.




####################
## e) Análisis diagnóstico
####################

# Verificar normalidad de los residuos
shapiro_test <- shapiro.test(residuals(anova_result))
print(shapiro_test)    # 0.1742 ES normal

# Verificar homocedasticidad
levene_test <- leveneTest(sodio ~ marca, data = cerveza_data)
print(levene_test)   # 0.3376 Es homocedastico.





####################
## f) Conclusion
####################
# Como el p-valor del ANOVA es menor que 0.05, rechazamos la hipótesis nula y concluimos que 
# hay diferencias significativas en el contenido en sodio entre las marcas de cerveza.