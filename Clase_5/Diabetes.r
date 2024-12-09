rm(list=ls())
library(glmnet)
library(readxl)
library(dplyr)

setwd('C:/RegresionAvanzada/Clase_5')
diabetes <- read_excel('diabetes.xls')
attach(diabetes)
head(diabetes)


# Crear una nueva columna para los intervalos de SSPG
diabetes <- diabetes %>%
  mutate(SSPG_interval = cut(SSPG, 
                             breaks = c(seq(0, 320, by = 40), max(SSPG, na.rm = TRUE)), 
                             right = FALSE))

# Verificar los intervalos creados
table(diabetes$SSPG_interval)

# Calcular la media de SSPG y la proporción de DIABET = 1 en cada intervalo
summary_by_interval <- diabetes %>%
  group_by(SSPG_interval) %>%
  summarise(
    mean_SSPG = mean(SSPG, na.rm = TRUE),
    proportion_DIABET = mean(DIABET == 1, na.rm = TRUE)
  )

# Mostrar el resumen por intervalos
print(summary_by_interval)

mod_1 <- glm(DIABET ~ SSPG, data=diabetes, family=binomial)	 
summary(mod_1)

confint(mod_1, level = 0.95)

# Calcular el Odds Ratio para un incremento de 10 en SSPG
exp(10 * beta)
# El odds ratio para un incremento de 10 en SSPG es 1.27 esto quiere decir que por cada incremento de 10 en SSPG, 
# la probabilidad de tener diabetes se incrementa en 27%
 
# Para hallar el IC unitario:
exp(confint(object = mod_1, level = 0.95 )[2,])

# Para 10 unidades:
exp(10 * confint(object = mod_1, level = 0.95 )[2,]) 
# Esto se interpreta como que el intervalo de confianza para un incremento de 10 en SSPG es entre 1.2 y 1.4
# por lo tanto la probabilidad de tener diabetes se incrementa entre 20% y 40% por cada incremento de 10 en SSPG

# Calcular el valor de x para el cual p(x) = 0.5
x_05 <- -mod_1$coefficients[1] / mod_1$coefficients[2]
x_05

# Definir los coeficientes del modelo
beta_0 <- coef(mod_1)[1]
beta_1 <- coef(mod_1)[2]

# Función para calcular el valor de SSPG para una probabilidad específica
calculate_SSPG <- function(p_z, beta_0, beta_1) {
  log_odds <- log(p_z / (1 - p_z))
  x <- (log_odds - beta_0) / beta_1
  return(x)
}

# Ejemplo: Calcular el valor de SSPG para una probabilidad de 0.7
p_z <- 0.268839
SSPG_value <- calculate_SSPG(p_z, beta_0, beta_1)
SSPG_value

# Calcular la probabilidad de tener diabetes para un valor de SSPG de SSPG_value
p_z <- 1 / (1 + exp(-(beta_0 + beta_1 * SSPG_value)))
p_z

# Utilizando predict para calcular la probabilidad de tener diabetes
p_z1 <- predict(mod_1, newdata = data.frame(SSPG = SSPG_value), type = "response")
p_z1


# Test de Hosmer-Lemeshow
library(ResourceSelection)
hoslem.test(diabetes$DIABET, mod_1$fitted.value)
HL <- hoslem.test(mod_1$y, fitted(mod_1), g = 10)
cbind(HL$observed, HL$expected)

# Tabla de clasificación
# Matriz de confusión
table(DIABET,1*(predict(mod_1)> 0.5))

# Sensibilidad (Recall o True Positive Rate)
# Es la probabilidad de que el modelo prediga que una persona tiene diabetes dado que en realidad tiene diabetes
sensibilidad <- sum((DIABET == 1) & (predict(mod_1) > 0.5)) / sum(DIABET == 1)
sensibilidad

# Especificidad (Specificity o True Negative Rate)
# Es la probabilidad de que el modelo prediga que una persona no tiene diabetes dado que en realidad no tiene diabetes
especificidad <- sum((DIABET == 0) & (predict(mod_1) <= 0.5)) / sum(DIABET == 0)
especificidad

# 1 - Especificidad
# Es la probabilidad de que el modelo prediga que una persona tiene diabetes dado que en realidad no tiene diabetes
1 - especificidad

# Valor predictivo positivo (VPP)
# Es la probabilidad de que una persona tenga diabetes dado que el modelo predice que tiene diabetes
VPP <- sum((DIABET == 1) & (predict(mod_1) > 0.5)) / sum(predict(mod_1) > 0.5)
VPP

# Valor predictivo negativo (VPN)
# Es la probabilidad de que una persona no tenga diabetes dado que el modelo predice que no tiene diabetes
VPN <- sum((DIABET == 0) & (predict(mod_1) <= 0.5)) / sum(predict(mod_1) <= 0.5)
VPN

# Calcular la exactitud (accuracy)
# Es la probabilidad de que el modelo prediga correctamente si una persona tiene o no diabetes 
accuracy <- sum((DIABET == 1 & predict(mod_1) > 0.5) | (DIABET == 0 & predict(mod_1) <= 0.5)) / length(DIABET)
accuracy

# Calcular el área bajo la curva ROC
library(pROC)
roc_curve <- roc(DIABET, predict(mod_1, type = "response"))
plot(roc_curve, col = "blue", lwd = 2, main = "Curva ROC")
auc(roc_curve)

roc(DIABET=='1', mod_1$fitted.values, plot = TRUE, legacy.axes =TRUE, add=TRUE,print.auc = TRUE, percent = TRUE, xlab = 'Porcentaje Falsos positivos',ylab = 'Porcentaje verdaderos postivios',col = '#377eb8', lwd = 2 ,xlim=c(100,0))
# Medir el intervalo de confianza del AUC
ci(roc_curve)
# Mostrar el valor que maximiza la sensibilidad y especificidad
coords(roc_curve, "best", ret = c("threshold", "accuracy", "sensitivity", "specificity"))
# a que valor de SPPG corresponde el mejor punto de la curva ROC
coords(roc_curve, "best", ret = c("threshold", "accuracy", "sensitivity", "specificity"))$threshold

# Calcular el índice de Youden
coords(roc_curve, "best", ret = c("threshold", "accuracy", "sensitivity", "specificity"))$sensitivity + coords(roc_curve, "best", ret = c("threshold", "accuracy", "sensitivity", "specificity"))$specificity - 1
