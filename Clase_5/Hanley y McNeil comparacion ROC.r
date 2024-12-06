rm(list=ls())
# Instalar y cargar el paquete pROC
if (!require(pROC)) install.packages("pROC")
library(pROC)

# Datos de ejemplo
# Etiquetas de la verdadera clase (0 = negativo, 1 = positivo)
true_labels <- c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0)

# Predicciones modelo 1
scores_model1 <- c(0.9, 0.8, 0.4, 0.7, 0.3, 0.85, 0.2, 0.1, 0.88, 0.05)

# Predicciones modelo 2
scores_model2 <- c(0.85, 0.7, 0.3, 0.65, 0.25, 0.8, 0.15, 0.05, 0.9, 0.1)

# Crear las curvas ROC para ambos modelos
roc1 <- roc(true_labels, scores_model1)
auc(roc1)
roc2 <- roc(true_labels, scores_model2)
auc(roc2)

# Visualizar las curvas ROC
plot(roc1, col = "blue", main = "Comparación de Curvas ROC")
lines(roc2, col = "red")
legend("bottomright", legend = c("Modelo 1", "Modelo 2"), col = c("blue", "red"), lwd = 2)

# Comparación estadística de las AUC
test_result <- roc.test(roc1, roc2, method = "delong")
print(test_result)
