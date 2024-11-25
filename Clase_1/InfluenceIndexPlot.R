# Instalar y cargar el paquete car
library(car)

# Generar datos de ejemplo
set.seed(123)
x <- rnorm(20)  # Variable independiente
y <- 2 * x + rnorm(20)  # Variable dependiente

# Ajustar un modelo de regresión lineal
modelo <- lm(y ~ x)

# Generar el gráfico de índices de influencia
influenceIndexPlot(modelo,
                   id.method = "identify",  # Permite identificar puntos al hacer clic
                   main = "Gráfico de índices de influencia",
                   sub = "Distancia de Cook, leverage y residuos estudiados",
                   col = "blue",  # Color de los puntos
                   cex.axis = 0.8)  # Ajusta el tamaño de los ejes
