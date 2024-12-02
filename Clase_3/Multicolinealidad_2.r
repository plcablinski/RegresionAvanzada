rm(list=ls())
# Cargar librería necesaria
if (!require(olsrr)) install.packages("olsrr")
library(olsrr)

# Simulación de datos
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- 0.5 * x1 + rnorm(n, sd = 0.1)  # Correlacionado con x1
x3 <- rnorm(n)                      # Independiente
y <- 3 + 2 * x1 + 0.5 * x2 + 1.5 * x3 + rnorm(n)

# Crear el modelo de regresión
modelo <- lm(y ~ x1 + x2 + x3)

# Calcular índice de condición usando olsrr
condicion <- ols_coll_diag(modelo)
summary(condicion)

# Ver los resultados
print("Índice de condición:")
print(condicion$condition_index)

# Identificar posibles problemas de multicolinealidad
if (any(condicion$condition_index > 30)) {
  cat("Alerta: El modelo tiene índices de condición altos (> 30), lo que indica severa multicolinealidad.\n")
} else {
  cat("No hay problemas significativos de multicolinealidad según el índice de condición.\n")
}


# model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
? mtcars 
# vif and tolerance
ols_vif_tol(model)

# eigenvalues and condition indices
ols_eigen_cindex(model)

# collinearity diagnostics
ols_coll_diag(model)

? ols_coll_diag
