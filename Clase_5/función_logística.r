# Parámetros
alpha <- -4.548
beta <- 0.025

# Valores de x entre 0 y 1
x <- seq(0, 500, length.out = 1000)

# Función logística
p_x <- 1 / (1 + exp(-(alpha + beta * x)))

# Graficar
plot(x, p_x, type = "l", col = "blue", lwd = 2,
     main = expression(p(x) == frac(1, 1 + e^-(alpha + beta * x))),
     xlab = "x", ylab = "p(x)")
grid()

