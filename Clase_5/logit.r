# Dar un ejemplo de los valores de la función logit
# Tomando el rango de i que va de 0 a 1 en pasos de 0.01

# Definir el rango de i de 0 a 1 en pasos de 0.01
i <- seq(0, 1, by = 0.00000001)

# Calcular los valores de la función logit
logit_values <- log(i / (1 - i))

# Crear un data frame para mostrar los resultados
logit_df <- data.frame(i = i, logit = logit_values)

# Mostrar los primeros registros del data frame
head(logit_df)
tail(logit_df)
# Eliminar los valores infinitos
logit_df <- logit_df[!is.infinite(logit_df$logit), ]


# Graficar los valores de la función logit
library(ggplot2)
ggplot(logit_df, aes(x = i, y = logit)) +
  geom_line() +
  labs(title = "Función Logit", x = "i", y = "logit(i)") +
  theme_minimal()
