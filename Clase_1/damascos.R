#SUPHOJA 44.09 36.67 51.72 36.04 38.97 41.28 42.06 53.33 
#PESOF 49.29 49 43.04 66.79 63.11 43.8 39.63 44.98 
#LONGF 46.34 44.58 40.06 51.3 50.7 40.29 39.71 40.1
#SUPHOJA 40.14 39.31 33.53 36.88 36.94 34.13 42.03 41.55
#PESOF 21.44 38.75 40.96 39.39 54.7 44.65 39.65 47.16 
#LONGF 29.63 37 37.38 36.14 49.33 42.37 37.4 39.42

library(ggplot2)
suphoja <- c(44.09, 36.67, 51.72, 36.04, 38.97, 41.28, 42.06, 53.33, 40.14, 
             39.31, 33.53, 36.88, 36.94, 34.13, 42.03, 41.55)

pesof <- c(49.29, 49, 43.04, 66.79, 63.11, 43.8, 39.63, 44.98, 21.44, 38.75,
           40.96, 39.39, 54.7, 44.65, 39.65, 47.16)

longf <- c(46.34, 44.58, 40.06, 51.3, 50.7, 40.29, 39.71, 40.1, 29.63, 37,
           37.38, 36.14, 49.33, 42.37, 37.4, 39.42)
#graficar los datos en un diagrama de dispersión, en el eje x longf y en el y pesof
# mostrar las medias en el gráfico indicando que valor toma cada media
texto1 <- paste('Media de longf:', mean(longf))
texto1
texto2 <- paste('Media de pesof:', mean(pesof))
texto2
ggplot(data.frame(longf, pesof), aes(x = longf, y = pesof)) +
  geom_point() +
  geom_vline(xintercept = mean(longf), linetype = 'dashed', color = 'red') +
  geom_hline(yintercept = mean(pesof), linetype = 'dashed', color = 'blue') +
  geom_text(aes(x = mean(longf), y = 50, label = texto1 ), color = 'red') +
  geom_text(aes(x = 40, y = mean(pesof), label = texto2 ), color = 'blue') +
  labs(x = 'Longitud de la fibra', y = 'Peso de la hoja') +
  theme_bw()
# agregar los valores correspondientes a cada media


