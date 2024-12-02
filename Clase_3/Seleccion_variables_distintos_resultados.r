library(readxl)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)# para manejo de texto en variables
library(ggmosaic)
library(car)#qqplots
library(RColorBrewer)#colores
library(ggthemes)  # estilos de gráficos
library(ggrepel)   # etiquetas de texto más prolijas que las de ggplot
library(utilities)    # bandas de confianza
library(ggridges) # Para hacer gráficos de densidad facetados
library(GGally) # Para hacer varios gráficos juntos.
library(cowplot)  #Para unir gráficos generados por ggplot2
library(forcats)  #Para reordenar factores
library(ggcorrplot) # para correlogramas
library(plotrix)
library(WRS2) #para datos ejemplo anova
library(viridis)# paleta de colores
# library(kableExtra)# para tablas
library(vioplot) # para gráficos de violín
library(ggpubr)
library(tidyverse)
library(faraway)
library(tidyr)
library(leaps) #test exacto de wilcoxon
library(RVAideMemoire)# mood median test
library(lmtest)# analisis diagnostico
library(MVN)# para testear normalidad multivariada
library(ISLR)# para los datos
library(corrplot)
library(kableExtra)

options(scipen=999) #para desactivar la notación científica

suppressPackageStartupMessages(library(dplyr))

data(fat)
bodyfat <- fat %>% 
  select(-siri, -density, -free) 
bodyfat
dim(bodyfat)

bodyfat_full = regsubsets(brozek ~ ., data = bodyfat)
bodyfat_summary = summary(bodyfat_full)
bodyfat_summary

data.frame(Rsq = sprintf("%0.2f%%", bodyfat_summary$rsq * 100)) %>% kbl()
data.frame(Rsq = sprintf("%0.2f%%", bodyfat_summary$adjr2 * 100)) %>% kbl()
