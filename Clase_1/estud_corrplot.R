library(dplyr)
library(corrplot)
library(ggcorrplot)
library(pheatmap)
library(corrgram)

estud <- read.csv("C:\\RegresionAvanzada\\Datos_estud.csv", sep = ",")
base_est <- estud %>% select(read, write, math, science, socst) 

corrplot( M, tl.col = "red", bg = "white", tl.srt = 35, addCoef.col = "black", type = "full")

#ggcorrplot(M, lab = TRUE, lab_col = "black", lab_size = 3, colors = c("blue", "white", "red"))
#heatmap(M, col = colorRampPalette(c("red", "white", "blue"))(20), symm = TRUE)
#pheatmap(M, color = colorRampPalette(c("blue", "white", "red"))(50))
#corrgram(M, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)
