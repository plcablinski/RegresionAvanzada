library(MVN)
# incoporamos los datos
X = c(60,61,61,62,63,64,65,68,70)
W = c(125,130,120,135,130,140,140,160,169)
data = data.frame(X,W)
data
# guradamos los resultado del Test Henze-Zirkler en el objeto result
result = mvn(data, mvnTest = "hz")
# mostramos los resultados
result
result$multivariateNormality
# Puede sostenerse el supuesto de normalidad multivariada
cor(X,W)
# Testear la correlación
cor.test(X,W)
# Se rechaza la hipótesis nula de no correlación

y = c(196.5,199.1,199.9,204.2,204.2,207.4,234.1,181.7,183,192.8)
z = c(0.76,1.11,1.66,0.96,1.21,1.14,1.53,1.51,1.28,0.84)
data1 = data.frame(y,z)
data1
cor.test(y,z,method="spearman")
