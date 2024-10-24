# Testear normalidad de las variables X y W multivariadas.
# Si hay normalidad multivariada testeada con Henze-Zirkler's Multivariate Normality Test
# entonces se puede usar la correlación de Pearson.
library(MVN)
X = c(60,61,61,62,63,64,65,68,70)
W = c(125,130,120,135,130,140,140,160,169)
data = data.frame(X,W)
result = mvn(data, mvnTest = "hz") # Henze-Zirkler's Multivariate Normality Test
result$multivariateNormality
cor(X,W)
cor.test(X,W)

# Si no hay normalidad multivariada, se puede usar la correlación de Spearman.
x <- c(196.5, 199.1, 199.9, 204.2, 204.2, 207.4, 234.1, 181.7, 183, 192.8)
y <- c(0.76, 1.11, 1.66, 0.96, 1.21, 1.14, 1.53, 1.51, 1.28, 0.84)
data = data.frame(x,y)
result = mvn(data, mvnTest = "hz") # Henze-Zirkler's Multivariate Normality Test
result$multivariateNormality
cor.test(x, y, method = "spearman")



