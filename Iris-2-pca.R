#########################
# PCA ###################
#########################

# setwd(...)
rm(list=ls())


data(iris)
#View(iris)


# séparer les colonnes numériques (matrice X) de la colonne des espèces
irisNum <- iris[, 1:4]
irisSpecies <- iris[,5]

######################

### PCA

# standardisation ? 

# calcul de l'écart-type par variable
apply(irisNum, 2, sd)

# standardisation avec la fonction scale 
irisNum_sd <- scale(irisNum, center = TRUE,  scale = TRUE)

# vérification 
# apply(irisNum_sd, MARGIN = 2, FUN = sd)
# apply(irisNum_sd, MARGIN = 2, FUN = mean)

# en pratique: prétraitement se fait directement dans prcomp
help(prcomp)
iris_pca <- prcomp(irisNum, retx = TRUE, center = TRUE, scale. = TRUE)
# retx: return the scores

summary(iris_pca) 
str(iris_pca) # liste de plusieurs éléments 

# 1) proportion de variabilité expliquée par chaque CP
# pdf("screeplot_PCA.pdf", height = 5)
par(mfrow=c(1,2))
plot(iris_pca, main = "Scree plot \n Valeurs propres")
pcvar <- (iris_pca$sdev)^2/sum((iris_pca$sdev)^2)
plot(pcvar, type="b",main = "Scree plot \n % variance expliquée")
# dev.off()

# 2) Loadings 
iris_pca$rotation
# base orthonormale: vecteurs propres de norme 1

# loadings de JMP 
# (the i,jth loading is the correlation between the ith variable and the jth principal component)
# iris_pca$rotation %*% diag(iris_pca$sdev)


# 3) matrice des Scores
iris_pca$x
# var(iris_pca$x)

### Visualisation: scores plot

color <- irisSpecies

plot(iris_pca$x[,1],iris_pca$x[,2], xlab="PC1", 
     ylab="PC2", main = "Scores plot PC1 et PC2",
     col = color, pch = as.numeric(irisSpecies))
legend("topright",pch=1:3,col=1:3,legend=levels(irisSpecies))

plot(iris_pca$x[,1],iris_pca$x[,3], xlab="PC1", 
     ylab="PC3", main = "Scores plot PC1 et PC3",
     col = color, pch = as.numeric(irisSpecies))
legend("topright",pch=1:3,col=1:3,legend=levels(irisSpecies))

### Visualisation: biplot

#pdf("biplot.pdf", width = 12, height = 12)
par(mfrow=c(1,1))
biplot(iris_pca, xlabs = substr(irisSpecies,1,2),
       main = "biplot",
       cex = 1)
#dev.off()



######################

### Pour aller plus loin

# Calcul des corrélations entre CP et variables
# Petal.Length et Petal.Width fort corrélées avec PC1
cor(irisNum[,3],iris_pca$x[,1])
cor(irisNum[,4],iris_pca$x[,1])

# Visualisations toutes faites sont disponibles dans package factoextra
require(factoextra)
fviz_pca_ind(iris_pca,col.ind=irisSpecies) # score plot 
fviz_pca_biplot(iris_pca,col.ind=irisSpecies)




