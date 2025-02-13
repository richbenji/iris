###################
# Clustering  #####
###################


#setwd(...)
rm(list=ls())
data(iris)

irisNumeric <- iris[,1:4]
irisEspece <- iris$Species

#### Clustering hierarchique ############

# 1. Matrice des distances
?dist
iris.dist <- dist(irisNumeric, method = "euclidean")
class(iris.dist) # objet de classe dist 
# View(as.matrix(iris.dist)) # pour faciliter la visualisation


#2. Algorithme de clustering hiérarchique
?hclust # package stats
# input: matrice de distances (classe dist), output: object of class hclust
# complete linkage est le par défaut ("ward.D", "ward.D2", "single", "complete",...)
iris.hc.complete <- hclust(d = iris.dist,  method = "complete")
str(iris.hc.complete)

#3. Dendrogramme

#pdf("dendro.pdf",width=10)
plot(iris.hc.complete,  cex = 0.4, hang=-1) # labels sont les identifiants
plot(iris.hc.complete, labels = irisEspece, cex = 0.4, hang=-1) # labels sont les espèces 
#dev.off()

# Dendrogramme HORIZONTAL avec plot.dendrogram(),
# après conversion en un objet "dendrogram" avec as.dendrogram()
iris.hc.complete$labels <- irisEspece
par(mar=c(4,4,4,4))
plot(as.dendrogram(iris.hc.complete), horiz = TRUE) #admet param horiz

# Dendrogramme COLORIE 
dendro <- as.dendrogram(iris.hc.complete)
# class(dendro)
classNum <- as.numeric(irisEspece)
color <- classNum[order.dendrogram(dendro)] 
#order.dendrogram(dendro): indices des feuilles gauche à droite (ordre du dendrogramme)
require(dendextend) # visualize and compare trees
dendextend::labels_colors(dendro) <- color
# fonction label_colors du package dendextend

pdf("Dendrogram.pdf", width = 10, height = 10)
par(mar=c(4,4,4,7))
plot(dendro, horiz = TRUE) 
dev.off()


#4. Clustering criterion (distance)

criterion <- iris.hc.complete$height
plot(criterion, main = "Clustering criterion (distance)", type="b")

iris.hc.complete$merge #description chronologique du processus itératif de fusion (149 étapes)

head(iris.hc.complete$merge)
head(iris.hc.complete$height)
tail(iris.hc.complete$merge)
tail(iris.hc.complete$height)
#max(iris.dist)

# 5. Obtenir les cluster à partir du dendrogramme
plot(dendro)
rect.hclust(iris.hc.complete, k = 4) # ajout de rectangles

# cutree: on va couper dans l'arborescence (toujours le même dendrogramme) 
# soit à une hauteur donnée (paramètre h)
# soit à une hauteur calculée permettant une division en k groupes (paramètre k donné)

# cutree has an implementation in stat and in dendextend
CutTree <- stats::cutree(iris.hc.complete, h = 6)
CutTree <- stats::cutree(iris.hc.complete, k = 3)
CutTree

# table de contingence
table(CutTree, irisEspece)
# !!! les 3 clusters ne correspondent pas aux 3 espèces 
# impossible de bien séparer les versicolor des virginica
# besoin d'autres stratégies


# pour info:
#6. Indice de Rand
require(phyclust)

RI_HC = RRand(trcl = classNum, prcl = CutTree, lab = NULL)[1]$Rand
ARI_HC = RRand(trcl = classNum, prcl = CutTree, lab = NULL)[2]$adjRand

# K-means ###############
?kmeans

iriskmeans <- kmeans(irisNumeric, 
                      centers = 4,
                      nstart = 20,
                      iter.max = 20)


# pour neutraliser l'effet du choix aléatoire des centres de départ 
# on peut faire plusieurs répétitions (paramètre nstart) et garder le meilleur résultat
# en termes de ratio between_SS / total_SS  (maximum)

str(iriskmeans) 

# iriskmeans$cluster: étiquettes des clusters
# table de contingence
table(iriskmeans$cluster, irisEspece)

# iriskmeans$centers: centres de gravité des clusters



iris2D <- iris[,c(1,2),]
km2 <- kmeans(iris2D,centers=2)
km3 <- kmeans(iris2D,centers=3)
km4 <- kmeans(iris2D,centers=4)
km5 <- kmeans(iris2D,centers=5)

par(mfrow=c(2,2))
plot(iris$Sepal.Length,iris$Sepal.Width,col=km2$cluster,xlab='Sepal Length',
        ylab='Sepal Width',main='k = 2', pch=classNum)
plot(iris$Sepal.Length,iris$Sepal.Width,col=km3$cluster,xlab='Sepal Length',
        ylab='Sepal Width',main='k = 3', pch=classNum)
plot(iris$Sepal.Length,iris$Sepal.Width,col=km4$cluster,xlab='Sepal Length',
        ylab='Sepal Width',main='k = 4', pch=classNum)
plot(iris$Sepal.Length,iris$Sepal.Width,col=km5$cluster,xlab='Sepal Length',
        ylab='Sepal Width',main='k = 5', pch=classNum)

# clustering hiérarchique sur données standardisées
apply(irisNumeric,2,sd)

irisNumeric.CR <- scale(irisNumeric, center = TRUE, scale = TRUE)
#apply(irisNumeric.CR,2,sd)

iris.dist.CR <- dist(irisNumeric.CR,method='euclidean')
iris.hc.complete.CR <- hclust(iris.dist.CR,method='complete')

par(mfrow=c(1,1))
plot(iris.hc.complete.CR,label=irisEspece,hang = -1, cex=1.5)
