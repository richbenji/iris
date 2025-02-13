################################
# Statistiques descriptives ####
################################

# setwd(...)
rm(list=ls())


### CHARGER LES DONNEES
data(iris)
View(iris)

#str(iris)

### SUMMARY
summary(iris)

# test t pairé pour comparer les longueurs des petales/sepales
t.test(iris$Sepal.Length, iris$Petal.Length, 
       paired = TRUE)

### SCATTERPLOT MATRIX
?pairs


pdf("scatterplotMatrix.pdf")
pairs(iris[,1:4], col = iris$Species,
      pch = as.numeric(iris$Species),
      main = "Scatterplot Iris")
dev.off()
#parameter col: les couleurs par défaut (1=noir, 2=rouge, 3=vert, ...)
#parameter pch: plotting symbols (1=cercle, 2=triangle, 3="+")


# autre façon de travailler: exercice
# customiser les couleurs et symboles du scatterplot

color <- as.character(iris$Species)
color[color == "setosa"] <- "blue"
color[color == "versicolor"] <- "green"
color[color == "virginica"] <- "red"

pch <- as.character(iris$Species)
pch[pch == "setosa"] <- 6
pch[pch == "versicolor"] <- 7
pch[pch == "virginica"] <- 8
pch <- as.numeric(pch)

pairs(iris[,1:4], col = color,
      pch = pch,
      main = "Scatterplot Iris")


