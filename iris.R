# Affectation du jeu de données iris
iris <- iris

# Mettre 2 graphiques côte à côte
par(mfrow = c(1,2))

# Histogramme de la variable Sepal Length de iris
  # Calcule du ylim
  densite <- dnorm(iris$Sepal.Length,
                   mean=mean(iris$Sepal.Length),
                   sd=sd(iris$Sepal.Length))
  histo <- hist(iris$Sepal.Length,
                plot = FALSE) # plot = FALSE permet d'afficher les paramètres du graphe au lieu de faire le graphe
  limiteY <- max(densite, histo$density)

hist(iris$Sepal.Length,
     col = "light grey",
     freq = FALSE,
      # autre façon : prob=TRUE => !freq
     xlab = NA,
     main = NA,
     ylim = c(0, limiteY),
     cex.axis = 0.6,
     cex.lab = 0.6,
     mgp = c(1.5,0.5,0)
     )

# Densité normale
curve(dnorm(x,
            mean= mean(iris$Sepal.Length),
            sd= sd(iris$Sepal.Length)
            ),
      col = "blue",
      add = T
      )

# QQ-plot
qqnorm(iris$Sepal.Length,
       main = NULL,
       labels = TRUE,
       cex = 0.6,
       cex.axis = 0.6,
       cex.lab = 0.6,
       mgp = c(1.5,0.5,0)
       )
qqline(iris$Sepal.Length)
legend("topleft",
       cex = 0.6,
       legend = c(paste ("Moyenne :",
                  round(mean(iris$Sepal.Length),
                        digits = 2)),
                  paste("Ecart-type :",
                  round(sd(iris$Sepal.Length),
                        digits = 2)
                  )
                  )
       )

# Rajouter le titre
mtext(text = "Test de la normalité de la variable Sepal Length",
      side = 3,
      outer = TRUE,
      line = -2)

# Fonction test de normalité
test.normalite <- function(x, figure = TRUE, var.name = NULL) {
  if (figure == TRUE) {
    par(mfrow = c(1,2))
    
    # Histogramme de var.name
      # Calcul de la limite de l'axe Y
    densite <- dnorm(x, mean=mean(x), sd=sd(x))
    histo <- hist(x,plot = FALSE) # plot = FALSE permet d'afficher les paramètres du graphe au lieu de faire le graphe
    limiteY <- max(densite, histo$density)
    
    hist(x,
         col = "light grey",
         freq = FALSE,
         xlab = NA,
         main = NA,
         ylim = c(0, limiteY),
         cex.axis = 0.6,
         cex.lab = 0.6,
         mgp = c(1.5,0.5,0)
    )
    
    # Densité normale
    curve(dnorm(x,
                mean= mean(x),
                sd= sd(x)
                ),
    col = "blue",
    add = T
    )
    
    # QQ-plot
    qqnorm(x,
           main = NULL,
           labels = TRUE,
           cex = 0.6,
           cex.axis = 0.6,
           cex.lab = 0.6,
           mgp = c(1.5,0.5,0)
    )
    qqline(x)
    legend("topleft",
           cex = 0.6,
           legend = c(paste ("Moyenne :",
                             round(mean(x),
                                   digits = 2)),
                      paste("Ecart-type :",
                            round(sd(x),
                                  digits = 2)
                            )
           )
    )
    
    # Rajouter le titre
    mtext(text = paste("Test de la normalité de la variable", var.name),
          side = 3,
          outer = TRUE,
          line = -2)
  }
  if (shapiro.test(x)$p.value > 0.05) {
    cat(paste0("La variable ", var.name, " est normale selon le test de Shapiro-Wilk (pvaleur : ", shapiro.test(x)$p.value, ")\n"))
    }
  else {
    cat(paste0("La variable ", var.name, " n'est pas normale selon le test de Shapiro-Wilk (pvaleur : ", shapiro.test(x)$p.value, ")\n"))
    }
}

# Fonction finale
fonction.finale <- function(df, figure = TRUE) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[,i]) == TRUE) {
      test.normalite(df[,i], figure = TRUE, var.name = colnames(df[i]))
    }
    else {
      #cat(paste0("la variable ", colnames(df[i]), " n'est pas numérique\n"))
      message("la variable ", colnames(df[i]), " n'est pas numérique !!!\n")
    }
    }
  }

# Test de ma fonction sur iris et pressure

pressure <- pressure

liste.df <- list(iris, pressure)

lapply(liste.df, fonction.finale)
