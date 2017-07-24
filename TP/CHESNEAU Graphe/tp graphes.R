setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP/CHESNEAU Graphe")
colors()
demo(graphics)

dev.off()

enquete = read.table("http://www.math.unicaen.fr/~chesneau/enquete.txt",header = T)
enquete
attach(enquete)
str(enquete)

stripchart(poids)
dotchart(poids)
dotchart(sort(poids))

plot(poids)

plot(c(4, 1, 2, 6, 5, 9, 7, 10, 3, 8), poids)

enquete$poids
enquete$couleur
enquete$nb

plot(poids, type = "l", lty = 2, axes = F, main = "poids des personnes")

plot(poids, type = "o")

plot(poids, type = "b", xlab = "numero")

plot(poids, pch = 4, xlab = "numero")

plot(poids, type = "b", pch = "a", xlab = "numero")

plot(poids, type = "b", xlab = "numero", xlim = c(-10, 20), ylim = c(30,90))

plot(poids, type = "b", xlab = "numero", xlim= c(-10, 20), ylim = c(30, 90), xaxt="n",yaxt="n")

plot(poids, pch = 19, xlab = "numero", col = rainbow(10))

plot(poids, pch = 19, xlab = "numero", col = "#1200FF")

plot(poids, pch = "a", xlab = "numero", cex = 3)

plot(poids, type = "l", lty = 24)

plot(poids, type = "l", lty = 2, axes = F)

plot(poids, type = "l", main = "poids des personnes")

plot(poids, type = "l", lwd = 5)

curve(sin(x), -3, 10)

fonc = function(x) { sin(cos(x) * exp( -x / 2)) }
curve(fonc, -8, 7)

curve(fonc, -8, 7, n = 2001)

curve(fonc, -8, 7, lwd = 5)

barplot(poids)

barplot(table(couleur))

barplot(table(nb))

barplot(poids, xlim = c(-3, 33), width = 0.8)

barplot(poids, space = 2)

barplot(poids, xlim = c(-3, 13), ylim = c(0, 100))

barplot(poids, xlim = c(-3, 33), width = 0.8)

prenoms = c("Karen", "Elodie", "Paul", "Paul", "Elsa", "Karen", "Aurelie",
            "Elsa", "Karen", "Sophie")
barplot(poids, space = 2, names.arg = prenoms)

barplot(poids, space = 2, names.arg = prenoms, cex.names = 0.6)

barplot(poids, horiz = TRUE)

pie(couleur)

pie(table(couleur))

hist(poids)

hist(poids, prob = TRUE)

hist(poids, breaks = c(45, 60, 75), prob = T)

hist(poids,breaks = c(45, 60, 75))

hist(poids,breaks = c(45, 60, 75), prob = TRUE)

hist(poids,breaks = c(45, 50, 70, 75))

hist(poids, right = FALSE)

hist(poids,right = TRUE)

boxplot(poids)

boxplot(poids ~ couleur)

par(mfrow = c(2, 1))
hist(poids)
plot(poids)

par(mfcol = c(1, 2))
hist(poids)
plot(poids)

par(mfrow = c(1, 1))

par(ask = TRUE)
hist(poids)
plot(poids)
par(ask = FALSE)

plot(poids)
points(c(2, 2, 6.3), c(55, 60, 70), cex = 2, pch = 5, col = "red")

plot(poids)
lines(1:8, rep(60, 8) + c(-10, 10), lty = 3,col="blue")

plot(poids)
text(7, 72, paste("moyenne = ", mean(poids)))


prenoms = c("Karen", "Elodie", "Paul", "Paul", "Elsa", "Karen", "Aurelie",
            "Elsa", "Karen", "Sophie")
plot(poids)
text(poids + c(rep(-1, 4), 1, rep(-1, 5)), prenoms, cex = 0.7)

plot(poids)
abline(0,8,col="blue")

plot(poids)
curve(60 + sin(7 * x), add = T)

plot(poids)
arrows(c(4, 8), c(55, 60), c(2, 10), c(70, 65))

plot(poids)
title(paste("Graphique du poids des", length(poids), "personnes"))

plot(poids, yaxt = "n")
axis(2, 1:100, 1:100)

plot(poids, type = "l")
lines(c(2, 3, 5), c(50, 48, 62), col = "red")
legend(8, 65, legend = c("poids", "ajout"), fill = c("black", "red"))

plot(poids, type = "l")
lines(c(2, 3, 5), c(50, 48, 62), col = "red")
legend("topleft", legend = c("poids", "ajout"), fill = c("black", "red"))
colors()
plot(poids, type = "o")
grid(lwd = 2,col="turquoise")

plot(x1, y1, xlim = limx, ylim = limy, type = "n")

library(ggplot2)

ggplot(enquete, aes(x = 1:10, y = poids)) + geom_point()

ggplot(enquete, aes(x = 1:10, y = poids)) + geom_point(aes(color = nb))

p1 = ggplot(enquete, aes(x = 1:10, y = poids)) + geom_point(aes(color = nb, shape = couleur))
p1

p1 + scale_x_continuous(name = "individus")

ggplot(enquete, aes(x = couleur, y = poids)) + geom_boxplot()

library(plotrix)
table(couleur)
pie3D(table(couleur), explode = 0.1, labelcex = 1,labels=levels(couleur))

####################################################################################
####################################################################################
#############################EXERCICES##############################################
####################################################################################

### Exo 1
#Nous déclarons les fonctions.
f= function(x){
  x^2 - 6*x - 3
}
g = function(x){
  x^2 + x - 1 
}
# On trace la première fonction f(x) et définissons l'environnement graphique
curve(f(x), -12, 12, col = "red", xlab = "x", ylab = "y", lwd = 2,
      main = "Graphiques de f(x) et g(x)")
# Nous ajoutons la grid
grid()
# Nous ajoutons la fonction g(x)
curve(g(x), col = "blue", lwd = 2, add = TRUE)
# Nous ajoutons la légende
legend("topright", col = c("red", "blue"), lty=1, legend = c("f(x)", "g(x)"))

### Exo 2 
#Import des données 
enquete = read.table("http://www.math.unicaen.fr/~chesneau/enquete.txt",header = T)
#Affichage des données du data frame
str(enquete)
attach(enquete)
#On définit trois nouveaux objets (les variables du dataframe), sur lesquels on pourra faire appel directement
#Pas besoin de faire enquete$poids ...
bppoids = barplot(
  sort(poids, decreasing = T), 
  ylim = c(0, 80), 
  main = "Poids",
  col = heat.colors(10)
)
text(
  bppoids, 
  sort(poids, decreasing=T) + 1.5, ## Permet d'afficher les résultats textuels (1,5 unité au dessus des graphes) 
  sort(poids, decreasing = T)
)

axis(
  1, 
  at = bppoids,
  labels = order(poids, decreasing = T)
)

###
tcoul = table(couleur) # On récupère les effectifs de couleurs et on les stock dans un vecteur (Blanc : 2, Bleu : 4, Noir : 3, Rouge : 1)
bpcoul = barplot(
  tcoul, 
  ylim = c(0,10), 
  main = "Couleur choisie",
  col = heat.colors(4) ## Dégradé à 4 nuances
)
text(
  bpcoul, 
  tcoul + 0.5, 
  tcoul
)
## On va rajouter l'axe de droite correspondant aux fréquences
axis(
  4, 
  at = (0:10),
  labels = seq(0, 1, by = 0.1)
)
## Et on rajoute des tirets pour sous chaque barre pointants vers le nom de la couleur
axis(
  1, 
  at = bpcoul,
  labels = names(tcoul)
)

### 

tcoul = table(couleur)
bpcoulbis = 
  barplot(
    sort(tcoul, decreasing = T), 
    ylim = c(0, 10),
    main = "Couleur choisie", 
    col = heat.colors(4)
)
text(
  bpcoulbis, 
  sort(tcoul, decreasing = T) + 0.5, 
  sort(tcoul, decreasing = T)
)
axis(
  4, 
  at = (0:10), 
  labels = seq(0, 1, by = 0.1)
)
axis(
  1, 
  at = bpcoulbis, 
  labels = names(tcoul)[order(tcoul, decreasing = T)]
)

## Exo 3

nba = read.table("http://www.math.unicaen.fr/~chesneau/nba.txt",header = T, sep = ",")
str(nba)
## Etat initial.
names(nba)
## Surcharge des noms des variables
names(nba) = c("joueurs", "role", "taille", "poids", "age")
names(nba)

attach(nba)

stripchart(poids)

hist(poids)

hist(taille)

pie(table(role),col = heat.colors(3))

boxplot(poids~role,col = topo.colors(3))

range(names(table(age)))
nb <- 40-15
boxplot(poids~age,col=terrain.colors(nb))
boxplot(taille~age,col=rainbow(nb))

plot(taille, poids)

h = function(x){
  exp(-4.59678)*x^(2.28551)
}
curve(h, 66, 88, col = "red", lwd = 2, add = TRUE)

library(ggplot2)

ggplot(nba, aes(x = taille, y = poids)) +
  geom_point(aes(color = age, shape = role))

library(ggplot2)
ggplot(nba, aes(x = taille, y = poids)) + geom_point(shape=1) + geom_smooth(, se = FALSE,method = 'loess')
## Exo 4
tdens = function(x, nu) {
  (1 / sqrt(nu * pi)) * (gamma((nu + 1)/2) / gamma(nu / 2)) * (1 + x^2 / nu)^(-(nu + 1) / 2)
}
## On ajoute les 4 courbes pour des valeurs de nu différentes
### nu = 1 + Les paramètres de la fenêtre graphique
curve(tdens(x, 1), xlim = c(-6, 6), ylim = c(0, 0.38), col = "#0C99F9", lwd = 3,
      ylab = "", main = "Densités associées à la loi de Student")
### nu = 2
curve(tdens(x, 2), col = "#F0CF99", lwd = 3, add = TRUE)
### nu = 5
curve(tdens(x, 5), col = "#CFFFF9", lwd = 3, add = TRUE)
### nu = 16
curve(tdens(x, 16), col = "#CC00F9", lwd = 3, add = TRUE)
## On trace les axes de centrage
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
## On ajoute les légende
legend("topright",
       legend = c(
         expression(paste(nu, "=1")),
         expression(paste(nu, "=2")),
         expression(paste(nu, "=5")),
         expression(paste(nu, "=16"))),
       lwd = 3, col = c("#0C99F9", "#F0CF99", "#CFFFF9", "#CC00F9"))


# Exo 5
## Le séparateur de variable ici est un espace.
x = scan("http://www.math.unicaen.fr/~chesneau/norm.txt", sep = " ")
hist(
  x, 
  main = "Histogramme de x et fonction f", 
  ylab = "Fréquence",
  freq = FALSE, 
  col = cm.colors(14)
)
f = function(x) { 
  (1/sqrt(2 * pi)) * exp(- x^2 / 2) 
}
curve(f, 
      -4, 
      4, 
      col = "darkblue", 
      lwd = 2, 
      add = T)
legend(
  "topleft", 
  col = c(cm.colors(1), "darkblue"), 
  lty = 1,
  legend = c("histogramme", "fonction f")
)

f = function(x) { 
  (1/sqrt(2 * pi)) * exp(- x^2 / 2) 
}

par(mfrow = c(2,1))
hist(
  x, 
  main = "Histogramme de x", 
  ylab = "Fréquence", 
  freq = FALSE,
  col = cm.colors(14)
)
legend(
  "topleft", 
  fill = cm.colors(2), 
  legend = "histogramme"
)
curve(f, 
      -4, 
      4, 
      col = "darkblue", 
      lwd = 2, 
      main = "fonction f"
)
legend(
  "topleft", 
  fill = "darkblue", 
  legend = "fonction f"
)

par(mfrow = c(1,1))