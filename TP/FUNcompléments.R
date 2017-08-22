age <- c( 18,27,34,18,24,NA,30,28,19,19 )
age
sexe <- c("F","F","M","F","M","M","M","F","M","F")
sexe

length(age)
length(sexe)
mode(age)
mode(sexe)


opin <- c(1,3,2,1,4,1,5,3,2,2)
factor(opin)

opin <- factor(opin, labels =c("Pas du tout d'accord","Moyennement d'accord","Sans opinion","Assez d'accord","Tout à fait d'accord"))
opin

nlevels(opin)

levels(opin)

levels(opin)[4:5]

levels(opin)[4:5] <- "Assez ou tout a fait d'accord"

opin

factor(opin, ordered = TRUE)

sexe <- factor(sexe)
sexe
levels(sexe)

sexe <- relevel(sexe,"M")
sexe

test <- factor(c("1","oui","Oui","Non","non","3"))
test

age[1]

age[c(1,2,3)]
sexe[c(1,2,3)]

age[1:3]
sexe[1:3]

is.na(age)

age
which(is.na(age))
age
sexe
age
sexe
sexe[which(age>25)]

sum(age)

sum(age[-6])

sum(age, na.rm = TRUE)

sum(age^2, na.rm = TRUE)

age
age[is.na(age)]<-mean(age, na.rm = TRUE)
age
n <- length(age) ## Nombre d'observations
n
age - mean(age) ## Ecart à la moyenne 

(age - mean(age))^2 ## Carrés des écarts à la moyenne 

sum((age - mean(age))^2)/(n-1)

sqrt(sum((age - mean(age))^2)/(n-1))

sd(age)

res <- sum(age , na.rm = TRUE)
res

rm(test)
ls()

rm(n,res)
ls()

sexe
age
d <- data.frame(age, sex = sexe)
d

dim(d)
nrow(d)
ncol(d)
names(d)
colnames(d)
rownames(d)
str(d)

age[is.na(age)]<-mean(age, na.rm = TRUE)
age

age[is.na(age)]<-mean(age, na.rm = TRUE)
d$age
d$age[1:2]
d$age[1:7]

d$var1 <- 1:10
d[1:3,]
d[,3] <- NULL
d[1:3,]
d$opinion <- opin
str(d)
d

summary(d)
setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")
getwd()

f <- read.table("données.txt",header = TRUE, sep="",dec=".")
f

f <- read.csv("données.csv")
f
##############################

Salut <- function( x )
{
  # La fonction "cat" imprime dans la console
  # "\n" provoque un retour de chariot
  cat("Bonjour", x, "\n")
}

Additionne <- function( a, b )
{
  # On additionne les valeurs a et b
  c <- a + b
  # Imprime le résultat de l’addition
  cat( c , "\n")
}

Obj1 <- 5
Obj2 <- 2
Additionne( a = Obj1, b = Obj2 )
Additionne(Obj1, Obj2)
Additionne(5, 2)

NbPneu <- function(voiture=0, bicyclette=0, monocycle=0, tricycle=0)
{
  # Dans cette fonction, nous calculons le nombre total de pneus
  A <- voiture * 4
  # Le Nb de pneus de voiture
  B <- tricycle * 3
  # Le Nb de pneus de tricycle
  C <- bicyclette * 2
  # Le Nb de pneus de bicyclette
  D <- monocycle * 1
  # Le Nb de pneus de monocycle
  NbPneuTotal <- A + B + C + D
  # Retourne le nombre total de pneus
  return( NbPneuTotal )
}

NbPneu(voiture=4, bicyclette=12, monocycle=5, tricycle=0)

NbPneu( voiture=2, bicyclette=0, monocycle=0, tricycle=1 )
# Vous obtenez 11
NbPneu( 2, 0, 0, 1 ) # Vous obtenez 11
NbPneu( voiture=2, tricycle=1 ) # Vous obtenez 11
NbPneu( 2, 1 ) # Oups… le résultat n’est plus le même!

CoefVar <- function( datum )
{
  # Fonction permettant de calculer le coefficient de variation
  # Applicable seulement pour des données ayant un véritable zéro
  # Calcul de l'écart type
  EcartType <- sd(datum)
  # Calcul de la moyenne
  Moyenne <- mean(datum)
  # Calcul du coefficient de variation
  CV <- 100 * EcartType / Moyenne
  # Retourne le résultat
  return(CV)
}

TestData <- c( 1, 2, 3, 4, 5 )
CoefVar( datum = TestData )


smp <- read.csv2("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP/DONNEES/smp2.csv")
smp <- read.csv2("smp2.csv")
dim(smp) ## Dimensions
names(smp) ## Noms des variables du HEADER
str(smp) ## Structure du Data Frame (type + qq valeurs)
summary(smp) ## Résumé des données

class(smp)

rownames(smp)[1:10] #Noms des 10 premières lignes
colnames(smp)[1:10] #Noms des 10 premières colonnes

class(smp$prof)
summary(smp$prof)

class(smp$abus)
table(smp$abus,useNA = "always")

smp$abus <- factor(smp$abus, levels = c(0,1), labels = c("Non", "Oui"))
class(smp$abus)
summary(smp$abus)

smp[3,2]

smp[3,"prof"]

smp["3","prof"]

setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")
get("quartz")()
dev.set(2)  
plot(1)
dev.set(1)
plot(2)

split.screen(c(1,2))
screen(1)
plot(1,col="red")
screen(2)
plot(2,col="blue")

layout(matrix(1:4, 2, 2))
layout.show(4)

layout(matrix(1:6, 3, 2))
layout.show(6)

mat = matrix(c(1:3, 3), 2, 2)
layout(mat)
layout.show(3)
#############################################################
C1 = matrix(rnorm(200, sd = 0.5), ncol = 2)
C2 = matrix(rnorm(200, mean = 1, sd = 0.5), ncol = 2)
mat = rbind(C1, C2)

plot(C1)

plot(C1, col = "blue")
points(C2, col = "red")

plot(C1, col = "blue",
     xlim = range(mat[ ,1]),
     ylim = range(mat[ ,2]),
     main = "représentation d'un nuage de points",
     xlab = "X1", ylab = "X2"
)
points(C2, col = "red")

plot(1,
     xlim = range(mat[ ,1]),
     ylim = range(mat[ , 2]),
     main = "représentation d'un nuage de points",
     xlab = "X1", ylab = "X2",
     bty = "l", tcl = -.25
)
rect(-3, -3, 3, 3, col = "cornsilk")
points(C1, col = "blue", pch = 22, bg = "red")
points(C2, col = "red", pch = 25, bg = "yellow")

plot(iris[ ,1:4],
     bg = c("red", "green3","blue")[iris[ ,5]],
     pch = c(21, 25, 24)[iris[ ,5]],
     main = "Iris de Fisher",
     labels =
       c("Longueur\nSepale",
         "Largeur\nSepale",
         "Longueur\nPetale",
         "Largeur\nPetale"
       )
)

matrice_iris <- iris
pairs(matrice_iris[ ,1:4],
      bg = c("red", "green3", "blue")[iris[ ,5]],
      pch = c(21, 25, 24)[iris[ ,5]],
      main = "Iris de Fisher - Matrice",
      labels = c("Longueur\nSepale",
                 "Largeur\nSepale",
                 "Longueur\nPetale",
                 "Largeur\nPetale")
)

n = 500
x = rpois(n, lambda = 2)
y = rpois(n , lambda = 2)
layout(t(matrix(1:2)))
plot(x, y, pch = 19, main = "un nuage de point trompeur")
sunflowerplot(x, y, pch = 19, main = "un nuage de points moins trompeur")


library(MASS)
data(survey)
mat = matrix(c(1:2), 1, 2)
layout(mat)
hist(survey$Height, col = "yellow", border = "red",
     main = paste("Taille de", nrow(survey), " étudiants"),
     xlab = "Taille [cm]", ylab = "Effectifs", ylim = c(0, 50),
     labels = TRUE)
hist(survey$Height, breaks = seq(from = 150, to = 200, length = 20),
     col = "green3", border = "sienna",
     main = paste("Taille de", nrow(survey), " étudiants"),
     xlab = "Taille [cm]", ylab = "densité",
     proba = TRUE, labels = TRUE, ylim = c(0, 0.06))
x = seq(from = 150, to = 200, length = 100)
lines(x, dnorm(x, mean(survey$Height, na.rm = TRUE),
               sd(survey$Height, na.rm = TRUE)
)
)
mtext("Ajustement à une loi normale")

adj = 0.5
dst = density(survey$Height, na.rm = TRUE, adjust = adj)
hist(survey$Height,
     breaks = seq(from = 150, to = 200, length = 20),
     col = "yellow", border = "red",
     main = paste("Taille de", nrow(survey), " étudiants"),
     xlab = "Taille [cm]", ylab = "densité",
     proba = TRUE, labels = TRUE, ylim = c(0, 0.06))
lines(dst$x, dst$y, lwd = 2)
mtext(paste("adjust = ", adj))

adj = 1
dst = density(survey$Height, na.rm = TRUE, adjust = adj)
hist(survey$Height,
     breaks = seq(from = 150, to = 200, length = 20),
     col = "yellow", border = "red",
     main = paste("Taille de", nrow(survey), " étudiants"),
     xlab = "Taille [cm]", ylab = "densité",
     proba = TRUE, labels = TRUE, ylim = c(0, 0.06))
lines(dst$x, dst$y, lwd = 2)
mtext(paste("adjust = ", adj))


adj = 1.5
dst = density(survey$Height, na.rm = TRUE, adjust = adj)
hist(survey$Height,
     breaks = seq(from = 150, to = 200, length = 20),
     col = "yellow",
     border = "red",
     main = paste("Taille de", nrow(survey), " étudiants"),
     xlab = "Taille [cm]", ylab = "densité",
     proba = TRUE, labels = TRUE, ylim = c(0, 0.06))
lines(dst$x, dst$y, lwd = 2)
mtext(paste("adjust = ", adj))


ng = sum(survey$Sex == "Male", na.rm = TRUE)
nf = sum(survey$Sex == "Female", na.rm = TRUE)
n <- ng + nf
dst = density(survey$Height, na.rm = TRUE)
dstg = density(survey$Height[survey$Sex ==
                               "Male"], na.rm = TRUE )
dstf = density(survey$Height[survey$Sex ==
                               "Female"], na.rm = TRUE )
hist(survey$Height, col = "yellow",
     border = "red",
     main= paste("Taille de", nrow(survey),
                 "étudiants" ),
     xlab = "Taille [cm]", proba = TRUE,
     ylim = c(0, max(dst$y))
)
lines(dstg$x, ng/n * dstg$y, lwd = 3,
      col = "darkblue" )
lines(dstf$x, nf/n * dstf$y, lwd = 3, lty = 3,
      col = "darkred" )
lines(dst$x, dst$y)
legend(185, 0.04, legend = c("Filles", "Garcons"),
       col = c("darkred", "darkblue"),
       lty = c(3, 1), lwd = 2, pt.cex = 2 )

boxplot(survey$Height,
        col = "sienna",
        main = paste("Taille de", nrow(survey),
                     "etudiants"
        ),
        ylab = "Taille",
        las = 1
)
rug(survey$Height, side = 2)

boxplot(survey$Height ~ survey$Sex,
        col = c("lightpink", "lightblue"),
        main = paste("Taille de", nrow(survey),
                     "etudiants"
        ),
        ylab = "Taille",
        las = 1
)
rug(survey$Height, side = 2)


vente.tarte = c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(vente.tarte) = c("Cerise", "Framboise ", "Pomme",
                       "Abricot", "Pêche", "Fraise"
)
pie(vente.tarte,
    col = c("tan", "green3", " plum",
            " royalblue", "red", "peru"),
    border = NA,
    main = "Ventes de tartes"
)


library(lattice)
library(ade4)
data("deug")
x = deug$tab$Algebra
y = deug$result
x
y

densityplot(~ x | y,
            xlab = " note d’algèbre ",
            col = "red")

bwplot(~ x | y,
       main = "note d'algèbre")

histogram(~ x | y,
          col = "peru",
          main = "note d'algèbre",
          xlab = "histogramme")

densityplot(~ x | y, xlab = "!note d’algèbre ",
            panel = function(x, ...)
            {
              panel.mathdensity(dmath=dnorm, args=list(mean=mean(x), sd=sd(x)), col="red")
              panel.histogram(x, breaks = NULL, col = "peru")
            }
)

xyplot(iris$Petal.Length ~ iris$Petal.Width,
       groups=iris$Species,
       type = c("p", "smooth"),
       col = 2:4,
       xlab = "longueur des pétales",
       ylab = "largeur des pétales",
       main = "Les iris de Fisher",
       span = 0.75,
       key = list(
         x = 0.15,
         y = 0.85,
         points=list(
           col=2:4,
           pch = 1
         ),
         text= list(levels(iris$Species))
       )
)

splom(~iris[1:3] | Species, data = iris,
      pscales = 0,
      varnames = c("Sepal\nLength",
                   "Sepal\nWidth",
                   "Petal\nLength"
      )
)

n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)
out <- lm(y ~ x)
library(xtable)
tab <- xtable(summary(out)$coef, digits=c(0, 2, 2, 1, 2))
tab
print(tab, type="html")



smp <- read.csv2("DONNEES/smp2.csv")
names(smp)
str(smp)

summary(smp)
smp$age


mat = matrix(1:9, ncol = 3)
as.data.frame(mat)

age = c(24, 26, 22)
sexe = c("H", "H", "F")
love = c(TRUE, FALSE, TRUE)
enquete = data.frame(age, sexe, love)
enquete

sexe = factor(c("H", "H", "F"))
levels(sexe)

names(enquete) = c("Age", "Sexe", "Love")

row.names(enquete) = c("Jules", "Jim", "Elsa")
enquete

enquete = data.frame(Age = c(24, 26, 22), Sexe = c("H", "H", "F"),Love = c(TRUE, FALSE, TRUE))
enquete

str(enquete)


enquete[1:2, 2:3]

summary(enquete)

enquete$Age

attach(enquete)

Age

detach(enquete)

attach(enquete)
table(Sexe)

table(Sexe, Love)
##############################
##############################
##############################
##############################
##############################
région = c("Calvados", "manche", "Orne");région
pop = c(664000, 489500, 292337);pop
superficie = c(5548, 5938, 6103);superficie
basnor = data.frame(région,pop,superficie)
basnor
str(basnor)
basnor$région
basnor$superficie
class(basnor$région)
class(basnor$superficie)
basnor$superficie[1]
basnor$région[2]
basnor[1]
class(basnor[1])
length(basnor[1])
basnor[[1]]
basnor[1,2]
basnor[3,1]
basnor[2:3,1:2]
basnor[2:3, ]
basnor[, 2:3]
basnor[basnor$superficie < 6000, ]
basnor[basnor$superficie < 6000, c("région", "superficie")]
#Pour éviter de répéter "basnor", on aurait pu attacher la data frame :
attach(basnor)
région
basnor[superficie < 6000, c("région", "superficie")]
mean(superficie)
var(superficie)

donnees = read.table("http://www.math.unicaen.fr/~chesneau/donnees.txt",
                     header = T,na.strings = "NA")
donnees


u = c(31, 43, NA, 36, NA)
is.na(u)

mean(u, na.rm = TRUE)

donnees = read.table("donnees.txt", header = T, na.strings = "NA")
donnees[!complete.cases(donnees), ]

donnees2 = na.omit(donnees)
donnees2

a = numeric()
vec = c(1, 3, 2, 4, 7, 5, 6, 9, 8)
if (mean(vec) > 6) { a = 1:9 } else { a = rev(1:9) }
a


b = numeric()
for(i in 1:8) { b[i] = vec[i] + vec[i + 1] }
b

while (mean(vec) < 10) { vec = vec + 1 }
vec

t = 1
repeat { t = 3 * t + 1
if (t >= 15.3) break
}
t

arrangement = function(n, k) { factorial(n) / factorial(n - k) }
arrangement(7, 3)


courbe = function(x) { sqrt(x + exp(x)) }
courbe(1)

moyennemat = function(x) {
  a = numeric()
  nbcol = dim(x)[2]
  for (i in 1:nbcol) { a[i] = mean(x[,i]) }
  return(a)
}

mat = matrix(1:9, ncol = 3);mat
sum(mat[,1])/nrow(mat) # moyenne de la colonne 1 
sum(mat[,2])/nrow(mat) # moyenne de la colonne 2
sum(mat[,3])/nrow(mat) # moyenne de la colonne 3
moyennemat(mat) # On applique la fonction pour vérifier 

compter = function(a, b) {
  d = numeric()
  for(i in 1:length(a)) { d[i] = sum(b == a[i]) }
  names(d) = as.character(a)
  return(d)
}

couleur = c("rose", "vert", "jaune");couleur
fleurs = c("rose", "rose", "vert", "rose", "rose", "jaune", "jaune");fleurs


mat = matrix(1:9, ncol = 3);mat
apply(mat, 2, mean)
apply(mat, 2, max)
apply(mat, 2, min)

mat = matrix(1:9, ncol = 3);mat
mat[1,] # Première ligne
(mat[1,] > 5)
mat[2,] # Seconde ligne
(mat[2,] > 5)
mat[3,] # Troisième ligne
(mat[3,] > 5)
apply(mat, 1, function(x) { sum(x > 5)} )

liste = list(c(1, 2), c(3, 1, 2));liste
sapply(liste, mean)

lapply(liste, sd)

Age = c(24, 26, 22);Age
Sexe = c("H", "H", "F");Sexe
Love = c(TRUE, FALSE, TRUE);Love
tapply(Age, Sexe, mean)


split(Age, Sexe)

x = scan("http://www.math.unicaen.fr/~chesneau/norm.txt")
head(x, 10)
summary(x)





vec <- c(2, 2, 2, 1, 1, 1, 4, 5, 3, 4, 5, 2, 8, 9, 8, 7, 1, 3)
vec
M = matrix(vec,ncol = 3);M


mat = cbind(M,c(8, 7, 1, 2, 1, 2));mat

mat = as.data.frame(mat)
mat

colnames(mat) = c("cas", "mois", "semaine", "valeur")
mat
rownames(mat) = letters[1:6]
mat

matBis <- mat[c(2, 4, 6), ]
matBis

mat
mat = mat[order(mat$valeur), ]
mat


data(airquality)
?airquality

names(airquality)
dim(airquality)
summary(airquality)

var(airquality$Temp)
sqrt(var(airquality$Temp))

airquality[2, ]

head(airquality[ ,3],15)
length(airquality[,3])
airquality[c(1, 2, 4), ]

airquality[3:6, ]

airquality[ ,-c(1, 2)]

head(airquality[ ,-c(1, 2)],15)
dim(airquality[ ,-c(1, 2)])

airquality[airquality$Temp>90, ]

airquality$TooWindy = airquality$Wind >= 10
table(airquality$TooWindy)
airquality$TooWindy = NULL ## Réinitialisation de la variable
airquality$TooWindy

aq = airquality[ !is.na(airquality$Ozone),]
dim(aq)
head(aq,15)

library(ggplot2)
qplot(Temp, Ozone, data = airquality, colour = Month)



conso.dat = read.table("http://www.math.unicaen.fr/~chesneau/donneesconsommation.txt", header = T)
head(conso.dat,5)
str(conso.dat)

attach(conso.dat)
str(conso.dat)
summary(conso.dat)

conso.dat[Cafe=="AC" & Revenu <15000, ]

conso.dat[Enfants >3 & Pates >60 & Revenu <20000, ]

fonc1 = function(m, n){
  x = 1:n
  y = x^m
  sum(y)
}
fonc1(10,2)

fonc2 = function(x){
  n = length(x)
  x^(1:n) / (1:n)
}
c <- seq(1,10,1);c
fonc2(c)

fonc3 = function(x){
  n = length(x)
  ( x[1:(n - 2)] + x[2:(n - 1)] + x[3:n] ) / 3
}
c <- seq(1,10,0.5);c
fonc3(c)
