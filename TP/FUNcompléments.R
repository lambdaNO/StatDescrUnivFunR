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


fonc4 = function(x) {
  if (x < 0)
    0
  else
    (1 / 10) * exp(-x / 10)
}
fonc4(-2)
fonc4(2)

fonc5 = function(k, n, p) {
  choose(n, k) * p^k * (1 - p)^(n - k)
}

fonc5(1,5,3)

fonc5(3,3,1)

fonc6 = function(k, lambda) {
  exp(-lambda) * (lambda^k / factorial(k))
}
fonc6(5,9)

Loop = function(n) {
  x = numeric()
  x[1] = 0.5
  for(j in 2:n)
    x[j] = 1 + 0.25 * x[j-1]^2
  x
}

plot(Loop(500))

###################################################################
###################################################################
###################################################################
###################################################################
setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")
smp <- read.csv2("DONNEES/smp2.csv")
names(smp)
str(smp)

gauss <- factor(smp$n.enfant)
levels(gauss)[6:13]<-"5+"
table(gauss)

names(smp) ## Affichages des noms de variables 
smp[1,1]
smp[1,"age"]  ## Méthode équivalente 

###################################################################
###################################################################
###################################################################
###################################################################




3+2
3-2
3*2
3/2
3^2
3+2;3-2;3*2;3^2
3+1+1
3+1-1
3-1-1
3*1+1
3*(1+1)
3/1+1
3/(1+1)
3^1+1
3^(1+1)
(1+2)^(1+1)
pi
pi^2
pi/3.141593
1+2+3+4+5+6+7+8+9+10
10*11/2 # Commentaire : C'est le dernier


x = 2
a = 2
b = 3
c = 4
x + c 
b*x + c
a*x^2 + b*x + c
b/(x+c)^a
x^(a+b+c)
rm(x,a,b,c) # on retire définitivement les variables x,a,b et c de la mémoire, "rm" signifie "remove"
a
a+b


Bonjour # Ce n'est pas un objet R
"Bonjour" # Cela est considéré par R car signalé en tant qu'objet extérieurà R
TRUE # Vrai ; C'est un objet R
FALSE # Faux ; C'est un objet R
T # Abréviation de TRUE; C'est un objet R
F # Abréviation de FALSE; C'est un objet R
NA # Abréviation comprise de Non Available ; C'est un objet R
3>2 # Est ce que 3 > 2 ?
3<2 # Est ce que 3 < 2 ?
3==2 # Est ce que 3 = 2 ?
2==2 # Est ce que 2 = 2 ?


vec1 = c(2.8, 2.4, 2.1, 3.6, 2.8)
vec1

vec2 = c("rouge", "vert", "vert", "vert", "jaune")
vec2

vec3 = c(TRUE, TRUE, FALSE, FALSE, FALSE)
vec3

rep(4, 3)

vec4 = rep(vec1, 2)
vec4

vec5 = rep(vec1, c(2, 1, 3, 3, 2))
vec5

vec6 = 1:10
vec6

vec7 = seq(from = 3, to = 5, by = 0.2)
vec7

vec7 = seq(from = 3, length = 11, by = 0.2)
vec7

vec8 = numeric()
vec8[1] = 41.8
vec8[2] = -0.3
vec8[3] = 92
vec8

vec9 = character()
vec9


vec10 = logical()
vec10

vecA = integer()
vecA

vec4
mat1 = matrix(vec4, ncol = 5)
mat1

mat2 = matrix(vec4, ncol = 5, byrow = T)
mat2

vec1
mat3 = cbind(vec1, 3:7)
mat3


vec1
mat3 = rbind(vec1, 3:7)
mat3

mat4 = diag(c(2, 1, 5))
mat4

mat1
mat1[,3]

mat1
mat1[2, ]

mat1
mat1[, c(2, 4, 5)]

mat1
mat1[, c(2, 4, 5)]
mat1[, c(FALSE, TRUE, FALSE, TRUE, TRUE)]

mat3
mat3[c(1, 4), ]

mat1
dim(mat1)

vec1
mat1
list1 = list(vec1, c("rouge", "bleu"), mat1)
list1

vec1
mat2
list2 = list()
list2[[1]] = vec1
list2[[2]] = c("rouge", "bleu", "vert")
list2[[3]] = mat2
list2



vec1
vec11 = vec1
vec11
vec13 = vec12 = vec11
vec12;vec13

vec1
c(vec1, c(3.9, 2.7))

vec2
c("blanc", vec2)

vec1
vec1[2]

vec1[c(2, 4)]

vec1[2:4]

vec1
vec3
vec1[vec3]

vec11
vec11[-2]


vec12
vec12[-c(2, 4)]

vec5
vec5[3:5] = c(1034, 238, -99)
vec5

vec1
names(vec1) = c("julie", "paul", "solveigh", "valentin", "elsa")
vec1

vec1["paul"]

vec1
names(vec1) = c()
vec1

vec5
mode(vec5)


vec4
as.character(vec4)
vec4
vec1
vec1 + 4

vec1
3 * vec1
3 * vec1 + 1:5

vec1
2 * vec1 
2 * vec1 - c(1, 2)

vec1
rev(vec1)

vec1
sort(vec1)
sort(vec1, decreasing = TRUE)

vec1
order(vec1)

vec1
sin(vec1)

vec1
log(vec1)+3

round(c(9.238, -1.34222))

vec5
length(vec5)

vec8
which.min(vec8)

vec8
vec8[which.min(vec8)]

vec1
mean(vec1)

vec1
sd(vec1)
sqrt((1 / (length(vec1) - 1)) * sum((vec1 - mean(vec1))^2))

vec3
sum(vec3)

paste("jules", "jim")

paste("jules", "jim", sep = "")
paste("jules", "jim", sep = ".")
paste("jules", "jim", sep = "@")

paste("X", 1:4, sep = "")

paste("X", 1:4, c("s", "o"), sep = "")

a = c(1, 4, -2, 5)
b = c(2, 4, -3, 6)
a < b

a = c(1, 4, -2, 5, 6, 7, -1)
b = c(1, 4, -3, 5, 6, 7, -2)
a == b

vec1
vec1 > 2.7

vec1 > c(2.7, 2.7, 2.7, 2.7, 2.7)


a = c(3, 2, 8, 3, -3)
b = c(1, 2)
a <= b

a <= c(1, 2, 1, 2, 1)

vec1
vec1 > 2.7
vec1[vec1 > 2.7]

vec1
vec1 > 2.7
sum(vec1 > 2.7)

vec1
(vec1 > 2.7 | vec1 < 2)

vec1
(vec1 > 2.7 & vec1 < 2)

all(c(4, 1, 3, 2, -1) <= c(2, 4))

any(c(4, 1, 3, 2, -1) <= c(2, 4))

mat1 = matrix(vec4, ncol = 5)

vec1
mat1
list1 = list(vec1, c("rouge", "bleu"), mat1)
list1
names(list1) = c("poids", "couleur", "matrice")
list1

list2 = list(poids = vec1, couleur = c("rouge", "bleu"), matrice = mat1)
list2

list1
list1$couleur

list1
list1[[2]]

vec1
vec4
mat1
dimnames(mat1) = list(c("jules", "jim"), paste("X", 1:5, sep = ""))
mat1

mat1["jim", "X3"]

mat1
dimnames(mat1) = c()
mat1

mat1
mat2
mat1 + mat2

A
B = matrix(c(1, 3, 4, 5, 2, 7, 8, 9, 6), ncol = 3);B
A %*% B

mat1
t(mat1)

A
t(A)

mat4
det(mat4)

B
det(B)

mat4
solve(mat4)

B
solve(B)
##########################################

x = c(1, 3, 5, 7, 9)
y = c(2, 3, 5, 7, 11, 13)
z = c(9, 3, 2, 5, 9, 2, 3, 9, 1)

x+2
y*3
length(x)
x + y
sum(x)
sum(x>5)
sum(x[x > 5])
sum(x > 5 | x < 3)
y[3]
y[-3]
y[x]
(y > 7)
y[y > 7]
sort(z)
sort(z, dec = TRUE)
rev(z)
order(z)
unique(z)
duplicated(z)
table(z)
rep(z, 3)


x
length(y)

x0 <- c(51, 64, 71, 82, 98, 107);x0
x1 = rep(c(5, 6, 3), 4);x1
x2 = rep(1:10, 1:10);x2
xN <- c("John", "Lilly", "Stef", "Bob", "Anna", "Marik", "Boris");xN

x = 1:10;x
y = rep(0, 3);y
z = c(x[1:2], y, x[3:length(x)]);z

abs <- seq(2,8,0.1);abs ## Equivalent de seq(2,8,by=0.1)
f = function(a){
  exp(a)*sin(a)
}
f(abs)

x = c (4.12, 1.84, 4.28, 4.23, 1.74, 2.06, 3.37, 3.83, 5.15, 3.76, 3.23, 4.87, 5.96, 2.29, 4.58)
x1 = x[-(1:4)];x1
x2 = x[-c(1, 15)];x2
x3 = x[(x > 2.57) & (x < 3.48)];x3
x4 = x[(x > 4.07) | (x < 1.48)];x4
x
which.min(x)
x[which.min(x)]
###########
n <- 10
x = 1:n
y = 1:n
a = y[-1] - x[-length(x)];a
b = cos(y[-length(y)]) / sin(x[-1]);b

y0 = seq(0, 10, 2)
y0
y1 = seq(2, 18, 2)
y1
y2 = rep(4, 20)
y2

y3bad = seq(0, 10,1/2)
y3bad
length(y3bad)
y3 = seq(0, 10, length.out = 20) ## On veut obtenir 20 valeurs comprises entre [0,10]
y3
length(y3)

y3
y3[3]

y3
y3[-3]

y3
matrix(y3, nrow = 2)
y3
matrix(y3, byrow = TRUE)

A = matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
A

B = matrix(1:12, nrow = 4, ncol = 3, byrow = FALSE)
B

A
A[2, 3]

A
A[ ,1] 
A[2, ]

A
C = A[c(1, 4), ]
C

matrix(1, 9, 9)
diag(9)
M = matrix(1, 9, 9) - diag(9)
M

x = 1:6
x
y = 5:10
y
z = x + y
z
z[z > 11] = 1
z

x
y
x %*% y

M = matrix(1:36, nrow = 6)
M
x
M %*% x

M
x
x %*% M

M
t(M)
M %*% t(M)


v <- c(95, 189, 68, 169, 85, 179, 72, 167, 55, 171, 86, 178, 115, 179)
length(v)
M = matrix(v, ncol = 7)
M
dimnames(M) = list(c("Poids", "Taille"), c("John", "Lilly", "Stef", "Bob",
                                           "Anna", "Marik", "Boris"))
M

v <- c(-2, 1, -2, -1, 1, 2, 1, -3, -3, 1, 1, 1, -2, -1, -1, 1)
A = matrix(v, ncol = 4)
A
det(A)

solve(A)

v <- c(2, 2, -2, -1, -1, -2, 1, -3, 3, 4, 3, 1, -4, -5, -1, -1)
B = matrix(v, ncol = 4)
B
det(B)

solve(B)

A
t(A)
det(t(A)) 
det(A)

solve(A)
t(A)
det(solve(A)) 
1 / det(t(A))

A
B
det(A %*% B) 
det(A) * det(B)

A
solve(A)
t(A)
t(solve(A))
solve(t(A))

A
t(A)
B
t(B)
A %*% B
t(A %*% B) 
t(B) %*% t(A)

A
B
A %*% B
solve(A)
solve(B) 
solve(A %*% B) 
solve(B) %*% solve(A)

v <- c(1, 1, 3,5, 2, 6, -2, -1, -3)
A = matrix(v,ncol = 3,byrow = TRUE)
A
A %*% A
A %*% A %*% A

A[1,]
A[2,]
A[3,]
A[3, ] = A[1, ] + A[2, ]
A[3,]
A

X = matrix(c(0.5^2, 0.5, 1, 1^2, 1, 1, 1.5^2, 1.5, 1), ncol = 3, byrow = TRUE)
X

X
det(X)

X
solve(X)

solve(X) %*% c(7, 4, 5)


A = matrix(0, nrow = 5, ncol = 5)
A
B = abs(col(A) - row(A)) + 1
B

B
det(B)
solve(B)


B
solve(B)
solve(B) %*% c(1, 2, 2, 3, 2)



