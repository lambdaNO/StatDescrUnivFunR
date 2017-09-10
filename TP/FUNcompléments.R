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

setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")


nb = c(18, 16, 8, 10, 6, 4, 4, 9, 11, 10, 12, 12)
bar = barplot(nb / 120, col = "white")
points(bar, rep(1 / 12, 12), type = "h",col="red")


nb = c(60, 105, 65, 47, 15, 4, 3, 1, 0)
bar = barplot(nb / 300, col = "white")
lambda = sum((0:8) * nb) / 300;lambda
prob = c(dpois(0:7, lambda), 1 - ppois(7, lambda));prob
points(bar, prob, type = "h",col="red")

x = c(43, 48, 65, 55, 51, 51, 44, 51, 59, 62, 45, 53, 55, 55, 49, 34, 52,
      69, 45, 54, 59, 36, 36, 29, 52, 59, 41, 58, 54, 55, 72, 53, 52, 49, 57, 42,
      70, 58, 42, 53, 57, 68, 40, 65, 54, 49, 32, 56, 50, 59)
hist(x, freq = FALSE, main = "Méthode de l’histogramme", ylab = "")
curve(dnorm(x, 51.94, 9.704638), add = TRUE,col="red")

plot(ecdf(x), main = "Méthode de la fonction de répartition")
curve(pnorm(x, 51.94, 9.704638), add = TRUE, col="red")

plot(density(x), main = "Méthode de l’approximation de la densité",
     ylab = "")
curve(dnorm(x, 51.94, 9.704638), lwd = 1.5, col = "red", add = TRUE)

qqnorm(x, main = "Méthode du QQ plot avec droite de Henry")
a = mean(x) ; b = sd(x);a;b
curve(a + b * x, -6, 6, col = "red", add = TRUE)

plot(qnorm(ppoints(x)), sort(x))

qqnorm(scale(x), main = "Méthode du QQ plot")
abline(0, 1, col = "red")

boxplot(x, main = "Méthode de la boîte à moustaches")

x = c(0.96, 1.45, 0.42, 3.69, 2.58, 1.95, 1.74, 0.01, 1.02, 1.12, 0.17,
      3.19, 0.85, 1.27, 0.68, 3.60, 1.23, 0.34, 0.31, 0.16, 0.07, 0.79, 0.02,
      1.20, 0.05, 2.09, 0.24, 5.46, 2.57, 0.89, 0.74, 1.67, 0.88, 2.27, 0.22,
      3.39, 0.12, 0.06, 0.78, 0.32, 5.79, 2.09, 0.39, 1.82, 2.96, 0.20, 0.08,
      0.37, 2.58, 0.30)
1 / mean(x)

hist(x, freq = FALSE, main = "Méthode de l’histogramme", ylab = "")
curve(dexp(x, 0.7446016), add = TRUE,col="red")

plot(ecdf(x), main = "Méthode de la fonction de répartition")
curve(pexp(x, 0.7446016), add = TRUE,col="red",lwd=1.5)

plot(density(x), main = "Méthode de l’approximation de la densité",
     xlim = c(0, 7), ylim = c(0, 0.5), ylab = "")
curve(dexp(x, 0.7446016), lwd = 1.5, col = "red", add = TRUE)

plot(qexp(ppoints(x), 0.7446016), sort(x), main = "Méthode du QQ plot",
     xlab = "")
abline(0,x, col = "red")

nb = c(84, 79, 75, 49, 36, 47)
proba = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)
chisq.test(nb, p = proba)$p.value

nb = c(18, 16, 8, 10, 6, 4, 4, 9, 11, 10, 12, 12)
proba = rep(1 / 12, 12)
chisq.test(nb, p = proba)$p.value

nb = c(60, 105, 65, 47, 15, 4, 3, 1)
lambda = sum((0:7) * nb) / 300
lambda

proba = c(dpois(0:6, lambda), 1 - ppois(6, lambda))
proba

chisq.test(nb, p = proba)$p.value

proba
300 * proba

nb2 = c(60, 105, 65, 47, 15, 8)
proba2 = c(dpois(0:4, lambda), 1 - ppois(4, lambda))
proba2
300 * proba2

x2obs = chisq.test(nb2, p = proba2)$statistic
deg = 4
1 - pchisq(x2obs, deg)

mean(673,389,1832,570,522,2694,3683,644,1531,2916,1069,3145,2268,3574,791,1418,649,3344,1153,3922)

x = c(673, 389, 1832, 570, 522, 2694, 3683, 644, 1531, 2916, 1069, 3145,
      2268, 3574, 791, 1418, 649, 3344, 1153, 3922)
ks.test(x, "pexp", 1 / 1850)$p.value

x = c(43, 48, 65, 55, 51, 51, 44, 51, 59, 62, 45, 53, 55, 55, 49, 34, 52,
      69, 45, 54, 59, 36, 36, 29, 52, 59, 41, 58, 54, 55, 72, 53, 52, 49, 57, 42,
      70, 58, 42, 53, 57, 68, 40, 65, 54, 49, 32, 56, 50, 59)
shapiro.test(x)$p.value

nb = c(18, 23, 19, 12, 11, 15)
bar = barplot(nb / 100, col = "white")
points(bar, rep(1 / 6, 6), type = "h",col="red")

nb
proba = rep(1 / 6, 6)
chisq.test(nb, p = proba)$p.value

nb = c(60, 62, 67, 68, 64, 56, 62, 44, 58, 67)
proba = rep(1 / 10, 10)
chisq.test(nb, p = proba)$p.value

x = rnorm(100)
a = numeric()
for (i in 1:6) {
  a[i] = ks.test(x, "pnorm", 0, 1 + (i - 1) / 10)$p.value
}
a

nb = c(773, 231, 238, 59)
bar = barplot(nb / 1301, col = "white")
points(bar, c(9 / 16, 3 / 16, 3 / 16, 1 / 16), type = "h",col="red")

nb
proba = c(9 / 16, 3 / 16, 3 / 16, 1 / 16)
chisq.test(nb, p = proba)$p.value


n = 1000
a = rpois(n, 5)
b = rpois(n, 3)
c = rpois(n, 8)
qqplot(a + b, c,col)

nb = c(15, 18, 11, 8)
bar = barplot(nb / 52, col = "white")
lambda = sum((0:4) * nb) / 52;lambda
prob = c(dpois(0:2, lambda), 1 - ppois(2, lambda))
points(bar, prob, type = "h",col="red")

nb
prob
chisq.test(nb, p = prob)$p.value

nb = c(65, 110, 70, 48, 16, 5, 4, 2)
lambda = sum((0:7) * nb) / 320
lambda

proba = c(dpois(0:6, lambda), 1 - ppois(6, lambda))
proba

nb
proba
chisq.test(nb, p = proba)$p.value


proba
sum(nb)
320*prob

nb2 = c(65, 110, 70, 48, 16, 11)
proba2 = c(dpois(0:4, lambda), 1 - ppois(4, lambda))
320 * proba2

x2obs = chisq.test(nb2, p = proba2)$statistic
deg = 4
1 - pchisq(x2obs, deg)

x = c(145, 110, 170, 48, 116, 95, 74 )
ks.test(x, "pexp", 0.01)$p.value

x = c(25.12, 12.36, 24.35, 12.19, 5.27, 18.35, 19.11, 27.08, 21.09, 17.19,
      8.45, 13.27, 15.17)
1 / mean(x)

ks.test(x, "pexp", 0.05936073)$p.value

xinf = c(0, 4, 8, 12, 16, 20, 24)
xsup = c(4, 8, 12, 16, 20, 24, 28)
centre = (xinf + xsup) / 2
centre
n = c(2, 5, 12, 14, 11, 5, 1)
x = rep(centre, n)
x

hist(x, freq = FALSE, breaks = c(0, 4, 8, 12, 16, 20, 24, 28),
     main = "Méthode de l’histogramme", ylim = c(0, 0.082), ylab = "")
a = mean(x) ; b = sd(x)
curve(dnorm(x, a, b), add = TRUE, col="red")

x = c(247.0, 247.8, 250.2, 251.3, 251.9, 249.4, 248.8, 247.1, 255.0, 247.0,
      254.8, 244.8, 250.7, 250.7, 252.6, 251.1, 254.1, 249.2, 252.0, 254.0)
qqnorm(x, main = "Méthode du QQ plot avec droite de Henry")
a = mean(x) ; b = sd(x)
curve(a + b * x, -6, 6, col = "red", add = TRUE)

x
shapiro.test(x)$p.value

x = c(65.06, 71.44, 67.93, 69.02, 67.28, 62.34, 66.23, 64.16, 68.56, 70.45,
      64.91, 69.90, 65.52, 66.75, 68.54, 67.90)
qqnorm(x, main = "Méthode du QQ plot avec droite de Henry")
a = mean(x) ; b = sd(x)
a;b
curve(a + b * x, -6, 6, col = "red", add = TRUE)
x
shapiro.test(x)$p.value

getwd()
w = read.table("pression.txt", header = T)
str(w)
attach(w)

shapiro.test(Y)$p.value
shapiro.test(X1)$p.value

par(mfrow = c(1, 2))
qqnorm(scale(X1))
abline(0, 1, col = "red")
qqnorm(scale(Y))
abline(0, 1, col = "blue")
par(mfrow = c(1, 1))

a = rexp(1000, 3.8)
b = rexp(1000, 3.8)
c = rgamma(1000, 2, 3.8)
qqplot(a + b, c)

a = rgamma(1000, 4.2, 2.1)
b = rgamma(1000, 4.2, 2.1)
c = rgamma(1000, 8.4, 2.1)
qqplot(a + b, c)

a = rnorm(1000, 1.6, 1.5)
b = rnorm(1000, 1.6, 1.5)
c = rnorm(1000, 3.2, sqrt(1.5^2 + 1.5^2))
qqplot(a + b, c)

a = rchisq(1000, 3.2)
b = rchisq(1000, 3.2)
c = rchisq(1000, 6.4)
qqplot(a + b, c)

a = rnorm(1000)
b = rnorm(1000)
c = rchisq(1000, 2)
qqplot(a^2 + b^2, c)

a = rnorm(1000)
b = rchisq(1000, 3.9)
c = rt(1000, 3.9)
qqplot(a / sqrt(b / 3.9), c)

a = rchisq(1000, 2.1)
b = rchisq(1000, 8.3)
c = rf(1000, 2.1, 8.3)
qqplot((a / 2.1) / (b / 8.3), c)


moy <- 10
std <- 3
plot(function(x) dnorm(x,moy,std),  moy-4*std, moy+4*std,xlab="x",ylab="",main = "",col="blue")


x <- rnorm(1000,0,0.5)
y <- rnorm(1000,3,1)
z <- c(x,y)
r <- c(-10,10)
hist(z,freq=FALSE,col="grey",ylim=c(0,0.5),20)
plot(function(x) 0.5*dnorm(x,0,0.5),xlim=r,col=2,add=TRUE,lwd=2)
plot(function(x) 0.5*dnorm(x,3,1),xlim=r,col=3,add=TRUE,lwd="2")

simu <- rnorm(1000)
{hist(simu, prob=T, breaks="FD", 
      main="Histogramme de 1000 tirages N(0,1)",col=rainbow(12), lwd=2)}
curve(dnorm(x), add=T)

###################################################################
###################################################################
###################################################################
###################################################################

setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")
dbinom(4, 8, 0.3)
choose(8, 4) * 0.3^4 *(1- 0.3)^(8 - 4)
dnorm(1.7, 2, 0.12)
(1 / sqrt(2 * pi * 0.12^2)) * exp(- (1.7 - 2)^2 / (2 * 0.12^2))
dbinom(c(4, 6), 8, 0.3)
dexp(2, c(1, 2, 3))
vec = dexp(2, c(1, 2, 3))
vec
plot(0:5, dbinom(0:5, 5, 0.2), type = "h", ylab = "P(X = x)")
curve(dnorm(x, 5, 1.5), 0.5, 9.5, ylab = "fX(x)")
pbinom(4, 8, 0.3)
sum(dbinom(0:4, 8, 0.3))
pnorm(12, 9, 2)
pexp(2, 3, lower.tail = FALSE)
exp(-6)
plot(stepfun(0:15, c(0, pbinom(0:15, 15, 0.6))), ylab = "FX(x)", main = "")
curve(pnorm(x, 5, 1.5), 0.5, 9.5, ylab = "FX(x)")
qbinom(0.25, 5, 0.6)
dbinom(c(1,2,3,4,5),5,0.6)
pbinom(0:5, 5, 0.6)
qnorm(0.975, 0, 1)
pnorm(1.96, 0, 1)
rpois(10, 2)
sum(rbinom(80, 1, 0.02))
x = rnorm(15, 22, 2)
x
x = 1:7
sample(x)
y = sample(c("rouge", "vert", "bleu", "blanc", "noir"))
y
sample(1:3, size = 2, replace = TRUE, prob = c(25 / 100, 20 / 100,55 / 100))
sample(1:10, size = 3)
sample(1:5, size = 9, replace = TRUE)
y = c("rouge", "vert", "bleu", "blanc", "noir")
sample(y, 2, prob = c(10 / 100, 30 / 100, 10 / 100, 30 / 100, 20 / 100))
###

plot(0:8, dpois(0:8, 1), type = "h", xlab = "x", ylab = "P(X = x)")
curve(dchisq(x, 3), 0, 10, xlab = "x")
## On split la fenêtre en deux
par(mfrow = c(2,1))
# On veut les densité donc : dloi
## Var Bino -> B(50,0.08) 
plot(0:50, dbinom(0:50, 50, 0.08), type = "h", xlab = "x", ylab = "P(X = x)",
     ylim=c(0, 0.25), main = "Densité associée à la loi B(50, 0.08)",col="red")
## Var Poiss -> P(0,4)
plot(0:50, dpois(0:50, 4), type = "h", xlab = "x", ylab = "P(X = x)",
     ylim = c(0, 0.25), main = "Densité associée à la loi P(0.08)",col="blue")
## On oubli pas de mettre les options graphique par défaut
par(mfrow = c(1,1))

par(mfrow = c(3, 1))
curve(dnorm(x, 4, 1), 1, 8, ylab = "",
      main = "Densités associées aux lois N(4,1) et N(5,1)")
curve(dnorm(x, 5, 1), 1, 8, col = "red", ylab = "", add = TRUE)
curve(dnorm(x, 4, 1), -2, 10, ylab = "",
      main = "Densités associées aux lois N(4,1) et N(4,4)")
curve(dnorm(x, 4, 2), -2, 10, col = "blue", ylab = "", add = TRUE)
curve(dnorm(x, 4, 1), -1, 11, ylab = "",
      main = "Densités associées aux lois N(4,1) et N(5,4)")
curve(dnorm(x, 5, 2),-1, 11, col = "green", ylab = "", add = TRUE)
par(mfrow = c(1,1))

plot(0:50, dbinom(0:50, 50, 0.4), type = "h", xlab = "x", ylab = "",
     main = "Densités associées aux lois B(50,0.4) et N(20,12)")
curve(dnorm(x, 20, sqrt(12)), 0, 50, col = "red", ylab = "", add = TRUE)

plot(0:20, dbinom(0:20, 100, 0.01), type = "h", xlab = "x", ylab = "",
     ylim = c(0, 0.4), main = "Densités associées aux lois B(100,0.01) et N(1,0.99)")
curve(dnorm(x, 1, sqrt(0.99)), 0, 20, col = 3, ylab = "", add = TRUE)
pnorm(-0.5, 0, 1)
1 - pnorm(1.5, 0, 1)
1 - pnorm(-1, 0, 1)
pnorm(1.96, 0, 1) - pnorm(-1.96, 0, 1)
pnorm(2.58, 0, 1) - pnorm(-2.58, 0, 1)
1 - (pnorm(3, 0, 1) - pnorm(-3, 0, 1))
pnorm(1.5, 0, 1, lower.tail = FALSE)
pnorm(20, 15, 3) - pnorm(16, 15, 3)
1 - pnorm(18, 15, 3)
pnorm(6, 15, 3)
1 - (pnorm(15 + 5.88, 15, 3) - pnorm(15 - 5.88, 15, 3))

plot(stepfun(0:50, c(0, pbinom(0:50, 50, 0.4))),
     main = "Fonctions de répartitions associées aux lois B(50, 0.4) et N(20, 12)",col="blue",lwd=1.5)
curve(pnorm(x, 20, sqrt(12)), 0, 50, col = "red", add = TRUE)


p = c(0.00135, 0.025, 0.95, 0.999, 0.995, 0.99865)## On définit les valeurs des quantiles 
x = qnorm(p) ## On cherche les quantiles pour les valeurs que l'on a définit
cbind(p, x) ## On améliore l'affichage

p = c(0.975, 0.025)
y = qnorm(p, 19, sqrt(3))
cbind(p, y)

y[1]
qnorm(0.975)
y[1] - 19 - sqrt(3) * qnorm(0.975)


simul1 = rnorm(1000, 15, sqrt(3))
head(simul1,5);tail(simul1,5)
hist(simul1, col = 3, prob = TRUE, ylim = c(0, 0.25),
     main = "Histogramme de rnorm(1000,15, sqrt(3)) et
     \n densité associée à la loi N(15, 3)")
curve(dnorm(x, 15, sqrt(3)), col = "black", add = TRUE)
boxplot(simul1, main = "Boxplot de rnorm(1000, 15, sqrt(3))")

simul2 = rpois(1000, 1)
head(simul2,10);tail(simul2,10)
table(simul2)

compter = function(a, b) {
  d = numeric()
  for(i in 1:(length(a))) {
    d[i] = sum(b == a[i])
  }
  names(d) = as.character(a)
  print(d)
}

maxi = max(simul2);maxi
nbsimul2 = compter(0:maxi, simul2)

bppois = barplot(nbsimul2)
points(bppois, dpois(0:maxi,1) * 1000, type = "h",
       col = "blue", lwd = 2)
axis(4, at = seq(0, 1000, by = 100), labels = seq(0, 1, by = 0.1))

peage = function(mu1, mu2, sigma, n) {
  attente = numeric(n)
  cabines = c("C1", "C2")
  for(i in 1 : n) {
    tirage = sample(cabines, 1)
    if(tirage == "C1") {
      attente[i] = rnorm(1, mu1, sigma)
    } else {
      attente[i] = rnorm(1, mu2, sigma)
    }
  }
  a = 0.9 * max(hist(attente, prob = TRUE, ylim = c(0, 0.22), xlab = "attente",
                     main = "Histogramme de attente")$density)
  moy = mean(attente)
  ecart = sd(attente)
  curve(dnorm(x, moy, ecart), add = TRUE)
  text(moy, a, paste("moy =", round(moy, 2), "écart =", round(ecart, 2)))
  curve(0.5 * (dnorm(x, mu1, sigma) + dnorm(x, mu2, sigma)), col = "red", add = TRUE)
}
par(mfrow = c(2, 1))
peage(50, 51, 2, 1000)
peage(50, 60, 2, 1000)
par(mfrow = c(1, 1))

stu = function(n,nb,mu,sigma) {
  vec = numeric(nb)
  for (i in 1:nb) {
    simul = rnorm(n,mu,sigma)
    xbar = mean(simul)
    sech = sd(simul)
    vec[i] = sqrt(n) * (xbar - mu) / sech
  }
  par(mfrow = c(2, 1))
  hist(vec, prob = TRUE,
       breaks = seq(-1000.25, 1000.25, by = 0.5), xlim = c(-5, 5), ylim = c(0, 0.5),
       main = "Histogramme de vec")
  curve(dnorm(x), -5, 5, add = TRUE)
  curve(dt(x, n - 1), -5, 5, col = "red", add = TRUE)
  legend("topleft",
         legend = c("N(0,1)", paste("T(", n - 1, ")", sep = "")), col = c("black", "red"),
         lwd = 1)
  plot(ecdf(vec), xlim = c(-5, 5))
  curve(pnorm(x), -5, 5, col = "green", lwd = 2, add = TRUE)
  curve(pt(x, n-1), -5, 5, col = "red", lwd = 2, add = TRUE)
  legend("topleft", legend = c("F_N(0,1)", paste("F_T(", n - 1, ")", sep = "")),
         col = c("green", "red"), lwd = 1)
  par(mfrow = c(1,1))
}
stu(3, 1000, 10, 2)

par(mfrow = c(2, 2))
curve(dnorm(x), -3, 3)
curve(dt(x, 2), -3, 3, col = "red", add = TRUE)
legend("topleft", legend = c("N(0,1)", "T(2)"), col = c("black", "red"), lwd=1)
curve(dnorm(x), -3, 3)
curve(dt(x,3), -3, 3, col = "red", add = TRUE)
legend("topleft", legend = c("N(0,1)", "T(3)"), col = c("black", "red"), lwd = 1)
curve(dnorm(x), -3, 3)
curve(dt(x, 10), -3, 3, col = "red", add = TRUE)
legend("topleft", legend = c("N(0,1)", "T(10)"), col = c("black", "red"), lwd = 1)
curve(dnorm(x), -3, 3)
curve(dt(x, 30), -3, 3, col = "red", add = TRUE)
legend("topleft", legend = c("N(0,1)", "T(30)"), col = c("black", "red"), lwd = 1)
par(mfrow = c(1, 1))
#########

stu = function(n,nb,mu,sigma) {
  vec = numeric(nb)
  for (i in 1:nb) {
    simul = rnorm(n,mu,sigma)
    xbar = mean(simul)
    sech = sd(simul)
    vec[i] = sqrt(n) * (xbar - mu) / sech
  }
  par(mfrow = c(2, 1))
  hist(vec, prob = TRUE,
       breaks = seq(-1000.25, 1000.25, by = 0.5), xlim = c(-5, 5), ylim = c(0, 0.5),
       main = "Histogramme de vec")
  curve(dnorm(x), -5, 5, add = TRUE)
  curve(dt(x, n - 1), -5, 5, col = "red", add = TRUE)
  legend("topleft",
         legend = c("N(0,1)", paste("T(", n - 1, ")", sep = "")), col = c("black", "red"),
         lwd = 1)
  plot(ecdf(vec), xlim = c(-5, 5))
  curve(pnorm(x), -5, 5, col = "green", lwd = 2, add = TRUE)
  curve(pt(x, n-1), -5, 5, col = "red", lwd = 2, add = TRUE)
  legend("topleft", legend = c("F_N(0,1)", paste("F_T(", n - 1, ")", sep = "")),
         col = c("green", "red"), lwd = 1)
  par(mfrow = c(1,1))
}
stu(3, 1000, 10, 2)
stu(30, 1000, 10, 2)

par(mfrow = c(2, 3))
curve(dchisq(x, 1), 0.01, 10, main = "Chisq(1)")
curve(dchisq(x, 2), 0.01, 10, main = "Chisq(2)")
curve(dchisq(x, 3), 0.01, 10, main = "Chisq(3)")
curve(dchisq(x, 5), 0.01, 10, main = "Chisq(5)")
curve(dchisq(x, 10), 0.01, 20, main = "Chisq(10)")
curve(dchisq(x, 20), 0.01, 60, main = "Chisq(20)")
par(mfrow = c(1, 1))

khi2 = function(n, nb, mu, sigma) {
  vec = numeric(nb)
  for (i in 1:nb) {
    simul = rnorm(n, mu, sigma)
    sech2 = var(simul)
    vec[i] = (n-1) * sech2 / sigma^2
  }
  vecnew = vec[vec >= 0.01 & vec <= 10]
  hist(vecnew, prob = TRUE, main = "Histogramme de vecnew")
  curve(dchisq(x, n-1), 0.01, 10, add = TRUE)
  legend("topright", legend = paste("Chisq(",n-1,")", sep = ""), col = "black",
         lwd = 1)
}
par(mfrow = c(1, 1))
khi2(4, 10000, 10, 2)

par(mfrow = c(2, 2))
curve(dexp(x, 1), 0, 3, main = "exp(1)")
curve(dexp(x, 2), 0, 3, main = "exp(2)")
curve(dexp(x, 0.5), 0,20, main = "exp(0.5)")
curve(dexp(x, 0.1), 0,60, main = "exp(0.1)")
par(mfrow=c(1, 1))

curve(dnorm(x, 0, 1), xlim = c(-10, 10), ylim = c(0, 0.58), col = "red", lwd = 3,
      ylab = "", main = "Exemples de densités associées à la loi normale")
curve(dnorm(x, 2, 3), col = "green", lwd = 3, add = TRUE)
curve(dnorm(x, 0, 5), col = "blue", lwd = 3, add = TRUE)
curve(dnorm(x, -2, 0.7), col = "yellow", lwd = 3, add = TRUE)
legend("topright", legend = c("N(0,1)", "N(2,9)", "N(0,25)", "N(-2,0.49)"),
       lty = 1, lwd = 3, col = c("red", "green", "blue", "yellow"))

curve(dexp(x, 0.5), xlim = c(0, 3), ylim = c(0, 1), col = "red", lwd = 3,
      ylab = "", main = "Exemples de densités associées à la loi exponentielle")
curve(dexp(x, 1), col = "green", lwd = 3, add = TRUE)
curve(dexp(x, 1.5), col = "blue", lwd = 3, add = TRUE)
curve(dexp(x, 2), col = "yellow", lwd = 3, add = TRUE)
legend("topright", legend = c("E(0.5)", "E(1)", "E(1.5)", "E(2)"),
       lty = 1, lwd = 3, col = c("red", "green", "blue", "yellow"))


densexp = function(x, lambda) {
  ifelse (x >= 0, lambda * exp(-lambda * x), 0)
}

integrate(function(x) dexp(x, 1 / 10), 0, 100)

par(mfrow = c(1, 2))
curve(dexp(x, 1 / 10), 0, 15, main = "Densité")
curve(pexp(x, 1 / 10), 0, 15, main = "Fonction de répartition")

1 - pexp(10.2, 1 / 10)

n = 0
x = rep(0, 5)
while(sum(x != 6) != 0) {
  x[x != 6] = sample(1:6, sum(x != 6), rep = T)
  print(x)
  n = n + 1
}
print(n)

n = 100
u = sample(c(-1,1), n, replace = T)
x = cumsum(u)
x = c(0, x)
head(x,50)
par(mfrow = c(1, 1))
plot(x)

densgamma = function(x, m, lambda) {
  ifelse (x >= 0, (1/factorial(m - 1)) * lambda^m * x^(m - 1) *
            exp(-lambda * x), 0)
}
integrate(function(x) dgamma(x, 5, 1), 0, 100)
x = 2:10
pgamma(x, 5, 1)
qgamma(0.92, 5, 1)

densnorm = function(x) {
  (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
}
integrate(function(x) dnorm(x), -100, 100)
par(mfrow = c(1, 2))
curve(dnorm(x), -5, 5, ylab = "Densité",main= "Densité",col="red")
curve(pnorm(x), -5, 5, ylab = "Fonction de répartition",main ="Fonction de répartition",col="blue")

pnorm(2.2)
1 - pnorm(1.7)
pnorm(1.7, lower.tail = FALSE)
pnorm(1.4) - pnorm(0.2)
2 * pnorm(1.96) - 1
qnorm(0.98)

pnorm(650, 550, 100)
1 - pnorm(746, 550, 100)
pnorm(746, 550, 100, lower.tail = FALSE)
pnorm(600, 550, 100) - pnorm(550, 550, 100)

foncrep = function(x) {
  n = 1 + 2 * 0:50
  0.5 + (1 / sqrt(2 * pi)) * exp(-x^2 / 2) * sum(x^n / cumprod(n))
}
foncrep(1.2)
pnorm(1.2)

x = sample(c(0, 2, 5), 1000, replace = T, prob = c(0.2, 0.5, 0.3))
table(x)

Urne = function(k, p, q) {
  contenu = rep(c("Rouge", "Noire"), c(p, q))
  sample(contenu, k)
}
Urne(6,8,5)

Freq = function(n) {
  tirage = sample(1:6, n, replace = T)
  sum(tirage == 5) / n
}
Freq(10)
Freq(100)
Freq(1000)
Freq(10000)

sample(c(0, 1), 10, replace = T, prob = c(2 / 3, 1 / 3))
sample(c(rep(0, 10), rep(1, 5)), 10)

x = numeric()
for(i in 1:500){
  x[i] = sum(sample(c(0, 1), 10, replace = T, prob = c(2 / 3, 1 / 3)))
}
table(x)

y = numeric()
for(i in 1:500){
  y[i] = sum(sample(c(rep(0, 10), rep(1, 5)), 10))
}
table(y)

titre1 = "Fréquences obtenues pour 500 tirages avec remise"
titre2 = "B(10, 1/3)"
titre3 = "Fréquences obtenues pour 500 tirages sans remise"
titre4 = "H(10, 5, 10)"
par(mfrow = c(2, 2))
barplot(table(x) / 500, main = titre1)
barplot(dbinom(0:10, 10, 1/3), names.arg = 0:10, main = titre2)
barplot(table(y) / 500, main = titre3)
barplot(dhyper(0:5, 5, 10, 10), names.arg = 0:5, main = titre4)
