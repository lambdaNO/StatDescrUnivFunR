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

