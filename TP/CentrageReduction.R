setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")
CentrageReduction <- function(x)
{
    # Cette fonction centre et réduit les données d’un vecteur
    # nommé x. Elle effectue la même tâche que la fonction «scale».
    # Soustrait la moyenne au vecteur x
    xCentre<- x - mean( x )
    # Calcule la variance
    variance <- var( x )
    # Trouve le l’écart type
    EcartType<- variance ^ (1/2)
    # Complète le centrage et la réduction des données
    xCentreReduit <- xCentre / EcartType
    # Retourne le vecteur de données centrées et réduites
    return( xCentreReduit )
}
source("CentrageReduction.R")
CentrageReduction("Roger")
vec <- c(1, 2, 3, 4, 5) # Crée un vecteur de données
source("CentrageReduction.R")
CentrageReduction( x = vec )

if (x>0) y=x*log(x) else y=0
y=ifelse(x>0,x*log(x),0)

x = -1
if (x>0) y=x*log(x) else y=0
print(y)
y=ifelse(x>0,x*log(x),0)
print(y)

for (i in 1:10) print(i)
y=z=0;
for (i in 1:10) {
  x=runif(1)
  if (x>0.5) y=y+1
  else z=z+1 }
y;z
for (i in c(2,4,5,8)) print(i)
x = rnorm(100)
y = ifelse(x>0, 1, -1) # condition
y;i=0
while (i<10){
  print(i)
  i=i+1}

MaFonction=function(x){x+2}
ls()
MaFonction
MaFonction(3)
x = MaFonction(4);x

Fonction2=function(a,b=7){a+b}
Fonction2(2,b=3)
Fonction2(5)

Calcule=function(r){
  p=2*pi*r;s=pi*r*r;
  list(rayon=r,perimetre=p,
       surface=s)}
resultat=Calcule(3)
resultat$ray
2*pi*resultat$r==resultat$perim
resultat$surface


rectangle=function(l1,l2){
  p=(l1+l2)*2
  s=l1*l2
  list(largeur=min(l1,l2),longueur=max(l1,l2),
       perimetre=p,surface=s)}

rectangle(12,5)
rectangle(12,5)$largeur
rectangle(12,5)$longueur
rectangle(12,5)$perimetre
rectangle(12,5)$surface

fibo=function(n){
  res=rep(0,n);res[1]=0;res[2]=1
  for (i in 3:n) res[i]=res[i-1]+res[i-2]
  res}
# Calcul du rapport de 2 termes consécutifs
res=fibo(20)
res
ratio=res[2:20]/res[1:19]
ratio
plot(1:19,ratio,type="b")

ligne.NA=function(vec){any(is.na(vec))}
filtre.NA=function(mat){
  tmp = apply(mat,1,ligne.NA)
  mat[!tmp,]}
# Application sur une matrice de test
matrice.test = matrix(1:40,nc=5)
matrice.test
matrice.test[2,5]=NA;matrice.test[4,2]=NA
matrice.test[7,1]=NA;matrice.test[7,5]=NA
filtre.NA(matrice.test)


data(iris)
summary(iris)
apply(iris[,1:4],2,sum)
lapply(iris[,1:4],sum)
sapply(iris[,1:4],sum)
tapply(iris[,1],iris[,5],sum)


n=10
Y=rnorm(n,80,5) # génération
Y
mean(Y) # moyenne
sd(Y) # écart-type
sd(Y)/sqrt(length(Y)) # écart-type de la moyenne
summary(Y) # quartiles et moyenne
boxplot(Y) # diagramme boîte
# histogramme de la densité
hist(Y, probability=T, col="blue")
# estimation par la méthode du noyau
lines(density(Y), col="red", lwd=2)
# tracer la loi théorique
x=1:100
curve(dnorm(x,mean=80,sd=5),add=TRUE,
      col="green",lwd=2)

n=100
# matrice de nombres aléatoires de
# 10 colonnes et n lignes
Y=matrix(rnorm(n*10,80,5),n,10)
head(Y,10)
# moyenne de chaque colonne
apply(Y,2,mean)
mean(apply(Y,2,mean)) # moyenne des moyennes
# écart-type de chaque colonne
apply(Y,2,sd)
mean(apply(Y,2,sd)) # moyenne des écarts-types

n=1000
N=12
X=rep(0,n)
# n itérations
for (i in 1 : n) X[i]=sum(runif(N))
# histogramme
hist(X, col="blue", probability=T)
# estimation par méthode su noyau
lines(density(X), col="red", lwd=2)
x=X
sigma2=N/12
curve(dnorm(x,mean=N/2,sd=sqrt(sigma2)),
      add=T, col="green", lwd=2)


