setwd("~/Desktop/DIVERS_TEMPLATES/R/DEVOIR")
##Chargement des données du data frame dans une variable shd
shd <- read.csv2("satisfaction_hopital.csv")
library(prettyR) #Describe
colors <- c("blue","pink")
str(shd)
##table(shd)


#Pour les trois variables catégorielles du fichier, présentez les pourcentages de sujets relevant de chacune des modalités.## Nombre d'obervations du dataframe
## Nombre d'observation
nb <-nrow(shd)
## Sexe
Frsex<-round((table(shd$sexe)/nb)*100,2);Frsex
## Service 
Frserv<-round((table(shd$service)/nb)*100,2);Frserv
## Profession
Frprof<-round((table(shd$profession)/nb)*100,2);Frprof


# Pour les autres variables, donnez de façon synthétique : moyenne, médiane, écart-type, minimum, maximum, nombre de données disponibles (non manquantes).
describe(shd$age,num.desc =c("mean","median","sd","min","max","valid.n"),xname='age' )
describe(shd$amelioration.sante, num.desc =c("mean","median","sd","min","max","valid.n"),xname='amelioration.sante')
describe(shd$amelioration.moral, num.desc =c("mean","median","sd","min","max","valid.n"),xname='amelioration.moral')
describe(shd$recommander, num.desc =c("mean","median","sd","min","max","valid.n"),xname='recommander')
describe(shd$score.relation, num.desc =c("mean","median","sd","min","max","valid.n"),xname='score.relation')
describe(shd$score.information, num.desc =c("mean","median","sd","min","max","valid.n"),xname='score.information')

# Faites un histogramme du score de relation (score.relation).
hist(shd$score.relation,xlab="Score",ylab = "Nombre d'observations",col=blues9,main="Qualité des relations Patient/Personnel Soignant")

# A l’aide de deux « boxplots », représentez côte à côte la distribution du score de relation chez les hommes et les femmes.
table(shd$sexe)
table(shd$score.relation)
boxplot(shd$score.relation~shd$sexe,col=colors,xlab="Homme (0) - Femme (1)",ylab="Score de relation", main ="Distribution du score de relation en fonction du sexe.")

remove(age,colors,Frprof,Frserv,Frsex,moral,nb,type,scorela,scorinfo,sante,recom)
