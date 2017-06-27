setwd("~/Desktop/DIVERS_TEMPLATES/R/TP/LAB2")
smp <- read.csv2("../DONNEES/smp2.csv")

##Affichage des variables
names(smp)
##Synthèse des variables
str(smp)
##Résumé des principaux indicateurs statistique sur les variables.
summary(smp)
##Créationn d'une variable qualitative
gauss <- factor(smp$n.enfant)
levels(gauss)
##Concaténation de plusieurs mode
levels(gauss)[6:13]<-"5+"
levels(gauss)
table(gauss)


#POUR LES VARIABLES NUMERIQUES
##Accès direct à des données dans une variable
smp$age[1]
##Accès direct à des données dans un dataframe
### Accès à l'observation 1 de la variable 1 
names(smp)
smp[1,1]
###OU
smp[1,"age"]

#POUR LES VARIABLES CATEGORIELLES
table(smp$prof)
head(smp$prof)
###Restriction d'une variable à une seule modalité
table(smp$prof == "agriculteur")
###Affichage des valeurs qui remplissent la condition :
which(smp$prof =="agriculteur")
###Ce principe d'indexation associe à une valeur particulier, une position donnée.
###Ainsi, il nous est facile d'indexer les valeurs d'age pour lequelles la profession du détenu est agriculteur
smp$age[which(smp$prof =="agriculteur")]
###En fait, on prend la variable age et on lui indique la liste des numéros d'obervations qui nous intéressent
####Conclusion : Nous avons l'age des individus dont la profession est agriculteur.

##Méthode plus rapide (DATAFRAME,CONDI,VARIABLE qui nous intéresse)
subset(smp,prof=="agriculteur",age)
##Si l'on souhaite étendre la selection à plusieurs variables :
subset(smp,prof=="agriculteur",1:5)

names(smp)[1:5]
subset(smp,prof=="agriculteur", c(age, duree, discip,n.enfant))
##La profession ne nous intéresse pas puisque nous savons déjà qu'il sont agriculteurs
###
##Il est possible de rajouter des filtres sur les lignes
subset(smp,prof=="agriculteur" & n.enfant > 2, c(age, duree, discip,n.enfant))
subset(smp,prof=="agriculteur" & n.enfant > 2 & complete.cases(duree), c(age, duree, discip,n.enfant))
###
table(gauss)
tab <-table(gauss)
tab
sum(tab)
tab/sum(tab)
##R procède mode par mode [0:(214/773)=0.276...],[1:(220/773)=0.284...], ...
##Il existe une fonction equivalente à tab/sum(tab)
prop.table(tab)
##Arrondit des résultats : 
round(prop.table(tab),3)
round(prop.table(tab),2)
round(prop.table(tab),4)

freqrelative <- prop.table(tab)*100
freqrelative
barplot(freqrelative, xlab = "freqrelative")
barplot(freqrelative,ylim = c(0,30), xlab = "freqrelative, ylim = c(0,30)")
barplot(freqrelative,ylim = c(0,30),las=1,xlab = "freqrelative, ylim = c(0,30),las=1,xlab")

head(smp$age)
summary(smp$age)
##Histogramme de Frequence
hist(smp$age)
##Ajustement du nombre de modalité
hist(smp$age,nclass = 8)

lines(density(smp$age, na.rm = TRUE))
##Conversion de l'histogramme en histogoramme de densité
hist(smp$age,nclass = 8,prob=TRUE)
hist(smp$age,nclass = 8,prob=TRUE,las=1)
lines(density(smp$age, na.rm = TRUE))

##Sauvegarde du fichier de données
save(smp,file="smp_lab2.rda")
