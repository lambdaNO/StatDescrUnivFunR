##Import du fichier de données smp2.csv

smp <- read.csv2("/comptes/E131729J/XX_Université_fun/univfunR/TP/smp2.csv")
##Définition de l'espace de travail par défaut

setwd("~/XX_Université_fun/univfunR/TP")
##Affichage des données : 
View(smp)

#################################################### AJOUTER LE INCLUDE


##Listing du nom des variables présentes dans le fichier : 
names(smp)

##Affichage des données V2
str(smp)
### int -> Variable quantitative
### factor -> Variable qualitative (level)

## Résumé numérique des données : 
summary(smp)
###La commande summuray fonctionne également pour des variables seules (pas seulement pour les data frames)
###Pour isoler une variable dans un data frame, xxx$yyy (x: nom du data frame, y : nom de la variable)
summary(smp$age)
## On peut toujours écrire smp$age mais dans ce cas, R va nous renvoyer l'ensemble des observations
smp$age
## On peut cependant afficher une observation précise grâce au crochets.
smp$age[1]

## On peut également demander à R d'afficher une suite de données (par exemple les dix premières)
smp$age[1:10]

## Calcul d'une min pour la valeur de l'aĝe (Il faut préciser le second paramètre sinon il n'enlève pas les valeurs manquante et nous renvoi NA)
min(smp$age, na.rm =TRUE)
max(smp$age, na.rm = TRUE)
mean(smp$age, na.rm = TRUE)

## Dans le cadre des variable binaire : 
smp$abus[1:10]
### OU
head(smp$abus, n=10)
## Renvoyer les modalité de cette variable binaire : 
unique(smp$abus)
## Nombre total d'observation : 
length(smp$abus)
## Nombre de ligne du tableau smp
nrow(smp)
##Tableau d'effectif associé à chaque modalités (On remarque que la somme des effectifs n'est pas égales à 799, cela provient du fait que les variables NA ne sont pas affichées)
table(smp$abus)
## Pour les affichers : 
table(smp$abus, useNA = "always")
 summary(smp$abus)

##On remarque que la variable abus est traités comme une variable numérique. Si l'on souhaite la traiter comme une variable qualitative : 
head(smp$abus)
##Renvoie les modes possibles.
head(factor(smp$abus))

##NOus allons créer une nouvelle variable pour traiter ABUS comme un variable qualitative

abus <- factor(smp$abus)
table(abus, useNA="always")
abus <- factor(smp$abus, levels =c(0,1), labels=c("Non","Oui"))
##Afficher les données de Abus sans les valeurs non renseignées
table(abus)
#Affichier les valeurs de Abus avec les valeurs non renseignées
table(abus, useNA="always")

names(smp)
## On choisit d'étudier le nombre d'enfant
head(smp$n.enfant)
##Renvoie les modes possibles
summary(smp$n.enfant)

gauss <- smp$n.enfant
head(gauss)
summary(gauss)

table(gauss)


##On souhaite retourner le nombre d'enfant dont l'âge est supérieur à 4 ans.
table(gauss > 4)
## Autre test 
table(gauss <=4)

##Définition de l'age des enfants dans une variable sous forme de facteurs (et non plus de nombres)
smp$n.enfant.cat <-factor(smp$n.enfant)
table(smp$n.enfant.cat)
##On souhaite retourner le nombre de niveau/mode de cette variable
levels(smp$n.enfant.cat)
nlevels(smp$n.enfant.cat)
##On souhaite a présent rassembler (aggreger) les derniers niveau
##On considère que les niveau 6 à 13 représentent une modalité unique - On redéfinit donc un nouveau mode :
levels(smp$n.enfant.cat)[6:13]<-"+5"
table(smp$n.enfant.cat)


##Nous allons maintenant sauvegarder notre fichier smp au format R
save(smp, file="smp_v1.rda")
##SAUVEGARDE DE L'HISTORIQUE DE COMMANDEX
savehistory("commande.R")
