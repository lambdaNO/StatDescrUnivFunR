sh <- read.csv2("satisfaction_hopital.csv")

#####Question 1 : Pour les trois variables catégorielles du fichier, présentez les pourcentages de sujets relevant de chacune des modalités.

# Pour les 3 variables catégorielles (services, sexe et profession), il est demandé le pourcentage de sujets (et non l'effectif) pour chaque modalité, il suffit pour cela d'ajouter *100/534 ( car N = 534) à la commande pour obtenir le % et non l'effectif à partir de la fonction table().

##Pour la variable qualitative service
table(sh$service)*100/534
# La commande useNa="always" est ici inutile car il n'y a pas de données manquantes pour cette variable
# 12,17228 % des sujets de l'échantillon ont été reçus dans le service codé 1, 11.04869 % dans le service codé 2, etc. (cf. la sortie)

##Pour la variable qualitative sexe
sh$sexe<-factor(sh$sexe,labels=c('Homme','Femme'))
#Renommer la variable sexe avec les modalités Homme pour 0 et Femme pour 1
table(sh$sexe)*100/534
# La commande useNa="always" est ici inutile car il n'y a pas de données manquantes pour cette variable
# 50,18727% des sujets de l'échantillon sont des hommes et 49.81273 % des femmes.

##Pour la varibale qualitative profession
table(sh$profession,useNA="always")*100/534
# La commande useNa="always" est ici indispensable car il y a des données manquantes pour cette variable
# 0.1872659 % des sujets de cet échantillon font partis de la catégorie socio-prof "agriculteurs/exploitants" (modalité 1), 7.1161049 % de la catégorie "artisans, commerçants, chef d'entreprise" (modalité 2), etc.

####Question 2 : Pour les autres variables, donnez de façon synthétique : moyenne, médiane, écart-type, minimum, maximum, nombre de données disponibles (non manquantes).

names(sh) #Permet de repérer le numéro de colonne des 6 autres variables
library(prettyR)
describe(sh[,c(3,5:9)],num.desc=c("mean","median","sd","min","max","valid.n"))
#[,c(3,5:9)] permet de ne selectionner que les variables désirées par leur numéro de colonne

####Question 3 : Faites un histogramme du score de relation (score.relation)

hist(sh$score.relation,col="pink",main="Histogramme des scores de relations",xlab="score de relations")

####Question 4 : A l’aide de deux « boxplots », représentez côte à côte la distribution du score de relation chez les hommes et les femmes.

boxplot(sh$score.relation~sh$sexe,ylab="score de relation",xlab="Sexe")

####################################################################################################################
####################################################################################################################
##########################################CORRECTION DE FUN ########################################################
####################################################################################################################
##Concernant la question "Pour les trois variables catégorielles du fichier, présentez les pourcentages de sujets..."
##Exemple de correction:
  # voici la liste des fonctions R utilisées
  # la fonction read.csv2() permet de lire le fichier qui contient les données. 
  # attention : il faut préciser le bon chemin vers le fichier de données.
  # la fonction table() permet de calculer les effectifs de chaque modalité d'une variable catégorielle 
  # la fonction prop.table() permet de calculer les proportion suite à un appel de la fonction table()
  # la fonction round() permet d'arrondir à un certain nombre de chiffres après la virgule 
  satisf <- read.csv2("G:/Bruno/mes documents/Travaux/MOOC/satisfaction_hopital.csv")
## recodage des variables catégorielles
##satisf$profession.c <- factor(satisf$profession,labels=c("agriculteur","artisan","cadre","intermédiaire","employé","ouvrier","sans emploi","autre"))
##satisf$service.c <- factor(satisf$service)
##satisf$sexe.c <- factor(satisf$sexe,labels=c("homme","femme"))

## Table de proportion pour la variable profession
##tabProf <- prop.table(table(satisf$profession.c, useNA="always"))
##names(tabProf)[9] <- "non déclarée"

## Affichage des pourcentages de sujets relevant de chacune des modalités de la variable profession
##print(round(tabProf*100,2))

## idem pour la variable service
##tabServ <- prop.table(table(satisf$service.c))
##print(round(tabServ*100,2))

## idem pour la variable sexe
##tabSexe <- prop.table(table(satisf$sexe.c))
##print(round(tabSexe*100,2))* (Requis)

##Concernant la question "Pour les autres variables, donnez de façon synthétique : moyenne, médiane, ..."
##Exemple de correction:
  ## chargement du package prettyR : on peut utiliser la fonction library() ou la fonction require()
  ##library(prettyR) 
## affichage synthétique des moyennes, médiane, etc. des 6 variables 
##describe(satisf[,c(3,5,6,7,8,9)], num.desc=c("mean","median","sd","min","max","valid.n"))

## Concernant la question "Pour les trois variables catégorielles du fichier, présentez les pourcentages de sujets..."
## Exemple de correction:
  # voici la liste des fonctions R utilisées
  # la fonction read.csv2() permet de lire le fichier qui contient les données. 
  # attention : il faut préciser le bon chemin vers le fichier de données.
  # la fonction table() permet de calculer les effectifs de chaque modalité d'une variable catégorielle 
  # la fonction prop.table() permet de calculer les proportion suite à un appel de la fonction table()
  # la fonction round() permet d'arrondir à un certain nombre de chiffres après la virgule 
  satisf <- read.csv2("G:/Bruno/mes documents/Travaux/MOOC/satisfaction_hopital.csv")
## recodage des variables catégorielles
satisf$profession.c <- factor(satisf$profession,labels=c("agriculteur","artisan","cadre","intermédiaire","employé","ouvrier","sans emploi","autre"))
satisf$service.c <- factor(satisf$service)
satisf$sexe.c <- factor(satisf$sexe,labels=c("homme","femme"))

## Table de proportion pour la variable profession
tabProf <- prop.table(table(satisf$profession.c, useNA="always"))
names(tabProf)[9] <- "non déclarée"

## Affichage des pourcentages de sujets relevant de chacune des modalités de la variable profession
print(round(tabProf*100,2))

## idem pour la variable service
tabServ <- prop.table(table(satisf$service.c))
print(round(tabServ*100,2))

## idem pour la variable sexe
tabSexe <- prop.table(table(satisf$sexe.c))
print(round(tabSexe*100,2))* (Requis)


##Concernant la question "Pour les autres variables, donnez de façon synthétique : moyenne, médiane, ..."
##Exemple de correction:
  ## chargement du package prettyR : on peut utiliser la fonction library() ou la fonction require()
  library(prettyR) 
## affichage synthétique des moyennes, médiane, etc. des 6 variables 
describe(satisf[,c(3,5,6,7,8,9)], num.desc=c("mean","median","sd","min","max","valid.n"))

## Concernant la question "Faites un histogramme du score de relation.
## Exemple de correction:
hist(satisf$score.relation, main="Histogramme de la variable score.relation", xlab="score.relation", ylab="Nombre de sujets", col=rgb(223,241,203,maxColorValue=255))

##Concernant la question "A l’aide de deux « boxplots », représentez côte à côte..."
##Exemple de correction:
# la  which() permet de d'extraire les individus qui vérifient une certaine condition
scorehomme <- satisf[which(satisf[,"sexe"]==0),"score.relation"]
scorefemme <- satisf[which(satisf[,"sexe"]==1),"score.relation"]
boxplot(list(homme = scorehomme, femme = scorefemme),
range = 0.3, varwidth = TRUE, names = c("homme", "femme"),
boxwex = 0.5, border = "blue", col = "pink",
horizontal = TRUE, ylab = "sexe", main = "score relation en fonction du sexe")


