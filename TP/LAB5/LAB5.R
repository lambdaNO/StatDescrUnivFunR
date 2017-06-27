##Défintion du des Workspaces
setwd("~/Desktop/DIVERS_TEMPLATES/R/TP/LAB5")
##Chargement des données du fichier smp
smp <- read.csv2("../DONNEES/smpc.csv")
### Autre possiblité pour charger directement un fichier avec l'historique des modification : 
#load("~/Desktop/DIVERS_TEMPLATES/R/TP/smp_v1.rda")
##Affichage des données du Data Frame
str(smp)
##Affichage du résumé des données du Data Frame
summary(smp)
###############################################
##Nous allons nous intéresser à un sous ensemble du Data Frame smp 
##Nous allons regarder pour les individus dont la profession est [ss-emploi & cadre & profession intermédiaire], l'âge et le nombre d'enfant : 
###Note : | est le OU logique dans R
##Les Premières lignes
head(subset(smp, prof == "sans emploi" | prof == "prof.intermediaire" | prof == "cadre", c(age,n.enfant,prof)))
##Autre écriture 
head(subset(smp, prof %in% c("sans emploi", "prof.intermediaire", "cadre"), c(age, n.enfant, prof)))
##L'ensemble des observations : 
subset(smp, prof == "sans emploi" | prof == "prof.intermediaire" | prof == "cadre", c(age,n.enfant,prof))
## Sauvegarde dans un objet : 
smpb <- subset(smp, prof == "sans emploi" | prof == "prof.intermediaire" | prof == "cadre", c(age,n.enfant,prof))
## On a maintenant : smp (799 obs. of 27 variables) et smpb (246 obs. of 3 variables)
summary(smpb)
##On remarque que pour la variable smpb.prof, R à conservé les anciens niveaux qui n'ont plus lieu d'être ici
## On va donc appliquer la fonction factor pour que R recalcule les niveaux de la variable
smpb$prof <- factor(smpb$prof)
summary(smpb)
##NE PAS CALCULER CE FACTEUR CAR CELA TRANSFORME UNE VARIABLE QUANTITATIVE EN VARIABLE QUALITATIVEsmpb$age <- factor(smpb$age)
##NE PAS CALCULER CE FACTEUR CAR CELA TRANSFORME UNE VARIABLE QUANTITATIVE EN VARIABLE QUALITATIVE smpb$n.enfant <- factor(smpb$n.enfant)
##On a donc :
summary(smpb)
##Représentation soous forme de tableau 
###Variable d'origine : 
table(smp$prof)
###Variable restreinte : 
table(smpb$prof)
##Resumer le nombre d'enfant moyen en fonction de la profession : 
###Dans le tableau restreint
agg<-aggregate(smpb$n.enfant ~ smpb$prof, data=smpb, mean)
agg
##Représentation graphique : 
boxplot(smpb$n.enfant ~ smpb$prof, data=smpb)
agg<-aggregate(n.enfant ~ prof, data=smpb, mean)
agg
##\begin{figure}[H]\begin{center}\includegraphics[scale=1]{ilu/lab5-1.png}\end{center}\end{figure}
boxplot(n.enfant ~ prof, data=smpb,xlab="Profession", ylab="Nombre d'enfants",col="cornflowerblue", border="cornflowerblue")
##\begin{figure}[H]\begin{center}\includegraphics[scale=1]{ilu/lab5-2.png}\end{center}\end{figure}

#############
##Nous allons a présent réaliser une ANOVA
###Recherche des informations sur la fonction Linear Model
help(lm)
lm(n.enfant ~ prof, data=smpb)
##Nous allons stocker le résultat de notre modèle de régression dans la variable m : 
m <- lm(n.enfant ~ prof, data=smpb) 
m
## Dans le cadre d'une ANOVA, nous allons utiliser la fonction qui en donnant le nom d’une variable et en spécifiant un test de Fisher Snedecor, de fournir un tableau d’analyse de variance,
drop1(m,test = "F")
###Prof est la variable explicative
###2, le degré de liberté 
### F value, correspondant à la valeur au test d'analyse de variance.
###=>la statistique de test 3.83 et ici le degré de significativité 0.02.

###Pour deux variables numériques : 
n <- lm(n.enfant ~ age, data=smpb) 
n
##Intercept = ordonnée à l'origine 
##age coef directeur

##Pour avoir les tests associés on tapera simplement summary(m) et on aura un tableau avec les coefficients de régression et les tests t associés ici pour la pente 10.77 et le degré de significativité.
summary(n)

##########################
##Depuis le début, nous travaillons sur un Data Frame Restreint que l'on a obtenu avec la fonction subset. La fonction lm() permet de réduire directement le Data Frame : 
##On a donc juste à travailler avec le tableau d'origine et de définir les options de filtre que l'on souhaite appliquer : 
m<-lm(n.enfant ~ age,smp, subset =prof == "sans emploi" | prof == "prof.intermediaire" | prof == "cadre")
summary(m)
##On obtient donc les même valeurs

## l’intérêt ici, c’est que l’on peut utiliser à la fois une notation par formule, on décrit la relation entre le nombre d’enfants qui est la variable de réponse et l’âge qui est la variable explicative, ces variables se trouvent dans le data-frame qui s’appelle smp. Par contre ce data-frame-là va être filtré selon les critères qui sont indiqués (dans la commande) dans l’option subset.


############################################@
##On ne va s’intéresser qu’aux individus qui remplissent les conditions profession égal soit sans emploi, soit profession intermédiaire, soit cadre.
##Dès lors que l'on a un modèle de regression, on peut utiliser la commande coef pour afficher ces derniers :
coef(m)

coef(m)[1]
coef(m)[2]

##On peut utiliser pour obtenir les intervalles de confiance : 
confint(m)
##un tableau d’analyse de variance associé à la régression à l’aide de la commande anova() :
anova(m)

############################################
##Lorsque l’on souhaite réaliser des prédictions sur des valeurs non nécessairement observées on peut utiliser la commande predict().
###Dans ces cas-là on va lui donner le nom de la variable dans laquelle on a stocké notre modèle de régression et un data-frame dans lequel on va indiquer pour la variable qui sert de variable explicative les valeurs pour lesquelles on souhaite effectuer la prédiction.

predict(m,data.frame(age=c(20,30,40)))
##Si l'on souhaite déterminer en même temps les intervalles de confiances : 
predict(m,data.frame(age=c(20,30,40)),interval ="confidence" )
##« fit » les valeurs prédites et « lwr », « upr » représentent les bornes inférieures et supérieures des intervalles de confiance à 95% pour la prévision.

############################################
##On peut également s'intéresser à la régression logisitique : 
### On prend une varaible binaire (que l'on va construire) On va s’intéresser au nombre d’enfants supérieur à 2. Dans ces cas-là on codera 1 sinon on code 0
smp$n.enfant.bin <- ifelse(smp$n.enfant > 2,1,0)
table(smp$n.enfant)
table(smp$n.enfant.bin)
##Pour effectuer une regression logisitique : c'est la commande glm() : Generalized Linear Models
help(glm)
m <- glm(n.enfant.bin ~ age , smp , family =binomial("logit"))
summary(m)
##cette fois-ci on a la variable explicative avec la valeur du coefficient de régression sur l’échelle du log odds.

