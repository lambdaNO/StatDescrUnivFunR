##Définition du répertoire de travail 
setwd("~/Desktop/DIVERS_TEMPLATES/R/DEVOIR/Dev_1")
## Import des données 
X <- read.csv2("satisfaction_hopital.csv")
### Alternative : read.csv2(file="satisfaction_hopital.csv")
str(X)

# Affichage des donnés de X$recommander
table(X$recommander,useNA = "always")
# Recodage de la variable 
## Si X est supérieur strictement à 1, alors X = 1
## Sinon X = 0
X$recommanderBIN <- ifelse(X$recommander > 1, 1, 0)
# Affichage des donnés de X$recommanderBIN
table(X$recommanderBIN, useNA = "always")
## Vérification 
table(X$recommander, X$recommanderBIN, deparse.level=2, useNA="always")

##install.packages("Epi")
library(Epi)
##ou require(Epi)

# Attention au recodage des variables 
#  x est transformée en 1-x, cf. cours
twoby2(1-X$recommanderBIN, 1-X$sexe)


## 
hist(X$age, main="histogramme de l'âge", xlab="âge", ylab="fréquences",freq=F,col="blue")
# Calcul de la corrélation entre les deux variables 
cor(X$score.relation, X$age, use="complete.obs")
# Test de la corrélation entre les deux variables
cor.test(X$score.relation, X$age)
## A la limite de la significativité - Voir pas du tout

# Vérification des hypothèses du test t
# Vérification des effectifs par groupe
table(X$sexe[!is.na(X$score.relation)])
# Vérification de l'égalité des écart-types
### Pour les hommes
sd(X$score.relation[X$sexe==0],na.rm=TRUE)
### Pour les femmes
sd(X$score.relation[X$sexe==1],na.rm=TRUE)
# test t
t.test(X$score.relation~X$sexe, var.equal=TRUE)









# Recodage des variables catégoricielle 
X$profession.recod <- factor(X$profession, labels = c("agriculteur","artisan","cadre","intermédiaire","employé","ouvrier","sans emploi","autre"))
table(X$profession.recod)

X$service.recod <- factor(X$service, labels = c("1","2","3","4","5","6","7","8"))
table(X$service.recod)

# Création du modèle linéaire
mod <- lm(score.relation~age+score.information+amelioration.sante+amelioration.moral+profession.recod+service.recod,data=X)
mod
drop1(mod,.~.,test="F")

summary(mod)

# Condition de validité du test
# normalité du terme de bruit (résidus du modèle)
hist(resid(mod), col="red")

# l'histogramme montre que les termes de bruit suivent une loi normale.
# On peut donc considérer que le modèle est valide.



# Recodage de recommander(catégoricielle) en variable binaire 
table(X$recommander,useNA = "always")
X$recommander.recod <- ifelse(X$recommander > 1, 1,0)
table(X$recommander.recod, useNA = "always")
# Verification croisée
table(X$recommander, X$recommander.recod, deparse.level=2, useNA="always")

# Création du modèle de régression logistique :

mod2 <- glm(recommander.recod~ age+sexe+score.information+amelioration.sante
            + amelioration.moral + profession.recod + service.recod,
            data=X, family="binomial")
mod2

drop1(mod2,.~.,test="Chisq")

summary(mod2)


# conditions de validité
# Au moins 5 à 10 évenements par variable explicative
# 1+1+1+1+1+7+7 variables explicatives: 19*10 = 190 < 269
# On peut donc considérer que le modèle est valide.