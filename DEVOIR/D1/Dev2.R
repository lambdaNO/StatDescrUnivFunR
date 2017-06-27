setwd("~/Desktop/DIVERS_TEMPLATES/R/DEVOIR")
library(Epi)
shd <- read.csv2("satisfaction_hopital.csv")
shd$recommander
table(shd$recommander,useNA = "always")
##Transformez la variable « recommander » en une variable binaire « recommander.b » :
###« recommander.b » vaut 0 si « recommander » vaut 0 ou 1 ;
###« recommander.b » vaut 1 si « recommander » vaut 2.
recommander.b <- ifelse(shd$recommander>1,1,0)
table(recommander.b)
##Calcul de l'Odds Ration
twoby2(1-recommander.b,1-shd$sexe)
##Résultats : ODR : 1.0837
##Intervalle de confiance :  [0.7169,1.6383]

## Calcul du coeffcient de corrélation de PEARSON
cor(shd$score.relation ,shd$sexe ,use="complete.obs")
