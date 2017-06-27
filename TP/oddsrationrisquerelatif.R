###################################################################################
######################################################################

smp <-read.csv2("/comptes/E131729J/XX_Université_fun/univfunR/TP/smp1.csv")
setwd("~/XX_Université_fun/univfunR/TP")
##variable de mesure d'évitement du danger avant encodage binaire
table(smp$ed,useNA = "always")
##variable de mesure d'évitement du danger après encodage binaire
### SI la valeur de ed est supérieure stricte à 2 (=3 -> forte), alors smp$ed.bin[i] = 1, sinon smp$ed.bin[i]=0
smp$ed.bin <- ifelse(smp$ed>2,1,0)
table(smp$ed.bin,useNA = "always")
##Analyse à l'oeil nu des valeurs avant et apès modification
str(smp)
##Analyse croisée de ed.bin par rapport à ed grâce à la fonction table :
table(smp$ed.bin,smp$ed,useNA = "always")
###Ajout du nom des variables 
table(smp$ed.bin,smp$ed,deparse.level = 2,useNA = "always")
###################################################################################
######
library(Epi)
twoby2(1-smp$ed.bin,1-smp$dep.cons)
