smp <-read.csv2("/comptes/E131729J/XX_Université_fun/univfunR/TP/smp1.csv")
setwd("~/XX_Université_fun/univfunR/TP")
library(prettyR)
describe(smp$age)
##Application de la formule ci dessus
borninf <- 38.9 - 1.96*(13.28/sqrt(797))
borninf
bornsup <- 38.9 + 1.96*(13.28/sqrt(797))
bornsup

library(binom)
binom.confint(3,10,method="all")
binom.confint(3,10,method="prop.test")
binom.confint(3,10,method="exact")

binom.confint(300,1000,method="all")



save(smp, file="s2-ic.rda")
savehistory("commande_2.R")
