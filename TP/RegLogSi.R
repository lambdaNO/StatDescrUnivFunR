#####
setwd("~/Desktop/DIVERS_TEMPLATES/R/TP")
smp <-read.csv2("DONNEES/smp2.csv")

mod <- glm(suicide.hr~abus, data = smp, family="binomial")
summary(mod)
######
exp(0.7688)



######
exp(0.7688)

library(Epi)
twoby2(1-smp$suicide.hr, 1-smp$abus)
######