setwd("~/Desktop/DIVERS + TEMPLATES/R/TP")
smp.c <- read.csv2("/Users/mehdilatif/Desktop/DIVERS + TEMPLATES/R/TP/smp2.csv")
smp <-read.csv2("/comptes/E131729J/XX_Université_fun/univfunR/TP/smp1.csv")
setwd("~/XX_Université_fun/univfunR/TP")

table(smp.c$age)
table(smp.c$n.enfant)
plot(jitter(smp.c$age),jitter(smp.c$n.enfant))
##############################################
str(smp)
cor(smp$age,smp$n.enfant,use="complete.obs")
