#####################################################
setwd("~/Desktop/DIVERS_TEMPLATES/R/TP/LAB3")
smp <- read.csv2("../DONNEES/smp2.csv")
names(smp)
str(smp)
summary(smp)
gauss <- factor(smp$n.enfant)
table(gauss)
levels(gauss)[6:13]<-"5+"
table(gauss)
f <- prop.table(table(gauss))*100
r<-sum(f)/100
f
##"La somme cumulée des fréquence est :")
r
barplot(f, ylim = c(0,30),las=1)

