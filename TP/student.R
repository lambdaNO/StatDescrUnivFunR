setwd("~/Desktop/DIVERS_TEMPLATES/R/TP")
smp.c <-read.csv2("DONNEES/smp1.csv")
smp.c$ed.bin <- ifelse(smp.c$ed>2,1,0)
table(smp.c$age)
hist(smp.c$age,main="",xlab="",ylab="")
qqnorm(smp.c$age)
qqline(smp.c$age)
##Afichage des effectifs par modalité 
table(smp.c$age)
##Calcul de la variance
v<-var(table(smp.c$age))
v
##Calcul de l'écart type 
s<-sd(table(smp.c$age))
s
##Vérification s^2=v (arondi à deux décimales)
(round(s^2,2))==(round(v,2))

by(smp.c$age,smp.c$ed.bin,var,na.rm=TRUE)

x = 13.38593
y = 13.29636
(x>=(y)*(1.5))
t.test(smp.c$age~ smp.c$ed.bin, var.equal = TRUE)
wilcox.test(smp.c$age~smp.c$ed.bin)
