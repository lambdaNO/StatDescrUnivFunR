####LAB 4#####
setwd("~/Desktop/DIVERS_TEMPLATES/R/TP/LAB4")
load("~/Desktop/DIVERS_TEMPLATES/R/TP/smp_v1.rda")
table(smp$subst.cons)
table(smp$subst.cons,useNA = "always")
table(smp$subst.cons, smp$abus)
tab <- table(smp$subst.cons, smp$abus)
tab
prop.table(tab)
##margin = 1 -> tous les effectifs vont être rapportés aux totaux lignes
prop.table(tab, margin = 1)
##margin = 2 -> tous les effectifs vont être rapportés aux totaux colonnes
prop.table(tab, margin = 2)
tob<-xtabs(~subst.cons + abus, smp )
barplot(tob,beside = TRUE, xlab="Modalités : Consommation de substance et Abus", ylab = "Effectifs")
#############################
tab
chisq.test(tab)
res <- chisq.test(tab)
res
##Retourne le nom de la variable étudiée 
res$data.name
##Retourne la méthode utilisée pour le test
res$method
##Retourne le résultat statistique du test 
res$statistic
##Retourne le degré de liberté
res$parameter
##Retourne le résultat du test p-value
res$p.value
##Retourne le tableau de contingence sur lequel nous avons effectuée le test
res$observed
##Retourne le tableau de contingence théorique que nous devrions obtenir
###Effectifs attendus sous l’hypothèse d’indépendance
res$expected
#############################
tab
fisher.test(tab)

#############################

head(smp$age)
table(smp$subst.cons)
table(smp$subst.cons, useNA = "always")

tapply(smp$age, smp$subst.cons, mean)
tapply(smp$age, smp$subst.cons, mean,na.rm= TRUE)


t.test(smp$age[smp$subst.cons == 0],smp$age[smp$subst.cons == 1])
t.test(smp$age[smp$subst.cons == 0],smp$age[smp$subst.cons == 1],var.equal = TRUE)


##L'âge expliqué par la variable subst.cons
t.test(age~subst.cons,smp) 

xtabs(age~subst.cons,smp)

aggregate(age ~ subst.cons, smp, mean)

ag <- aggregate(age ~ subst.cons, smp, mean)
boxplot(ag) 


boxplot(age~subst.cons,smp)
