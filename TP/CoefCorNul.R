setwd("~/Desktop/DIVERS_TEMPLATES/R/TP")
smp.c <-read.csv2("DONNEES/smp1.csv")
table(smp.c$age,useNA = "always")
table(smp.c$rs,useNA = "always")
cor.test(smp.c$age,smp.c$rs, method = "spearman")

t.test(smp.c$age, mu =24)

b.debut = c(1,3)
b.fin = c(3,2)
mcnemar.test(b.debut,b.fin)

data <- matrix(c(1, 4, 2, 3), ncol=2, byrow=T)
data
mcnemar.test(data)

data <- matrix(c(25, 5, 15, 15), ncol=2, byrow=T)
data2 <- matrix(c(16, 11, 3, 21, 8, 1), ncol=2, byrow=T)

mcnemar.test(data)
chisq.test(data)

mcnemar.test(data2)
chisq.test(data2)



x.debut =c(22,24,45,34,67,34)
x.fin =c(72,94,45,14,63,33)
t.test(x.debut,x.fin, paired = TRUE)
