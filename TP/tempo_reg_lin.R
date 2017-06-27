setwd("~/Desktop/DIVERS_TEMPLATES/R/TP")
smp <- read.csv2("DONNEES/smp2.csv")
str(smp)
plot(smp$age,smp$dur.interv)
plot(jitter(smp$age),jitter(smp$dur.interv))
plot(jitter(smp$age),jitter(smp$dur.interv,factor=4))

plot(jitter(smp$age),jitter(smp$dur.interv,factor=4))
abline(lm(smp$dur.interv~smp$age),lwd=2)

mod <- lm(dur.interv~age,data=smp)
summary(mod)

cor.test(smp$age,smp$dur.interv)

table(smp$dep.cons)
plot(jitter(smp$dep.cons,factor=1),jitter(smp$dur.interv,factor=4))
abline(lm(smp$dur.interv~smp$dep.cons),lwd=2,col="red")


mod <- lm(dur.interv~dep.cons,data = smp)
summary(mod)

t.test(smp$dur.interv~smp$dep.cons, var.equal =TRUE) 

abs(58.9234-66.53767)

mod_reg_mul <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons,data = smp)
summary(mod_reg_mul)

mod_reg_mul <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons+prof,data = smp)
summary(mod_reg_mul)

smp$prof <- relevel(smp$prof,ref="ouvrier")
mod_reg_mul <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons+prof,data = smp)
summary(mod_reg_mul)

drop1(mod_reg_mul,.~.,test="F")


mod_reg_mul <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons,data = smp)
summary(mod_reg_mul)

mod_reg_mul_intera <- lm(dur.interv~age+dep.cons*subst.cons+scz.cons,data = smp)
summary(mod_reg_mul_intera)

mod_reg_mul <- lm(dur.interv~prof, data = smp)
summary(mod_reg_mul)

drop1(mod_reg_mul,.~.,test="F")

mod_reg_mul <- lm(dur.interv~age+dep.cons+subst.cons+scz.cons,data = smp)
hist(resid(mod_reg_mul),col = "blue",main="")

### Régression logisitique 

setwd("~/Desktop/DIVERS_TEMPLATES/R/TP")
smp <- read.csv2("DONNEES/smp2.csv")
str(smp)

mod_reg_log = glm(suicide.hr~abus,data= smp, family = "binomial")
summary(mod_reg_log)

exp(0.7688)

library(Epi)
twoby2(1-smp$suicide.hr,1-smp$abus)
