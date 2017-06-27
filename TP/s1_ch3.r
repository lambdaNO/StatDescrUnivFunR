
setwd("~/XX_Université_fun/univfunR/TP")
outil <-read.csv2("/comptes/E131729J/XX_Université_fun/univfunR/TP/outils_hdrs.csv")


str(outil)
summary(outil)

library(prettyR)
library(moments) #pour skewness
q25<-function(x,na.rm) { return(quantile(x,probs=0.25,na.rm=na.rm))}
q75<-function(x,na.rm) { return(quantile(x,probs=0.75,na.rm=na.rm))}


describe(outil, num.desc =c("mean","sd","var","median","min","max","valid.n","skewness","kurtosis","q25","q75"))

smp <-read.csv2("/comptes/E131729J/XX_Université_fun/univfunR/TP/smp1.csv")


mean(smp$age)
mean(smp$age, na.rm = TRUE)
sd(smp$age)
sd(smp$age, na.rm = TRUE)

table(smp$prof, deparse.level = 2, useNA = "always")

save(outil, file="s1_ch3_v1.rda")
savehistory("commande_2.R")
