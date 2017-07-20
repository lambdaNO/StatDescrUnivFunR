
setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")
library(Rcmdr)
Commander()
tauxNO <- read.table("/Users/mehdilatif/Desktop/DIVERS_TEMPLATES/StatDesR/TP/NO2.txt",header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
str(tauxNO)
table(tauxNO$fluidite)
table(tauxNO$type)
summary(tauxNO$NO2)
library(abind, pos=15)
library(e1071, pos=16)
numSummary(tauxNO[,"fluidite"], statistics=c("mean", "sd", "IQR", "quantiles"),quantiles=c(0,.25,.5,.75,1))
table(tauxNO$fluidite)
## Pour calculer eta, on peut utiliser le menu Statistiques\Moyennes\ANOVA à un facteur... : 
## eta2 correspond au rapport des ”Sum Sq” (somme des carrés) pour le facteur sur ceux des ”Residuals” (résidus).
library(mvtnorm, pos=17)
library(survival, pos=17)
library(MASS, pos=17)
library(TH.data, pos=17)
library(multcomp, pos=17)
AnovaModel.1 <- aov(fluidite ~ type, data=tauxNO)
summary(AnovaModel.1)
with(tauxNO, numSummary(fluidite, groups=type, statistics=c("mean", "sd")))

## Graphe par groupe... de Graphes\Boite de dispersion...
boxplot(NO2~type, data=tauxNO, id.method="y")
boxplot(NO2~fluidite, data=tauxNO, id.method="y")

round(prop.table(table(tauxNO$fluidite,tauxNO$type)),2)

x<-table(tauxNO$fluidite,tauxNO$type)

tabligne=cbind(addmargins(prop.table(addmargins(x,1),1),2), c(margin.table(x,1),sum(x)))
colnames(tabligne)<-c(colnames(x),"TOTAL","EFFECTIF")
round(tabligne,2)

tabcol=rbind(addmargins(prop.table(addmargins(x,2),2),1)*100, c(margin.table(x,2),sum(x)))
rownames(tabcol)<-c(rownames(x),"TOTAL","EFFECTIF")

round(tabcol,2)

Commander()


chisq.test(tauxNO$type,tauxNO$fluidite,correct = FALSE)

x
#y <- addmargins(x,2)

y

y<-addmargins(prop.table(addmargins(x,1),1),2) 
y


##http://olivier.godechot.free.fr/hoparticle.php?id_art=465