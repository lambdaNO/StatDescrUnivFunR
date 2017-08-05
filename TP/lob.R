library(MASS)
data(survey)
names(survey)

hist( survey$Height, col = grey(0.9), border = grey(0.2),
      main = paste("Taille de", nrow(survey), "étudiants"),
      xlab = "Taille [cm]",
      ylab = "Effectifs",
      labels = TRUE, las = 1, ylim = c(0, 50))

hist( survey$Height, col = grey(0.9), border = grey(0.2),
      main = paste("Taille de", nrow(survey), "étudiants"),
      xlab = "Taille [cm]",
      proba = TRUE)
x <- seq(from = min(survey$Height, na.rm=T), to = max(survey$Height, na.rm=T),
         length = 100)
lines(x, dnorm(x, mean(survey$Height, na.rm = TRUE), sd(survey$Height,
                                                        na.rm = TRUE)))
mtext("Ajustement (mauvais) a une loi normale")

hist( survey$Height, col = grey(0.9), border = grey(0.2),
      main = paste("Taille de", nrow(survey), "étudiants"),
      xlab = "Taille [cm]",
      proba = TRUE, breaks = seq(from = 150, to = 200, length = 15))


isohist <- function(x, nclass, ...){
  breaks <- quantile(x, seq(from = 0, to = 1, length = nclass + 1),
                     na.rm = TRUE)
  invisible(hist(x, breaks = breaks, ...))
}
isohist(survey$Height, 10, col = grey(0.9), border = grey(0.2),
        main = paste("Taille de", nrow(survey), "étudiants"),
        xlab = "Taille [cm]",
        proba = TRUE)

hist( survey$Height, col = grey(0.9), border = grey(0.8),
      main = paste("Taille de", nrow(survey), "étudiants"),
      xlab = "Taille [cm]",
      proba = TRUE)
lines(density(survey$Height, na.rm = TRUE), lwd = 2)

hist( survey$Height, col = grey(0.9), border = grey(0.8),
      main = paste("Taille de", nrow(survey), "étudiants"),
      xlab = "Taille [cm]",
      proba = TRUE)
lines(density(survey$Height, na.rm = TRUE, adjust = 0.15), lwd = 2)
mtext("adjust = 0.15")


hist( survey$Height, col = grey(0.9), border = grey(0.8),
      main = paste("Taille de", nrow(survey), "étudiants"),
      xlab = "Taille [cm]",
      proba = TRUE)
lines(density(survey$Height, na.rm = TRUE, adjust = 2), lwd = 2)
mtext("adjust = 2")

par(lend="butt")
ng <- sum(survey$Sex == "Male", na.rm = TRUE)
nf <- sum(survey$Sex == "Female", na.rm = TRUE)
n <- ng + nf
dst <- density(survey$Height, na.rm = TRUE)
dstg <- density(survey$Height[survey$Sex == "Male"], na.rm = TRUE)
dstf <- density(survey$Height[survey$Sex == "Female"], na.rm = TRUE)
hist( survey$Height, col = grey(0.9), border = grey(0.8),
      main = paste("Taille de", nrow(survey), "étudiants"),
      xlab = "Taille [cm]",
      proba = TRUE, ylim = c(0, max(dst$y)))
lines(dstg$x, ng/n*dstg$y, lwd = 3, col = "darkblue")
lines(dstf$x, nf/n*dstf$y, lwd = 3, lty = 2, col = "darkred")
lines(dst$x, dst$y)
legend("topright", inset = 0.01, legend = c("Filles", "Garcons","Total"),
       col = c("darkred","darkblue","black"),
       lty = c(2, 1,1), lwd = 2, pt.cex = 2)

library(ade4)
data(deug)
plot(table(deug$tab$Option1), main = paste("Notes de", nrow(deug$tab),
                                           "étudiants"),
     las = 1, xlab = "note (Option 1)", ylab = "Nombre d'étudiants")

plot(table(deug$tab$Option1), main = paste("Notes de", nrow(deug$tab),
                                           "étudiants"),
     las = 1, xlab = "note (Option 1)", ylab = "Nombre d'étudiants")
dst <- density(deug$tab$Option1, adjust = 0.1)
lines(dst$x, max(table(deug$tab$Option1))*dst$y/max(dst$y), col = "red")

plot(table(deug$tab$Option1), main = paste("Notes de", nrow(deug$tab),
                                           "étudiants"),
     las = 1, xlab = "note (Option 1)", ylab = "Nombre d'étudiants",
     lwd = 5, lend = "square")

boxplot( survey$Pulse, col = grey(0.8),
         main = paste("Rythme cardiaque de", nrow(survey), "étudiants"),
         ylab = "Pulsations/minutes", las = 1)
rug(survey$Pulse, side = 2)

boxplot( survey$Pulse, col = grey(0.8),
         main = paste("Rythme cardiaque de", nrow(survey), "étudiants"),
         ylab = "Pulsations/minutes", las = 1)
rug(survey$Pulse, side = 2)
abline( h = median(survey$Pulse, na.rm = TRUE), col = "navy")
text(1.35, 70, "Médiane", col = "navy")
Q1 <- quantile(survey$Pulse, probs = 0.25, na.rm = TRUE)
abline( h = Q1, col = "darkred")
text(1.25, 62, "Q1 : premier quartile", col = "darkred")
Q3 <- quantile(survey$Pulse, probs = 0.75, na.rm = TRUE)
abline( h = Q3, col = "darkred")
text(1.25, 83, "Q3 : troisième quartile", col = "darkred")
arrows(x0 = 0.7,y0 =quantile(survey$Pulse, probs = 0.75, na.rm = TRUE),x1 = quantile(survey$Pulse, probs = 0.25, na.rm = TRUE),y1 = quantile(survey$Pulse, probs = 0.25, na.rm = TRUE),length = 0.1,code = text(0.7, 69, "h", pos = 2))
mtext("L'écart inter-quartile h contient 50 % des individus", side = 1)
abline( h = Q1-1.5*(Q3-Q1), col = "darkgreen")
text(1.35, 42, "Q1 -1.5 h", col = "darkgreen")
abline( h = Q3+1.5*(Q3-Q1), col = "darkgreen")
text(1.35, 104, "Q3 +1.5 h", col = "darkgreen")

boxplot(survey$Pulse~survey$Sex, col = c("lightpink","lightblue"),
        main = paste("Rythme cardiaque de", nrow(survey), "étudiants"),
        ylab = "Pulsations/minutes", las = 1)

boxplot( survey$Height~survey$Sex, col = c("lightpink","lightblue"),
         main = paste("Taille de", nrow(survey), "étudiants"),
         ylab = "Taille [cm]", las = 1, notch = TRUE)

library(sm)
library(vioplot)
par(las = 1)
vioplot(survey$Height[!is.na(survey$Height)], h = 1.5, names ="",
        col = "lightblue")
title(main = paste("Taille de", nrow(survey), "étudiants"))

genet <- read.table("http://pbil.univ-lyon1.fr/R/donnees/qualitatif.txt",
                    header=TRUE)
summary(genet)


pie(table(genet$cheveux), col = c("yellow", "chocolate4", "black", "orangered"),
    main = "Couleur des cheveux de 592 étudiants")

set.seed(01071966)
data <- rep(10,10) + rep( 2*runif(5), rep(2,5)) + rep(c(-2,2),5)
data <- 100*data/sum(data) # as percentage
names(data) <- letters[1:10]
par(mfrow = c(1, 2))
pie(data, main = "Diagramme en secteur")
dotchart(data, xlim = c(0, 14), pch = 19, main = "Graphe de Cleveland")
par(mfrow = c(1, 1))

data <- sort(as.numeric(table(genet$cheveux)))
par(mfrow = c(1, 2))
pie(data, col = c("orangered", "black", "yellow2", "chocolate4"), cex = 1.5)
dotchart(data, xlim = c(0, max(data)), pch = 21, bg = c("orangered",
                                                        "black","yellow2","chocolate4"), cex = 1.5)
par(mfrow=c(1,1))

library(MASS)
data(Cars93)
par(mfrow=c(1,2))
dotchart(as.numeric(table(Cars93$Manufacturer)), pch=19, cex = 0.8,
         main="Dans le desordre")
dotchart(sort(as.numeric(table(Cars93$Manufacturer))), pch=19, cex = 0.8,
         main = "Dans l'ordre")
par(mfrow=c(1,1))

data(Titanic)
classe <- apply(Titanic, 1, sum)[1:3]
pie(classe,main="Don't try this at home kids!")

dotchart(rev(classe), main="Classe des passagers du Titanic", pch = 19,
         xlim = c(0,max(classe)), cex = 1.5)

genet <- read.table("http://pbil.univ-lyon1.fr/R/donnees/qualitatif.txt",header=TRUE)
(tc <- table(genet[,1:2]))

library(gdata)
library(gtools)
library(gplots)
balloonplot(tc, dotsize=10)

par(mfrow = c(1,2))
balloonplot(tc, dotsize=8, main = "Observé")
balloonplot(as.table(chisq.test(tc)$expected), dotsize=8, main = "Attendu")
par(mfrow=c(1,1))
mosaicplot(tc, shade = TRUE, las =1,
           main = paste("Yeux et cheveux de", nrow(genet),"étudiants"))

boxplot(deug$tab$Algebra~deug$result, at = c(1, 5, 6, 2, 3, 4),
        col = grey(0.8), xlab = "Resultat final", ylab = "Note en Algebre",
        varwidth = TRUE,
        main = paste("Notes de", nrow(deug$tab),"étudiants"))

plot(x = deug$tab$Proba, y = deug$tab$Algebra, pch = 20,
     main = paste("Notes de", nrow(deug$tab),"étudiants"),
     xlab = "Note en Probabilité", ylab = "Note en Algèbre", las = 1)


data(crimtab)
crimtab.dft <- as.data.frame(crimtab)
str(crimtab.dft)
head(crimtab.dft,3)
expand.dft <- function(x, na.strings = "NA", as.is = FALSE, dec = ".") {
  DF <- sapply(1:nrow(x), function(i) x[rep(i, each = x$Freq[i]), ],
               simplify = FALSE)
  DF <- subset(do.call("rbind", DF), select = -Freq)
  for (i in 1:ncol(DF))
  {
    DF[[i]] <- type.convert(as.character(DF[[i]]),
                            na.strings = na.strings,
                            as.is = as.is, dec = dec)
  }
  DF
}
crimtab.raw <- expand.dft(crimtab.dft)
x <- crimtab.raw[,1]
y <- crimtab.raw[,2]
plot(x, y, las = 1, main = "3000 criminels", ylab = "Taille [cm]",
     xlab = "Majeur gauche [cm]")

sunflowerplot(crimtab.raw, las = 1, main = "3000 criminels",
              ylab = "Taille [cm]", xlab = "Majeur gauche [cm]", size = 1/20)

xyg <- expand.grid(as.numeric(rownames(crimtab)),
                   as.numeric(colnames(crimtab)))
symbols(x = xyg[,1], y = xyg[,2], circles = sqrt(as.vector(crimtab)),
        inches = 0.2, bg = rgb(0.5,0.5,0.5,0.5), xlab = "x", ylab ="y", las=1,
        main = "Avec des symboles de taille variable")

par(mfrow = c(1,2))
plot(x, y, main = "Sans bruitage")
plot(jitter(x), jitter(y), main = "Avec bruitage")

par(mfrow=c(1,1))

edl <- kde2d(x,y, n = 100)
image(edl, main = "Avec un estimateur de la densite locale",
      xlab = "x", ylab = "y")

contour(edl, main = "Avec un estimateur de la densite locale", xlab = "x",
        ylab = "y")

filled.contour(edl, main = "Avec un estimateur de la densite locale",
               xlab = "x", ylab = "y", color = terrain.colors)

filled.contour(edl, main = "Avec un estimateur de la densite locale",
               xlab = "x", ylab = "y", color = heat.colors)

filled.contour(edl, main = "Avec un estimateur de la densite locale",
               xlab = "x", ylab = "y", color = topo.colors)

filled.contour(edl, main = "Avec un estimateur de la densite locale",
               xlab = "x", ylab = "y", color = cm.colors)

smoothScatter(x,y)

persp(edl, xlab = "x", ylab = "y", zlab = "density", theta = 45, phi = 20,
      main = "Avec un estimateur de la densite locale")

library(rgl)
persp3d(edl, xlab = "x", ylab = "y", zlab = "density", theta = 45, phi = 20,
      main = "Avec un estimateur de la densite locale")

f <- function(x) { sin(x) + sin(0.9*x) }
curve(f, from = 0, to = 50*pi, n = 1000, main = "Battements", ylim = c(-3,3), col = "purple")

data(sunspots)
plot(sunspots, main = "Évolution de la densité de taches solaires",
     xlab = "Temps", ylab = "Taches solaires")

n <- 4
wd <- seq( start(sunspots)[1], end(sunspots)[1], length = n+1)
opar <- par(no.readonly = TRUE)
par(mfrow = c(n,1), mar = c(3,3,1,1))
for( i in 1:(length(wd)-1))
{
  plot( window(sunspots, wd[i], wd[i+1]), ylab = "", xlab = "", las = 1,
        ylim = c(0,max(sunspots)))
}

opar <- par(no.readonly = TRUE)
par(mfrow = c(n,1), mar = c(3,3,1,1))
for( i in 1:(length(wd)-1))
{
  plot( 
    window(sunspots, wd[i], wd[i+1]), 
    ylab = "", xlab = "", las = 1, col ="darkgrey",lwd = 0.8,
    ylim = c(0,max(sunspots)))
  
    lines(lowess(window(sunspots, wd[i], wd[i+1]), f = 0.025), 
    lwd = 1.3,col="red")
}

par(mfrow = c(2,3))
for(i in 1:6) plot(0, main = i)

par(mfrow = c(2,3))
par(col = "red")
plot(0, main = "col = \"red\"", sub = "sous-titre")
par(col = "black", col.axis = "red")
plot(0, main = "col.axis = \"red\"", sub = "sous-titre")
par(col.axis = "black", col.lab = "red")
plot(0, main = "col.lab = \"red\"", sub = "sous-titre")
par(col.lab = "black", col.main = "red")
plot(0, main = "col.main = \"red\"", sub = "sous-titre")
par(col.main = "black", col.sub = "red")
plot(0, main = "col.sub = \"red\"", sub = "sous-titre")
par(col.sub = "black", fg = "red")
plot(0, main = "fg = \"red\"", sub = "sous-titre")

par(mfrow=c(1,1))
par(mar = par("mar") + c(0, 1, 0, 0))
plot(0, xlab = "xlab", ylab = "Une légende un peu bavarde\nsur deux lignes",
     main = "main")

plot(1:10,main = par("usr"))

par(mfrow=c(2,2))
for(cex in seq(from = 0.8, to = 1.2, length = 4))
{
  par(cex=cex)
  plot(0, main = paste("cex =", round(cex,1)))
}

par(mfrow=c(1,1))
par(bg = "lightblue")
plot(rnorm(100),rnorm(100), pch = 21, bg = "yellow")

par(bg = "white")
par(mfrow=c(2,2))
for(i in 0:3) plot(0,las=i,main=paste("las =",i), font.axis = 2,
                   col.axis = "red")

par(mfrow=c(1,1))
plot(0)
abline(h=0)
par(xpd=NA)
abline(h=0.5,col="red",lwd=2)

