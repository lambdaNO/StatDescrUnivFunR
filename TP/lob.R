setwd("~/Desktop/DIVERS_TEMPLATES/StatDesR/TP")

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



############################################################

############################################################

############################################################

############################################################

library(ggplot2)
library(ggplot)
library(dplyr)

set.seed(1234)
wdata = as_data_frame(data.frame(sex = factor(rep(c("F", "M"), each=200)), weight = c(rnorm(200,55),rnorm(200,58))))
wdata
mu <- wdata %>% group_by(sex) %>% summarize(grp.mean = mean(weight))
mu
a <- ggplot(wdata, aes(x = weight))
a + geom_area(stat = "bin", color = "black", fill = "#00AFBB")

a + geom_area(aes(y = ..density..), stat = "bin")

data("diamonds")
diamonds <- as_data_frame(diamonds)
diamonds

p <- ggplot(diamonds, aes(x = price, fill = cut))
# Bar plot
p + geom_bar(stat = "bin")

p + geom_area(stat = "bin")

a + geom_density()

a + geom_density(color = "black", fill = "gray") + geom_vline(aes(xintercept = mean(weight)), color = "#FC4E08", linetype = "dashed", size = 1) + geom_vline(aes(xintercept = median(weight)), color = "blue", linetype = 4, size = 1)

a + geom_density(aes(fill = sex), alpha = 0.4) 

a + geom_density(aes(fill = sex), alpha = 0.4) + geom_vline(data = mu, aes(xintercept = grp.mean, color = sex), linetype = "dashed")

a2 <- a + geom_density(aes(color = sex)) + geom_vline(data = mu, aes(xintercept = grp.mean, color = sex), linetype = "dashed") + theme_minimal()

a2 + scale_color_manual(values = c("#999999", "#E69F00"))

a2 + scale_color_brewer(palette = "Paired")

a2 + scale_color_grey()

a3 <- a + geom_density(aes(fill = sex), alpha = 0.4) + theme_minimal()

a3 + scale_fill_manual(values = c("#999999", "#E69F00"))

a3 + scale_fill_brewer(palette = "Dark2")

a3 + scale_fill_grey()

a + geom_histogram()

a + geom_histogram(bins = 50)

a + geom_histogram(bins = 50, color = "black", fill = "grey") + geom_vline(aes(xintercept = mean(weight)), color = "#FC4E07", linetype = "dashed", size = 1) + theme_minimal()

a + geom_histogram(aes(y = ..density..), bins = 50)

# Change color by sex
a + geom_histogram(aes(color = sex), fill = "white", bins = 50) + theme_minimal()

# Position adjustment "identity"(overlaid)
a + geom_histogram(aes(color = sex), fill = "white", bins = 50, alpha = 0.6, position = "identity")

# Position adjustment "dodge" (Interleaved)
# Add mean lines and color by sex
a + geom_histogram(aes(color = sex), fill = "white", alpha = 0.6, position = "dodge", bins = 50) + geom_vline(aes(xintercept = mean(weight)), linetype = "dashed")

# Change fill, color manually
# Change outline color manually
a + geom_histogram(aes(color = sex), fill = "white", alpha = 0.4, position = "identity", bins = 50) + scale_color_manual(values = c("#00AFBB","#E7B800"))

a + geom_histogram(aes(color = sex, fill = sex), alpha =0.4, position = "identity", bins = 50) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + scale_color_manual(values = c("#00AFBB", "#E7B800")) 

a + geom_histogram(aes(y = ..density..),color = "black", fill = "white") + geom_density(alpha = 0.2, fill = "#FF6666") + theme_minimal()

# Color by groups
a + geom_histogram(aes(y = ..density.., color = sex, fill = sex),  alpha = 0.4, position = "identity") + geom_density(aes(color = sex), size =1)

# Basic plot
a + geom_freqpoly(bins = 30) + theme_minimal()

# Change color and linetype by sex
# Use custom color palettes
a + geom_freqpoly(aes(color = sex, linetype = sex), bins = 30 ) +  scale_color_manual(values = c("#999999", "#E69F00"))+theme_minimal()

# y density
a + geom_freqpoly(aes(y = ..density.., color = sex, linetype = sex), bins = 30 ) +  scale_color_manual(values = c("#999999", "#E69F00"))+theme_minimal()

a + geom_dotplot(aes(fill = sex))

a + stat_ecdf(geom = "point")

a + stat_ecdf(geom = "step")

data(mtcars)
mtcars <- as_data_frame(mtcars)
mtcars

mtcars <- mutate(mtcars, cyl = as.factor(cyl))
mtcars

p <- ggplot(mtcars, aes(sample = mpg))
# Basic plot
p + stat_qq()

# Change point shapes by groups
# Use custom color palettes
p + stat_qq(aes(shape = cyl, color = cyl)) + scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

data(mpg)
mpg <- as_data_frame(mpg)
mpg

ggplot(mpg, aes(fl)) + geom_bar(fill = "steelblue") + theme_minimal()

# Data format
mtcars

library(data.table)

b <- ggplot(mtcars, aes(x = wt, y= mpg, cyl = factor(cyl))) +geom_point()
# x weight
# y miles/gallon
#Basic scatter plots
b + geom_point(color = "#00AFBB")

# Change the point size, and shape
b + geom_point(color = "#00AFBB", size = 2, shape = 23)

# Control point size by continuous variable values
# qsec 1/4 mile time
b + geom_point(aes(size = qsec), color = "#00AFBB")

# Label text
b + geom_point() + geom_text(label = rownames(mtcars), nudge_y = 0.8)

# Change shape, color, size automatically
# Change point shape by the level of cyl
b + geom_point(aes(shape = factor(cyl)))

b + geom_point(aes(color = cyl, shape=factor(cyl)))

b + geom_point(aes(color = cyl, shape=factor(cyl), size = factor(cyl))) + scale_size_manual(values = c(2,3,4))

b + geom_point(aes(color = cyl, shape = cyl)) + scale_shape_manual(values = c(3,16,17)) + scale_color_manual(values = c('#999999','#E69F00', '#56B4E9'))

b + geom_point(aes(color = cyl, shape = factor(cyl))) + scale_color_brewer(palette = "Dark2") + theme_minimal()

b + geom_point(aes(color = cyl, shape = factor(cyl))) + scale_color_grey() + theme_minimal()

b + geom_point() + geom_smooth(method = lm)

# Point + regression line
# Remove the confidence interval
b + geom_point() + geom_smooth(method = lm, se = FALSE)

# loess method, local regression fitting
b + geom_point() + geom_smooth()

# Change the color and shape by groups 吧
b + geom_point(aes(color = cyl, shape = factor(cyl))) + geom_smooth(aes(color = cyl, fill = cyl), method = lm)


# Remove confidence intervals
# Extend the regression lines: fullrage
b + geom_point(aes(color = cyl, shape = factor(cyl))) + geom_smooth(aes(color = cyl), method = lm, se = FALSE, fullrange = TRUE)

# Add marginal rugs to a scatter plot
#geom_rug(sides = "bl")
# sides: a string, "trbl", top, right, bottom, left.
# Add marginal rugs
b + geom_point() + geom_rug()

# Change the color by group
b + geom_point(aes(color = cyl)) + geom_rug(aes(color = cyl))

data(faithful)
faithful <- as_data_frame(faithful)
faithful

ggplot(faithful, aes(x = eruptions, y = waiting)) + geom_point() + geom_rug()

# Jitter points to reduce overplotting
# geom_jitter(), position_jitter()
#alpha, color, fill, shape, size

# Use mpg data
p <- ggplot(mpg, aes(displ, hwy))

# Default sactter plot
p + geom_point()

# Use jitter to reduce overplotting
p + geom_jitter(position = position_jitter(width = 0.5, height = 0.5))

select(mpg, displ, hwy) %>% arrange(-hwy) %>% filter(displ == 1.9)

b + geom_text(aes(label = rownames(mtcars)), size = 3)

data("diamonds")
c <- ggplot(diamonds, aes(carat, price))
# Add heatmap of 2d bin counts
# geom_bin2d produce a scatter plot with rectangular bins.
# stat_bin_2d(), stat_summary_2d()
# max, xmin, ymax, ymin, alpha, color, fill, linetype, size
c + geom_bin2d()

# Change the number of bins
c + geom_bin2d(bins = 15)

# Specify the width of bins
c + geom_bin2d(binwidth = c(1,1000))

c + stat_bin_2d()

c + stat_summary_2d(aes(z = depth))

install.packages("hexbin")
library(hexbin)

c + geom_hex()

# Change the number of bins
c + geom_hex(bins = 10)

c + stat_bin_hex()

c + stat_summary_hex(aes(z = depth))

sp <- ggplot(faithful, aes(x = eruptions, y = waiting))
select(faithful, eruptions, waiting)

# Default plot
sp + geom_density_2d(color = "#E7B800")

# Add points
sp + geom_point(color = "#00AFBB") + geom_density_2d(color = "#E7B800")

# Use stat_density_2d with geom = "polygon"
sp + geom_point() + stat_density_2d(aes(fill = ..level..), geom = "polygon")

# Change the gradient color
sp + geom_point() + stat_density_2d(aes(fill = ..level..), geom = "polygon") + scale_fill_gradient(low = "#00AFBB", high = "#FC3E07")

ggplot(diamonds, aes(cut, color)) + geom_jitter(aes(color = cut), size = 0.5)

select(diamonds, cut, color)

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
ToothGrowth <- as_data_frame(ToothGrowth)
ToothGrowth

e <- ggplot(ToothGrowth, aes(x = dose, y = len))

geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 2, notch = FALSE)

# Basic box plot
e + geom_boxplot()

# Rotate the box plot
e + geom_boxplot() + coord_flip()

# Notched box plot
e + geom_boxplot(notch = TRUE)

# Box plot with mean points
e + geom_boxplot() + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 4, color = "blue")

# chose which item to display
e + geom_boxplot() + scale_x_discrete(limits = c("0.5", "2"))

# change default order of items
e + geom_boxplot() + scale_x_discrete(limits = c("2", "0.5", "1"))

e + stat_boxplot(coeff = 1.5)

e + geom_boxplot(color = "black", fill = "steelblue")

# Change outline colors by dose (groups)
e + geom_boxplot(aes(color = dose))

# Change the fill color by dose (groups)
e + geom_boxplot(aes(fill = dose))

# Change munually outline colors:
# Use custom color palettes
e2 <- e + geom_boxplot(aes(color = dose)) + theme_minimal()
e2 + scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))

# Use brewer color palettes
e2 + scale_color_brewer(palette  = "Dark2")

# Use grey scale
e2 + scale_color_grey()

## Change manually by fill color
# Use the custom color palettes
e3 <- e + geom_boxplot(aes(fill = dose)) + theme_minimal()
e3 + scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9"))

# Use brewer color palettes
e3 + scale_fill_brewer(palette = "Dark2")

# Use grey color
e3 + scale_fill_grey()

## Boxplot with multiple groups
#The grouping variable *dose* and *supp* are used:

# Change box plot colors by groups
e + geom_boxplot(aes(fill = supp))

# Change the position
e + geom_boxplot(aes(fill = supp), position = position_dodge(1.1))

# Change the fill color
e + geom_boxplot(aes(fill = supp), position = position_dodge(1.1)) + scale_fill_brewer("BrBG")

# Basic plot
e + geom_violin()

# Rotate the violin plot
e + geom_violin() + coord_flip()

# Set trim argument to FALSE
e + geom_violin(trim = FALSE, fill = "steelblue")

# Add mean and median points: use fun.y = mean or fun.y = median
e + geom_violin(trim = FALSE) + stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2, color = "blue")

# Add mean points +/- SD
# Use geom = "pointrange" or geom = "crossbar"
library("Hmisc") ## stat_summary
e + geom_violin(trim = FALSE) + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "red")

# Combine with box plot to add median and quartiles
e + geom_violin(trim = FALSE) + geom_boxplot(width = 0.2)

# Change the outline colors by dose (groups)
e + geom_violin(aes(color = dose), trim = FALSE)

# Change the fill color by dose
e  + geom_violin(aes(fill = dose), trim = FALSE)

# Change outline and fill color manually.
e2 <- e + geom_violin(aes(color = dose), trim = FALSE) + theme_minimal()
e2 + scale_color_brewer(palette = "Dark2")

# Change manually fill colors
e3 <- e + geom_violin(aes(fill = dose), trim = FALSE) + theme_minimal()
e3 + scale_fill_brewer(palette = "Dark2")

## Violin plot with multiple groups
# Change the color by groups
e + geom_violin(aes(fill = supp), trim = FALSE)

# Change fill colors
e + geom_violin(aes(fill = supp), trim = FALSE) + scale_fill_brewer(palette = "Dark2")

#Basic dot plot
e + geom_dotplot(binaxis ="y", stackdir = "center")

# Change dotsize and stack ratio
e + geom_dotplot(binaxis = "y", stackdir = "center", stackratio = 1.5, dotsize = 1.1)

# Add mean and median points: use fun.y = mean or fun.y = median
e + geom_dotplot(binaxis = "y", stackdir = "center") + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "red")

# Add mean points with +/- SD
# Use geom = "pointrange" or geom = "crossbar"
e + geom_dotplot(binaxis = "y", stackdir = "center") + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "red")

## Combine with box plot and dot plot:
# Combine with boxplot
e + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir ="center")

# Combine with violin plot
e + geom_violin(trim = FALSE) + geom_dotplot(binaxis = "y", stackdir ="center")

# Dotplot + violin plot + stat summary
e + geom_violin(trim = FALSE) + geom_dotplot(binaxis = "y", stackdir ="center") + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "red", shape = 11)

e + geom_dotplot(binaxis = "y", stackdir = "center", aes(color = dose), fill = "white") + theme_minimal()

e <- ggplot(ToothGrowth, aes(x = dose, y = len))
e + geom_jitter()

# Change the position
# 0.2 is the degree of jitter in x direction
e + geom_jitter(position = position_jitter(0.2))

# Change point shapes and size
e  + geom_boxplot()+ geom_jitter(position = position_jitter(0.2), shape = 11, size = 1.2)

e + geom_jitter(position = position_jitter(0.2)) + stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "red")

# use geom = "pointrange"
e + geom_jitter(position = position_jitter(0.2)) + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), shape = 18, color = "red")

# Combine with boxplot and violin plot
e + geom_violin(trim = FALSE) + geom_jitter(position = position_jitter(0.1)) + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), shape = 18, color = "red")

# Change point shape by group
e + geom_jitter(aes(shape = dose), position = position_jitter(0.2)) + scale_shape_manual(values = c(1,17,19))

# Change color by groups
e + geom_jitter(aes(color = dose, shape = dose), position = position_jitter(0.2)) + theme_minimal()

#Change colors and shapes by groups
e + geom_jitter(aes(color = supp, shape = supp), position = position_jitter(0.2))

# Add boxplot
e + geom_boxplot(aes(color = supp), position = position_dodge()) + geom_jitter(aes(color = supp, shape = supp), position = position_jitter(0.2)) + theme_minimal()

df <- data.frame(dose = c("D0.5", "D1", "D2"), len = c(4.2,10, 29.5))
df2 <- data.frame(supp = rep(c("VC", "OJ"), each = 3), dose = rep(c("D0.5", "D1", "D2"),2 ), len = c(6.8, 15, 33, 4.2, 10, 29.5))

p<- ggplot(data = df, aes(x = dose, y = len, group = 1))
p + geom_line() + geom_point()

# Change the line color and line type
p + geom_line(linetype = "dashed", color = "steelblue") + geom_point(color = "steelblue")

# use geom_step()
p + geom_step() + geom_point()

# use paht
p + geom_path() 

# Line plot with multiple groups
# line tpye and point shape automatically controlled by groups.
p <- ggplot(df2, aes(x = dose, y= len, group = supp))
p + geom_line(aes(linetype = supp)) + geom_point(aes(shape = supp))

# Change the line type, point shapes and colors
p + geom_line(aes(linetype = supp, color = supp)) + geom_point(aes(shape = supp, color = supp)) + scale_color_brewer(palette = "Dark2")

# X-axis is date; use economics
head(economics)

ggplot(data = economics, aes(x = date, y = pop)) + geom_line()

# subset data
ss <- subset(economics, date > as.Date("2006-1-1"))
ggplot(data = ss, aes(x = date, y = pop)) + geom_line()

# line size
ggplot(data = economics, aes(x = date, y = pop, size = unemploy/ pop)) + geom_line()

# multiple time series data:
# Solution 1
ggplot(economics, aes(x = date)) + geom_line(aes(y = psavert, color = "darkred")) + geom_line(aes(y = uempmed), color = "steelblue", linetype = "twodash") + theme_minimal()


# Solution 2: melt by date

# Area plot
ggplot(economics, aes(x = date)) + geom_area(aes(y = psavert), fill = "#999999", color = "#999999", alpha = 0.5) + geom_area(aes(y = uempmed), fill = "#E69F00", color = "#E69F00", alpha = 0.5) + theme_minimal()

df <- data.frame(dose = c("D0.5", "D1", "D2"), len = c(4.2,10, 29.5))
df2 <- data.frame(supp = rep(c("VC", "OJ"), each = 3), dose = rep(c("D0.5", "D1", "D2"),2 ), len = c(6.8, 15, 33, 4.2, 10, 29.5))

f <- ggplot(df, aes(x = dose, y = len))

f + geom_bar(stat = "identity")

#Change fill color and add labels at the top
f + geom_bar(stat= "identity", fill = "steelblue") + geom_text(aes(label = len), vjust = -0.3, size = 3.5) + theme_minimal()

f + geom_bar(stat= "identity", fill = "steelblue") + geom_text(aes(label = len), vjust = 1.6, size = 3.5, color = "white") + theme_minimal() + scale_x_discrete(limits = c("D2", "D0.5", "D1"))

# change the color by groups
f + geom_bar(aes(color = dose), stat = "identity", fill = "white")

#bar plot with multiple groups
g <- ggplot(data =df2, aes(x = dose, y = len, fill = supp))

# Statcked bar plot
g + geom_bar(stat = "identity")

# Use position = position_dodge()
g + geom_bar(stat = "identity", position = position_dodge()) + geom_text(aes(label = len), vjust = 1.6, color = "white", position = position_dodge(0.9), size = 3.5)

library(dplyr)
library(plyr)

df_sorted <- arrange(df2, dose, supp)
df_cumsum <- ddply(df_sorted, "dose", transform, label_ypos = cumsum(len))

# Create the bar plot
ggplot(data = df_cumsum, aes(x = dose, y = len, fill = supp)) + geom_bar(stat = "identity") + geom_text(aes(label = len, y = label_ypos), vjust = 1.6, color = "white", size = 3.5)



#ToothGrowth$dose <- as.factor(ToothGrowth$dose)
df <- ToothGrowth
attach(df)
# Compute mean and standard deviation
df2 <- ToothGrowth %>% group_by(dose) %>% summarize(sd = sd(len), len = mean(len))
attach(df2)
# Create plot
f <- ggplot(df2, aes(x = dose, y = len, ymin = len-sd, ymax = len + sd))
# geom_crossbar() 
# geom_errorbar()
# geom_errorbarh()
# geom_linerange()
# geom_pointrange()

f + geom_crossbar(aes(color = dose)) + scale_color_manual(values = c('#999999', "#E69F00", "#56B4E9")) + theme_minimal()


# Cross bar with multiple groups
df3 <- ToothGrowth %>% group_by(supp, dose) %>% summarise(sd = sd(len), len = mean(len))
f <- ggplot(df3, aes(x = dose, y = len, ymin = len-sd, ymax = len + sd))
f + geom_crossbar(aes(color = supp))
f + geom_crossbar(aes(color = supp), position = position_dodge(1))

# use summary
f <- ggplot(ToothGrowth, aes(x = dose, y = len, color = supp))

f + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "crossbar", width = 0.6, position = position_dodge(0.8))


## Error bar
f <- ggplot(df2, aes(x = dose, y = len, ymin = len - sd, ymax = len + sd))
f + geom_errorbar(aes(color = dose), width = 0.2)

# combine with line plot
f + geom_line(aes(group = 1)) + geom_errorbar(width = 0.2)

# combine with bar error, color by groups
f + geom_bar(aes(color = dose), stat = "identity", fill = "white") + geom_errorbar(aes(color = dose), width = 0.2)

# Keep only upper error bars
f + geom_bar(aes(color = dose), stat = "identity", fill = "white") + geom_errorbar(aes(color = dose, ymin = len), width = 0.2)

# error bar with multiple groups
f <- ggplot(df3, aes(x = dose, y = len, ymin = len - sd, ymax = len + sd))

# bar plot with error bar
f + geom_bar(aes(fill = supp), stat = "identity", position = "dodge") + geom_errorbar(aes(color = supp), position = "dodge")

# line plot with error bar
f + geom_line(aes(group = supp, color = supp)) + geom_point(aes(color = supp)) + geom_errorbar(aes(color = supp), width = 0.2, position = position_dodge(0.05))


# Horizontal error bar
f <- ggplot(df2, aes(x = len, y = dose, xmin = len - sd, xmax = len + sd))


# Interval represented by a vertical line
# geom_linerange()
# geom_pointrange()
f <- ggplot(df2, aes(x = dose, y = len, ymin = len - sd, ymax = len + sd))
# line range
f + geom_linerange()
# point range
f + geom_pointrange()

# plot dot plot and error bars.
g <- ggplot(df, aes(x = dose, y = len)) + geom_dotplot(binaxis = 'y', stackdir = 'center')
# use geom_crossbar
g + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "crossbar", width = 0.5)
# use geom_errorbar
g + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar",color = "red", width = 0.2) + stat_summary(fun.y = mean, geom = "point", color = "red")
# use geom_pointrange()
g + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "red")

df <- data.frame(group = c("Male", "Female", "Child"), value = c(25,25,50))

# create a pie chart
p <- ggplot(df, aes(x = "", y = value, fill = group)) + geom_bar(width = 1, stat = 'identity') + coord_polar("y", start = 0)
print(p)


p + scale_fill_brewer(palette = "Dark2")
# customized pie charts
blank_theme <- theme_minimal() + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(size = 14, face = "bold")
)
library(scales)

p+ scale_fill_brewer("Blues") + blank_theme + geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]),label = percent(value/100)), size = 5)

library(maps)
china = map_data('world2', region = "China")
ggplot(china, aes(x = long, y = lat, group = group)) + geom_polygon(fill = 'white', color = 'black')

  # WTF, why Taiwan is outside of china??
  
h <- ggplot(economics, aes(date, unemploy))
h + geom_path(size = 0.8, color = "#E46726") + theme_minimal()

# combine path, ribbon and rectangle
h + geom_rect(aes(xmin = as.Date('1980-01-01'), ymin = -Inf, xmax = as.Date('1985-01-01'), ymax = Inf), fill = "#A29B32", color = "#D8DA9E", size = 1.5) + geom_ribbon(aes(ymin = unemploy - 900, ymax = unemploy + 900), fill = "#F3BF94") + geom_path(size = 0.8, color = "#E46726") + theme_minimal()

# Add line segments and curve between points (x1, y1) and (x2, y2):
  # Create scatter plot
  i <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
# Add segment
i + geom_segment(aes(x = 2, y = 15, xend = 3, yend = 15))

library(grid)
i + geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25), arrow = arrow(length = unit(0.5, "cm")))

# Add curves
i + geom_curve(aes(x = 2, y = 15, xend =3, yend = 15))

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
p <- ggplot(ToothGrowth, aes(x = dose, y = len, fill = dose)) + geom_boxplot()

# Change title and axis labels
p <- p + labs(title = "Plot of length \n by dose", x = "Dose (mg)", y = "Teeth length")
print(p)

# Change the appearance of labels.
# values for face are one of 'plain', 'italic', 'bold' and 'bold.italic'
p + theme(
  plot.title = element_text(color = 'red', size = 12, face = 'bold.italic'),
  axis.title.x = element_text(color = 'blue', size = 12, face = 'bold'),
  axis.title.y = element_text(color = '#993333', size = 12, face = 'bold')
)

# Hide labels
p + theme(
  plot.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)

# Change the legend title
# use labs and scale functions to update the legend
p + labs(fill = "Dose (mg)")

# Legen position and Appearance
# Convert the variable dose form numeric to factor variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
p <- ggplot(ToothGrowth, aes(x = dose, y = len, fill = dose)) + geom_boxplot()

# Change the legend positions: 'left', 'right', 'top', 'bottom', 'none'
p + theme(legend.position = 'top')

# Legend position as numeric vector c(x,y)
p + theme(legend.position = c(0.8, 0.2))

# Remove the legend 
p + theme(legend.position = 'none')

# Change the appearance of legend title and labels
p + theme(legend.title = element_text(color = 'blue'),legend.text = element_text(color = 'red'))

# Change the legend box background color
p + theme(legend.background = element_rect(fill = 'lightblue'))

# Change the order of legend items
p + scale_x_discrete(limits = c("2", "0.5", "1"))

# Set legend title and labels
p + scale_fill_discrete(name = "Dose", labels = c("A", "B", "C"))

# gyudes(): set or remove the legend for a specific aestheic
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
# Plot with multiple aestheics
p <- ggplot(data = mtcars, aes(x = mpg, y = wt, color = cyl, size = qsec, shape = gear)) + geom_point()
print(p)

# Change the order of  guides using guide_legend()
p + guides(color = guide_legend(order = 1),
           size = guide_legend(order = 2),
           shape = guide_legend(order = 3)
)

# Remove a legend for a particular aestheic(color and size)
p + guides(color = F, size = F)

library(wesanderson)
# Convert dose and cyl columns from numeric to factor variables
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
mtcars$cyl <- as.factor(mtcars$cyl)
# Box plot 
bp <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot(aes(fill = dose))
# Scatter plot 
sp <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(aes(color = cyl))

bp + scale_fill_manual(values = wes_palette(n = 3, name = "GrandBudapest"))

sp + scale_color_manual(values = wes_palette(n = 3, name = "Darjeeling"))

# Gradient between two colors
# The graphs are colored using the qsec continuouse variable:
sp2 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(aes(color = qsec))
print(sp2)

# Change the low and high colors
# Sequnetial color scheme
sp2 + scale_color_gradient(low = "blue", high = "red")

# Diverging color scheme
mid <- mean(mtcars$qsec)
sp2 + scale_color_gradient2(midpoint = mid, low = 'blue', mid = "white", high = "red", space = "Lab")

# Load data and convert dose to a factor variable 
data("ToothGrowth") 
ToothGrowth$dose <- as.factor(ToothGrowth$dose) 
# Box plot 
p <- ggplot(ToothGrowth, aes(x=dose, y=len, group=dose)) + geom_boxplot(aes(fill=dose))
print(p)

p + facet_grid(supp ~ .)

p + facet_grid(.~ supp)

p + facet_grid(dose ~ supp)

library(cowplot)
library(gridExtra)

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

data("economics")
data("diamonds")
my3cols <- c("#E7B800", "#2E9FDF", "#FC4E07")

p <- ggplot(ToothGrowth, aes(x = dose, y = len))

# boxplot
bxp <- p + geom_boxplot(aes(color = dose)) + scale_color_manual(values = my3cols)

print(bxp)

# Dot plot(dp)
dp <- p + geom_dotplot(aes(color = dose, fill = dose), binaxis = 'y', stackdir = 'center') + scale_color_manual(values = my3cols) + scale_fill_manual(values = my3cols)
print(dp)

# line plot
lp <- ggplot(economics, aes(x = date, y = psavert)) + geom_line(color = '#E46726')
print(lp)

# Add gridlines
bxp + background_grid(major = 'xy', minor = 'none')

# Use theme_gray()
bxp + theme_gray()

plot_grid(bxp,dp,lp, labels = c('A','B','C'), ncol=2, nrow = 2)

ggdraw() + 
  draw_plot(bxp, x = 0, y = 0.5, width = 0.5 , height = 0.5) +
  draw_plot(dp, x = 0.5, y = 0.5, width = 0.5, height = 0.5) + 
  draw_plot(lp, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot_label(label = c("A","B","C"), x= c(0, 0.5, 0), y = c(1,1,0.5), size = 15)

# Save multi-figure plots
plot2by2 <- plot_grid(bxp,dp,lp, labels = c('A','B','C'), ncol=2, nrow = 2)

library(gridExtra)
my5cols <- c("#6D9EC1", "#646567", "#A29B32", "#E46726", "#F3BF94")

# create a bar plot
data("diamonds")
brp <- ggplot(diamonds, aes(x = clarity)) + geom_bar(aes(fill = cut)) + scale_fill_manual(values = my5cols)

grid.arrange(bxp, dp, lp, brp, ncol = 2, nrow = 2)

grid.arrange(bxp, arrangeGrob(dp, brp), ncol = 2)

grid.arrange(brp, bxp, dp, ncol = 2, nrow = 2, layout_matrix = rbind(c(1,1),c(2,3)))

# Use common legend for multiple graphs.
# 1. Get plot legend
get_legend <- function(myggplot){ 
  require(gridExtra) 
  tmp <- ggplot_gtable(ggplot_build(myggplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
}

# 2. Save the legend from the dot plot
legend <- get_legend(dp)

# 3. Remove the legend from the box plot and the dot plot
bxp2 <- bxp + theme(legend.position = "none")
dp2 <- dp + theme(legend.position = "none")

# 4. Arrange bxp2, dp2 and the legend with a specific width
grid.arrange(bxp2, dp2, legend, ncol = 3, widths = c(2.3,2.3,0.8))

set.seed(1234)
x <- c(rnorm(350, mean = -1), rnorm(350, mean = 1.5), rnorm(350, mean = 4))
y <- c(rnorm(350, mean = -0.5), rnorm(350, mean = 1.7), rnorm(350, mean = 2.5))
group <- as.factor(rep(c(1, 2, 3), each = 350))
df2 <- data.frame(x, y, group) 
head(df2)

library(gridExtra)
# Scatter plot of x and y variables and color by groups 
scatterPlot <- ggplot(df2, aes(x, y)) + geom_point(aes(color = group)) + scale_color_manual(values = my3cols) + theme(legend.position=c(0,1), legend.justification=c(0,1))
# Marginal density plot of x (top panel) 
xdensity <- ggplot(df2, aes(x)) + geom_density(aes(fill = group), alpha=.8) + scale_fill_manual(values = my3cols) + theme(legend.position = "none")
# Marginal density plot of y (right panel) 
ydensity <- ggplot(df2, aes(y)) + geom_density(aes(fill=group), alpha=.8) + scale_fill_manual(values = my3cols) + theme(legend.position = "none") + coord_flip()

blankPlot <- ggplot()+geom_blank()+ theme_void()

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

library(ggExtra)
ggMarginal(scatterPlot)

# Marginal histogram plot
ggMarginal(scatterPlot, type = "histogram", fill = "#6D9EC1", color= "#BFD5E3")


# Create a transparent theme object
transparent_theme <- theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), axis.line = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA))

p1 <- scatterPlot
# box plot of the x variable
p2 <- ggplot(df2, aes(factor(1),x)) + geom_boxplot(width = 0.3) + coord_flip() + transparent_theme
# box plot of the y variable
p3 <- ggplot(df2, aes(factor(1), y)) + geom_boxplot(width = 0.3) + transparent_theme

# Create the external graphical elements
# called a 'grop' in Grid terminology
p2_grob = ggplotGrob(p2)
p3_grob = ggplotGrob(p3)

# Insert p2_grob inside the scatter plot
xmin <- min(x); xmax <- max(x)
ymin <- min(y); ymax <- max(y)
p1 + annotation_custom(grob = p2_grob, xmin = xmin, xmax = xmax, ymin = ymin - 1.5, ymax = ymin + 1.5)

# Insert p3_grob inside the scatter plot
p1 + annotation_custom(grob = p3_grob, xmin = xmin - 1.5, xmax = xmin + 1.5, ymin = ymin, ymax = ymax)

library(GGally)
library(ggcorrplot)
mydata <- mtcars[, c(1,3,4,5,6,7)]
ggcorr(mydata, palette = "Set3", label = TRUE)

# Matrix of scatter plot
ggpairs(mydata)

mydata <- mtcars[, c(1,3,4,5,6,7)]
corr <- round(cor(mydata),1)
head(corr[,1:6],3)

p.mat <- cor_pmat(mydata)
head(p.mat[,1:4],3)

ggcorrplot(corr)

# reordering the correlation matrix
# use hierarchical clustering
ggcorrplot(corr, hc.order = T, outline.col = "white")

# Types of correlogram layout and customization
# Add correlation coefficients
ggcorrplot(corr, hc.order = T, 
           type = 'lower',
           outline.color = 'white',
           ggtheme = ggplot2::theme_bw,
           colors = c('#6D9EC1', 'white', '#E46726'),
           lab = T)

# Add correlation significance level
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr, hc.order = T, type = 'lower', p.mat = p.mat)

library(survival)
data(lung, package = 'survival')
# - time: Survival time in days.
# - status: censoring status 1 = censored, 2 = dead
# - sex: Male = 1; Female = 2
fit = survfit(Surv(time, status) ~ sex, data = lung)

library(survminer)

# ggsurvplot: drawing survival curves
ggsurvplot(fit)

# customize survival curves
ggsurvplot(fit, size = 1, # change line size 
           palette = c("#E7B800", "#2E9FDF"), # custom color palette 
           conf.int = TRUE, # Add confidence interval 
           pval = TRUE, # Add p-value 
           risk.table = TRUE, # Add risk table 
           risk.table.col = "strata", # Risk table color by groups 
           ggtheme = theme_bw() # Change ggplot2 theme 
)
