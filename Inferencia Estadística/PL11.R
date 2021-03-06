### ANOVA
rm(list=ls())
## Los datos prodeden de 10 observaciones en cuatro grupos diferentes
cont<-c( 4.13, 4.07, 4.04, 4.07, 4.05, 4.04, 4.02, 4.06 ,4.10, 4.04,3.86 ,3.85, 4.08, 4.11, 4.08, 4.01, 4.02,
4.04, 3.97, 3.95, 4.00, 4.02, 4.01, 4.01, 4.04, 3.99, 4.03, 3.97, 3.98, 3.98, 3.88, 3.88, 3.91, 3.95, 3.92, 3.97,
3.92, 3.90, 3.97 ,3.90)
lab<-rep(c(1,2,3,4), each=10)
LAB<-data.frame(lab,cont)
summary(LAB)
library(Rcmdr)
library(abind, pos=14)
library(e1071, pos=15)
numSummary(LAB[,"cont"], groups=LAB$lab, statistics=c("mean", "sd", "IQR", "quantiles"),
 quantiles=c(0,.25,.5,.75,1))
ngrupos<- length(table(LAB$lab)); ngrupos
par(mfrow=c(2,2)) # matriz de graficos 2 * 2
for (j in (1:ngrupos)) {
 titulo<- paste('lab', as.character(j)) ## titulo de cada grafico
 with(LAB, qqPlot(cont[which(lab==j)], dist="norm", ylab='cont', main= titulo) )
 }
par(mfrow=c(1,1))# volver la ventana gráfica a la forma usual
## Test de normalidad. Libreria nortest
install.packages("nortest")
library(nortest)
lillie.test(LAB$cont[which(lab==1)]) ## test de Lilliefors en el laboratorio 1
by(LAB[, 2], LAB[, 1], lillie.test ) ## test de Lilliefors para cada laboratorio
normalityTest(cont ~ lab, test="lillie.test", data=LAB)###por menu en R comander Windows
## Test de homogeneidad de varianzas
with(LAB, tapply(cont, lab, var, na.rm=TRUE)) #3 varianzas de cada laboratorio
bartlett.test(cont ~ lab, data=LAB)
bartlett.test(cont ~ lab, data=LAB[which(lab != '2'),]) ## test de bartlett sin el laboratorio 2
AnovaModel.1 <- aov(cont ~ lab, data=LAB[which(lab != '2'),])
summary(AnovaModel.1)
lista_2<- which(LAB$lab !=2); lista_2
AnovaModel.1 <- aov(cont ~ lab, data=LAB, subset= lista_2)
summary(AnovaModel.1)
####otra forma
LAB2 <- subset(LAB, subset= lab!="2") ## subset= debe ser una comparacion logica
AnovaModel.3 <- aov(cont ~ lab, data=LAB2)
summary(AnovaModel.3)
LAB$lab<- factor(LAB$lab) ## Para hacer los contrastes a posteiori usando "glht" se necesita que este como factor
library(MASS); library(multcomp);
with(LAB2, numSummary(cont, groups=lab, statistics=c("mean", "sd")))
local ({
 .Pairs <- glht(AnovaModel.3, linfct = mcp(lab = "Tukey"))
 print(summary(.Pairs)) # pairwise tests
 print(confint(.Pairs)) # confidence intervals
 print(cld(.Pairs)) # compact letter display
 old.oma <- par(oma=c(0,5,0,0))
 plot(confint(.Pairs))
 par(old.oma)
 })
## Comparaciones de los laboratorios 1,3,4 con el 2.
for (j in c(1,3,4)) print(t.test(LAB[which(lab== "2"), 2], LAB[which(lab== as.character(j)), 2]))
## t.test para contrastar si la media=4 en cada laboratorio
by(LAB[, 2]-4, LAB[, 1], t.test ) ## usando by para las cuatro comparaciones
for (j in (1:ngrupos)) print(t.test(LAB[which(lab== as.character(j)), 2], mu=4)) ## usando un bucle
