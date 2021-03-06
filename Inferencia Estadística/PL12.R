rm(list=ls())
## ajuste a una normal de los siguientes datos
x<-c(10.39, 10.66, 10.12, 10.32, 10.25, 10.91, 10.52, 10.83, 10.72, 10.28, 10.35, 10.46,
 10.72, 10.23, 10.18, 10.62, 10.49, 10.32, 10.61, 10.64, 10.23, 10.29, 10.78, 10.81,
 10.34, 10.62, 10.75, 10.34, 10.41, 10.81, 10.64, 10.53, 10.31, 10.46, 10.47, 10.43, 10.54, 10.39,
 10.57)
n<-length(x);n; mean(x);sd(x)
L<-data.frame(x)
ks.test(x,pnorm,10.5,0.1)#test de Kolmogorov H0: X es N(10.5,.1)
##corrección de los empates
table(x)
kmo<-replicate( 100,{e<-runif(n,-0.005,0.005);y<-x+e;s<-ks.test(y,pnorm,10.5,0.1);s[[2]]})
min(kmo);max(kmo)
library(nortest)### necesaria para test de normalidad en consola
lillie.test(x) #test de Lilliefors H0: X es Normal
shapiro.test(x)#test de Shapiro H0: X es Normal
###grafica de la función de distribucion empírica y los dos ajustes
x<-sort(x)
plot.ecdf(x,main='comparacion de funciones de distribución empírica y ajustadas')
## F<-ecdf(x)
##plot(x,F(x),type='l',main='comparacion de funciones de distribución empírica y ajustadas')
lines(x,pnorm(x,10.5,0.1),col=2)
lines(x,pnorm(x,mean(x),sd(x)),col=3)
legend(10.7,0.2, c("Empírica", "N(10.5,0.1)", "N(media,sd)"), text.col=c(1, 2, 3))
#####test sobre la media siendo X normal H0:mu=10.5
t.test(x, alternative='two.sided', mu=10.5, conf.level=.95)
##test sobre la varianza siendo X normal H0:sigma=0.1
echi<-(n-1)*var(x)/0.01;pvalor<-2*min(pchisq(echi,n-1),1-pchisq(echi,n-1))
cat('estadistico chi-2 para varianza=',echi,'sigma=0.1, p-valor:',pvalor,'\n')
###intervalo de confianza para la desviación típica
 a<-qchisq(c(0.025, 0.975),38)
ex<- sqrt((n-1)*var(x)/a);ex
cat('intervalo de confianza para sigma (0.95)= (',ex[2],', ',ex[1],')\n')
#......................................................
rm(list=ls())
###duraci?n del programa
tiempo<-c(12.032,14.848,15.124,16.974,11.147,10.773,12.952,12.303,13.321,10.523,
16.467,10.190,10.960,11.701,15.526,17.161,18.126,16.068,10.245,12.507,
11.298,16.477,20.705,11.006,11.640,15.946,21.643,10.655,13.543,10.624)
n<-length(tiempo);n
summary(tiempo)
boxplot(tiempo)
#estimaciones de los parametros
elanda=1/(mean(tiempo)-min(tiempo))
cat('estimacion de theta=' ,min(tiempo), 'estimacion de landa=',elanda,'\n')
##hip?tesis simple: tiempo ~ exp(0.2)+10
##comparaci?n funci?n de distribuci?n emp?rica y te?rica
landa<-0.2;theta<-10 #parametros
F0<-function(x) (1-exp(-landa*(x-theta)))*(x> theta)# F de distribucion teorica
plot.ecdf(tiempo, main='Comparacion distribucion emp?rica y te?rica')##funcion de distribuci?n emp?rica
lines(sort(tiempo),F0(sort(tiempo)),col=2)
###c?lculo del estad?stico de Dn para la muestra dada
ks.test(tiempo,F0)##empleo de la funci?n programada en R
#ks.test(tiempo-10,'pexp',landa)## otra alternativa
#### ajuste a hip?tesis compuesta
##hip?tesis simple: tiempo ~ exp(0.2)+theta
etheta<-min(tiempo)
F1<- function(x) (1-exp(-landa*(x-etheta)))*(x> etheta)
dnm<-ks.test(tiempo,F1);dnm$statistic
###dnm<-ks.test(tiempo-min(tiempo),'pexp',landa);dnm$statistic[[1]]
###simulaci?n del comportamiento del estadistico de lilliefors
nr<-10000
lill<-replicate (nr,{y<-rexp(n,landa)
 et<-min(y)
 d<-ks.test(y-et,'pexp',landa);d$statistic
 })# nr valores del estadistico de Lilliefors
p_valor<-sum(lill>dnm$statistic)/nr;p_valor
#estadistico KS
kst<-replicate (nr,{y<-rexp(n,0.2)
 d<-ks.test(y,'pexp',0.2);d$statistic
 })# nr valores del estadistico de K-S
##comparaci?n gr?fica de las distribuciones de los estadisticos de KS y Lilliefors
lill<-sort(lill)##estadistico de lilliefors
g<-ecdf(lill)
plot(lill, g(lill),xlim=c(0, .4), type='l', col=2,
main='comparacion estadistico de Lilliefors y KS',xlab='Dn' ,ylab='F')
kst<-sort(kst)##estadistico de ks
g2<-ecdf(kst)
lines(kst, g2(kst))
legend(0.2,0.4, c('KS','Lilliefors'),lty=1,col=c(1,2))
#### test chi-cuadrado
##hip?tesis simple: tiempo ~ exp(0.2)+10
landa<-0.2;theta<-10
##corte de la variable en clases
corte<-qexp((0:6)/6,landa)+10 ##cuantiles de la distribucion bajo Ho
nclases<-length(corte)-1
ni<-table( cut(tiempo,corte));ni #frecuecias absolutas de las clases
cc<- chisq.test(ni,p=rep(1/6,6)); cc
#test chi-cuadrado hip?tesis compuesta: tiempo ~ exp(0.2)+theta
etheta<-min(tiempo); etheta #estimador mv.
corte<-c(0,qexp((1:6)/6,landa)+etheta)##cuantiles de la distribucion bajo Ho
nclases<-length(corte)-1
ni<-table( cut(tiempo,corte));ni
a<-chisq.test(ni,p=rep(1,6)/6);a[[1]]# valor del estad?stico de Pearson
##hay que corregir la distribuci?n porque se estima un par?metro
pvalor<-1-pchisq(a$statistic,4)
cat('p-valor corregido:', pvalor,'\n')