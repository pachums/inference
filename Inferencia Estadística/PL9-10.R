####PL9 y PL10 TRV para una B(1,p)
rm(list=ls())
###TRV bilateral H0: p= 0.6, H1: p no = 0.6
## parametros del problema
n<- 15 # numero de observaciones de la muestra
p0<- .6; ## H0: p= p0
alfa<- .05 ## nivel de significacion
### Razon de Verosimilitudes como funcion de t= Suma(Xi)
D<-function(t) (n*p0/t)^t * (n*(1-p0)/(n-t))^(n-t)# ERV función de t=sum(xi)
t<-0:n #valores de T= sum(Xi)
rv<-D(t);rv # valores del estadistico RV para los distintos valores de t
plot(t, rv,type='l')
points(t, rv, col="red")

##ABRO NUEVO
h2<-qbinom(0.975,15,0.6);h2
w<-function(t)(D(t)-D(h2))
h1<-uniroot(w,c(4,7));h1<-h1$root;h1
a<-sum(dbinom(1:4,15,0.6))+sum(dbinom(13:15,15,0.6))
#sum(pbinom(h1,15,0.6)+1-pbinom(h2,15,0.6)) asi me la lia con <= o <
##CIERRO NUEVO
### calculo de la region critica ###
indice<- order(rv)-1; indice ## > order(c(1,3,4,2)) = 1 4 2 3
pro<-dbinom(indice,n,0.6) # probabilidades de cada valor t bajo Ho
P <- cumsum(pro);P ## probabilidades acumuladas según el valor de la razón de verosimilitudes
RC<-indice[which(cumsum(pro)<alfa)]; RC<-sort(RC) # valores de t que definen la region critica
tama<- sum(dbinom(RC, n, p0)); tama ## tamaño del test
i1<- which(diff(sort(RC))>1);
K1<- sort(RC)[i1];
K2<- sort(RC)[i1+1]-1
cat("RC = {Suma(Xi)< ", K1+1, "; Suma(Xi)> ", K2,"};", " P(RC|H0)= ",
tama,"\n")
## Otra región Crítica
C1<- K1+1; C2<- K2+1 ###
RC1<- c(0:C1, C2:n)
tama1<- sum(dbinom(RC1,n,p0)) ## tamaño del nuevo test
cat("Otra RC = {Suma(Xi)< ", 5, " U ", "Suma(Xi)> ", 13, "};", "
P(RC|H0)= ",tama1,"\n")
## pvalor asociado a t= 4 ###
lista<- which(rv <= D(4))-1; lista ## valores de t con RV(t) <= RV(4)
pvalor<- sum(dbinom(lista, n, p0))
cat("p-valor del TRV para Suma(Xi)= 4 : ", pvalor, "\n")
### potencia de los dos tests ###
p<- seq(.00001, .9999, length=100)
potencia<- pbinom(K1,n,p) +1 - pbinom(K2,n,p)
potencia1<- pbinom(C1,n,p) +1 - pbinom(C2,n,p)
plot(p, potencia, main="Función Potencia del test", type="l")
lines(p, potencia1, col="red")
## pot para p=0.7
pot<-pbinom(K1,n,.7) +1 -pbinom(K2,n,.7)
cat("Potencia del TRV (n=15, p1=0.07) =", pot, "\n")
## calcular el tamaño aproximándolo por una normal
## H0:mu= mu0; mu1= valor de la media, bajo H1, para calcular la potencia
## beta= potencia del test para mu1
## P(|N(0,1)|<z0)=1-alfa; P(N(0,1)< z1)=1-beta
## tamaño de la muestra = [(z1*sigma0 -z0*sigma1)/(mu0-mu1)]^2
beta<- .80; p1<- .70
z0<- qnorm(1-alfa/2,0,1);z1<- qnorm(1-beta,0,1)
s0<- sqrt(p0*(1-p0)); s1<- sqrt(p1*(1-p1))
tm<- ceiling(((z0*s0 - z1*s1)/(p1-p0))^2) ## tamaño aproximado de la muestra
cat("tamaño de muestra. Aprox. Normal= ", tm, "\n")
######función para el calculo de la RC en funcion de n
test<-function (n) {
 Ld<-function(t) t*log(n*p0/t)+(n-t)*log(n*(1-p0)/(n-t))# log RV función de t=sum(xi)
 t<-1:(n-1) #valores de T= sum(Xi)
 rv<-Ld(t) # valores del estadistico logRV para los distintos valores de t
 t<-c(0,t,n); rv<-c(n*log(1-p0),rv,n*log(p0))
 indice<- order(rv)-1
 pro<-cumsum(dbinom(indice,n,p0)) # probabilidades acumuladas t bajo Ho
 RC<-indice[which(pro<alfa)]; sort(RC) # valores de t que definen la region critica
 }
###calculo del tamaño de muestra exacto
pot<-sum(dbinom(test(tm),tm,0.7));pot
while (pot< 0.8) { tm<-tm+1;pot<-sum(dbinom(test(tm),tm,0.7)) }

cat("tamaño de muestra= ", tm, "; potencia= ",pot, ";(p1= ", p1, "; beta= ", beta, ") ", "\n")
##...............................................................................................
##### test unilateral
D1<-function(t) (n*p0/t)^t * (n*(1-p0)/(n-t))^(n-t)*(t>= n*p0)+(t < n*p0)# RV función de t=sum(
t<-0:n # valores de T
rv1<-D1(t) # valores del estadistico RV
plot(t,rv1, main='razon de verosimilitudes')# grafica de la razon de verosimilitudes
indice<-order(rv1)-1 ;indice # valores de T ordenados por la razon de
verosimilitudes
pro<-dbinom(indice,n, p0) # probabilidades de cada valor de la variable
t bajo Ho
P <- cumsum(pro);P
RCu<-indice[which(cumsum(pro)<= alfa)] ## Región crítica unilateral
cat("R. C. unilateral = ",sort(RCu), "\n")
### función potencia RC={sum(Xi)>= K}
p<-seq(0,1,length=100)
pot<-(1-pbinom(min(RCu),15,p))
plot(p,pot,type='l',main=' funcion potencia')
pvalor<-1-pbinom(4,n,0.6);pvalor
pot1<-(1-pbinom(min(RCu),15,.7));pot1