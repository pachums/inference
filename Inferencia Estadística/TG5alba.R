#### ~TG5 ALBA~ ####
x<-c(0.06, 0.02, 0.01, 0.08, 0.09, 0.08, 0.17, 0.15, 0.21, 0.05, 0.14, 0.37, 0.07,0.12, 0.09,0.47, 0.09, 0.06, 0.06, 0.07, 0.24, 0.03, 0.09, 0.02, 0.04, 0.04, 0.05, 0.03,0.27, 0.09, 0.06, 0.18, 0.01, 0.23,0.12)
n<-length(x);n
plot(ecdf(x),main="ecdf") ##representación gráfica de la función de distr. empírica
F0<- function (x) (1-(1- x)^4)*(x>0) ##introducimos F(x)
malla<-seq(0,0.6,0.01) ##secuencia de puntos para “plotear”
lines(malla,F0(malla)) ##representación de la func. de distribución
ks.test(x,F0) ##test de kolmogorov-Smirnov
#APARTADO B
EMV<- -(length(x))/(sum(log(1- x))); EMV
corte<- qbeta((1:6)/7,1,EMV)
corte<-c(0,corte,1); corte
nclases<-length(corte)- 1
ni<-table(cut(x,corte)); ni
a<- chisq.test(ni);a
pval<-1- pchisq(a$statistic,nclases-2)
cat("p-valor corregido:",pval,"\n")
#EJERCICIO 2
Dn<-function (x,F0){
x<-sort(x)
n<-length(x)
e<-(1:n)/n
t<-F0(x)
max(c(e-t,t-(e-1/n)))}
D20<-replicate(1000,{x<-rexp(20,1);
EMV<-1/mean(x); EMV
F0<-function(x) pexp(x,EMV); F0
Dn(x,F0)})
percentiles<-quantile(D20,c(0.9,0.95,0.99,0.999)); percentiles
