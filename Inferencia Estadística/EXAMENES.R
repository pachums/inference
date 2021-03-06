########################### EJERCICIOS EXAMENES ###########################
rm(list=ls())
#### MAYO 2012 ####
##EJ1(segun chi-cuadrado, mejor después con lilliefors)
x<-c(12.85,22.72,25.63,1.51,17.02,1.09,0.55,0.22,57.24,20.12,12.12,58.53,14.06,12.87,1.82,15.19,10.00,8.48,3.72,42.95)
n<-length(x);n
EMV<-1/mean(x);EMV
corte<-qexp(c(1:3)/4, EMV,1); corte<-c(0,corte,Inf)
table(cut(x,corte)); obs<-as.numeric(table(cut(x,corte)))
g<-function(lambda)(log(pexp(corte[2],lambda)-pexp(corte[1],lambda))*obs[1]+log(pexp(corte[3],lambda)-pexp(corte[2],lambda))*obs[2]+log(pexp(corte[4],lambda)-pexp(corte[3],lambda))*obs[3]+log(1-pexp(corte[4],lambda))*obs[4])
t<-optimize(g,c(0,1),maximum=T); EMV<-t[[1]];EMV
corte<-qexp(c(1:3)/4, EMV,1); corte<-c(0,corte,Inf)
table(cut(x,corte)); obs<-as.numeric(table(cut(x,corte)))
#las prob. de las clases son todas 0.25
e1<-pexp(corte[2],EMV)-pexp(corte[1],EMV); e2<-pexp(corte[3],EMV)-pexp(corte[2],EMV);e3<-pexp(corte[4],EMV)-pexp(corte[3],EMV);e4<-1-pexp(corte[4],EMV);e1;e2;e3;e4;
a<-chisq.test(obs,p=rep(0.25,4))
pval<-1-pchisq(a$statistic,5-2);pval[[1]]
t<-0:14
D<-function(t)((10*t/n)^n)
plot(t,D(t))
#IMPORTANTE#EJ1 con Lilliefors
a<-rexp(20,1);a #estoy generando Y=e(1)
dn<-ks.test(x,'pexp',1) #RARO: se supone que ks.test con la f.d F1<-function(x)(1-exp(1)^(x/mean(x))) deberia ser lo mismo y no ocurre
dn$statistic[[1]]
nr<-1000
ks2<-replicate(nr,{	a<-rexp(20,1)
			d<-ks.test(a,'pexp',1);d$statistic })
pvalor<-sum(ks2>dn$statistic[[1]])/nr;pvalor
#### MAYO 2015 EJ2 ####
x<-c(2.92,2.01,1.72,2.14,2.02,2.72,1.94,1.50,2.59,2.63,2.56,2.23,2.21,2.50,1.85,2.41,2.94,2.46,2.25,2.64)
n<-length(x);n

#### JUNIO 2016 EJ3 ####
x<-c(12,15.2,20.5,17.9,16.5,17.4,21.1,17.5,12.6,11.4,18.1,11.8,19.6,18.3,17,14,21.4,16.6,17.9,15.6,17.8,14.1,16.2,22.4,12.2,14.4,14,10.4,14.9,14.2)
n<-length(x);n
EMV<-16.296
corte<-qgamma(c(1:5)/6, EMV,1)
table(cut(x,c(0,corte,Inf))); obs<-as.numeric(table(cut(x,c(0,corte,Inf))))#frecuencias observadas
p_teoricas0<-c(pgamma(corte[1],EMV,1),pgamma(corte[2],EMV,1)-pgamma(corte[1],EMV,1),pgamma(corte[3],EMV,1)-pgamma(corte[2],EMV,1),pgamma(corte[4],EMV,1)-pgamma(corte[3],EMV,1),pgamma(corte[5],EMV,1)-pgamma(corte[4],EMV,1),1-pgamma(corte[5],EMV,1))
p_teoricas0*30
a<-chisq.test(obs,p=p_teoricas0);a
pval<-1-pchisq(a$statistic,6-2);pval

#### MAYO 2017 ####
##EJ1
n<-20
g<-function(t)((t/3)^(n/2)*exp(1)^(-0.5*n*t/3+0.5*n))
optimize(g,c(0,12),tol=0.001,maximum=TRUE)
z<-seq(0,12,by=0.01)
plot(z,g(z),type="l")
h2<-qchisq(0.975,20)*3/20;h2
w<-function(t)(g(t)-g(h2))
q<-seq(1.55,1.6,by=0.0001)
w(q)
h1<-q[136];h1
pchisq(h1,20)*3/20+pchisq(h2,20,lower.tail=F)*3/20

#### JUNIO 2017 ####
alto<-x[31:90,];bajo<-x[c(0:30,90:120),]
alto[,2]<median(x[,2])
sum(alto[,2]<median(x[,2]))
bajo[,2]<median(x[,2])
sum(bajo[,2]<median(x[,2]))

#### JUNIO EXTRAORDINARIA 2017 ####
x<-c(2, 2, 2, 1, 1, 1, 2, 1, 2, 3, 0, 1, 2, 1, 2, 2, 1, 0, 2, 2)
n<-length(x);n

################################################ MOVIDAS DE CLASE:
n<-20
g<-function(t)((t*3/n)^n*exp(1)^(-3*t+n))
z<-seq(0,12,by=0.1)
plot(z,g(z),type="l")
t<-optimize(g,c(0,12),maximum=T); EMV<-t[[1]];EMV #n/3
h2<-qgamma(0.975,n,3);h2
g(h2)
#evaluo para obtener el cero
w<-function(t)(g(t)-g(h2))
q<-seq(4.23,4.27,by=0.00001)
w(q)
q<-4.2335715
w(q)
pgamma(q,n,3)+1-pgamma(h2,n,3)
#hago ej.a
g<-function(t)((3*t)^n*exp(1)^(-n*(3*t-1)))
z<-seq(0,2,by=0.01)
plot(z,g(z),type="l")
t<-optimize(g,c(0,12),maximum=T); EMV<-t[[1]];EMV #n/3
h2<-3/n*qgamma(0.975,n,3);
#hago ej.b
n<-20
g<-function(t)((n*0.3/t)^t*(n*0.7/(n-t))^(n-t))
optimize(g,c(0,12),tol=0.001,maximum=TRUE)
z<-seq(0,12,by=0.01)
#a partir de aqui copiado:::::::
plot(z,g(z),type="l")
h2<-qchisq(0.975,20)*3/20;h2
w<-function(t)(g(t)-g(h2))
q<-seq(1.55,1.6,by=0.0001)
w(q)
h1<-q[136];h1
pchisq(h1,20)*3/20+pchisq(h2,20,lower.tail=F)*3/20


