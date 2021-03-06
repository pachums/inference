######################## EJERCICIOS TEMA 5 ########################
rm(list=ls())

# EJERCICIO 1 #
x<-c(262,91,86,31)
prob<-c(9,3,3,1)
#prob<-c(9/16,3/16,3/16,1/16) tambien vale
chisq.test(x,prob)
n<-sum(x);n
sum((x-prob*n)^2/(prob*n))

## EJERCICIO 2 ##
tiempo<-c(12.032,14.848,15.124,16.974,11.147,10.773,12.952,12.303,13.321,10.523,
16.467,10.190,10.960,11.701,15.526,17.161,18.126,16.068,10.245,12.507,
11.298,16.477,20.705,11.006,11.640,15.946,21.643,10.655,13.543,10.624)
n<-length(tiempo);n
summary(tiempo)
boxplot(tiempo)
## comparación función de distribución empírica y teórica
landa_0<-0.2;theta_0<-8 #parametros
F0<-function(x) (1-exp(-landa_0*(x-theta_0)))*(x> theta_0)# F de distribucion teorica
plot.ecdf(tiempo, main='Comparacion distribucion empírica y teórica')## funcion de distribución empírica
lines(sort(tiempo),F0(sort(tiempo)),col=2)
### cálculo del estadístico de Dn para la muestra dada y p valor
ks.test(tiempo,F0)## empleo de la función programada en R
#ks.test(tiempo-10,'pexp',landa)## otra alternativa
#### ajuste a hipótesis compuesta
etheta<- min(tiempo) ## variable global
F1<- function(x) (1-exp(-landa_0*(x- etheta)))*(x>= etheta)
dnm<-ks.test(tiempo,F1);dnm$statistic
### simulación del comportamiento del estadistico de lilliefors
nr<-1000
ks2<-replicate (nr,{ # nr valores del estadistico de K-S
 y<-rexp(n,landa_0)
 ## etheta<- min(y) es una variable local (del entorno replicate) y por tanto F1 se calcula con etheta=
min(tiempo)
 etheta<<- min(y) ## con etheta<<- min(y) se hace que F1 se calcule con etheta= min(y)
 d<-ks.test(y,F1);d$statistic
})
p_valor<-sum(ks2>dnm$statistic)/nr;p_valor
## comparación de las distribuciones de los estadisticos de KS y Lilliefors
## estadistico de lilliefors
ks2<-sort(ks2)
g<-ecdf(ks2)
plot(ks2, g(ks2),xlim=c(0, 1), type='l', col=2, main='Estadisticos de Lilliefors y KS',xlab='Dn' ,ylab='F')
# estadistico KS
kse<-replicate (nr,{ # nr valores del estadistico de K-S
 y<-runif(n,0,1)
 d<-ks.test(y,'punif',0,1);
 d$statistic
 })
## kse<-sort(kse); gre<-ecdf(kse); lines(kse, gre(kse))
lines(sort(kse), ecdf(kse)(sort(kse)))
legend(0.6,0.4, c('KS','Lilliefors'),lty=1,col=c(1,2))
#### test chi-cuadrado
## hipótesis simple: tiempo ~ exp(0.2)+10
landa_0<-0.2;theta_0<-10
nc<- floor(n/5); cat('número de clases para frecuencias esperadas >= cinco= ',nc, '\n')
## corte de la variable en clases
corte_0<-qexp((0:nc)/nc,landa_0)+ theta_0 ## cuantiles de la distribucion bajo Ho
corte_0
ni<- table(cut(tiempo,corte_0));ni #frecuecias absolutas de las clases
chi_cuad<- chisq.test(ni,p=rep(1/nc,nc))
chi_cuad
# test chi-cuadrado hipótesis compuesta: tiempo ~ exp(0.2)+theta
etheta<- min(tiempo); etheta # estimador mv.
corte_1<-c(0,qexp((1:nc)/nc, landa_0)+etheta) ## cuantiles de la distribucion bajo Ho
corte_1
ni<-table( cut(tiempo,corte_1));ni
a<-chisq.test(ni,p=rep(1,nc)/nc) ## a[[1]]= estadístico de Pearson;a[[2]]= g. l.; a[[3]]= p-valor
## corregir los grados de libertad de la distribución chi-cuadrado
pvalor<-1-pchisq(a$statistic, nc-2)
cat('Chi-cuadrado= ', a[[1]], '; gl= ', nc-2, '; p-valor corregido:', pvalor, '; p-valor inicial= ', a[[3]],'\n')

#### EJERCICIO 4 ####
v<-c(0:5);v
frec<-c(22,53,58,39,20,8)
n<-sum(frec)
x<-rep(c(0:4,6),frec) #considero 6 el valor >=5
EMV<-mean(x);EMV
g<-function(lambda){-lambda*sum(frec[1:5])+log(lambda)*sum(v[1:5]*frec[1:5])+frec[6]*log(1-ppois(4,lambda))}
z<-seq(EMV-3,EMV+3,.01);plot(z,g(z),type="l")
mediaMV<-optimize(g,c(EMV-3,EMV+3),tol=0.001,maximum=TRUE)
cat('Estimación Máximo Verosimil= ', mediaMV[[1]], '\n')
p<-c(dpois(0:4,EMV),1-ppois(4,EMV))
e<-p*200;e
D<-sum((frec-e)^2/e);D

##### EJERCICIO 5 ##### 
x<-c(1,2,2,3,3,10,11,11,12,14,17,17,18,20,25)
ks.test(x,'pexp',1/12)
n<-length(x)
sa<-replicate(100,{
	e<-runif(n,-0.005,0.005)
	a<-ks.test(x+e,'pexp',1/12)
	a[[2]]})
min(sa);max(sa);
###### EJERCICIO 6 ###### ERROR
ex<-c(0,1,2,3,4,5)
fr<-c(8,6,3,2,0,1)
x<-rep(ex,fr)
n<-sum(fr)
p<-1/mean(x);p
z<-cut(x,c(-1,0,1,10));levels(z)<-c('0','1','>1');z<-table(z);z
as.numeric(z)
prob<-c(dgeom(0,p),dgeom(1,p),1-pgeom(1,p))
chisq.test(as.numeric(z),p=prob,simulate.p.value = TRUE)

####### EJERCICIO 7 #######
ex<-c(0.3,0.45,0.55,0.65,0.75,0.8,0.85,0.9,0.95,1)
fr<-c(5,7,6,7,8,5,5,5,6,6)
n<-sum(fr);n
media<-sum(ex*fr)/n; ae<-media/(1-media)
l<-function(a) sum(fr*(log(pbeta(ex,a,1)-pbeta(c(0,ex[1:9]),a,1))))
op<-optimize(l,c(1,6));op[[1]]
prob<-c(pbeta(ex,ae,1)-pbeta(c(0,ex[1:9]),ae,1));prob
s<-chisq.test(fr,p=prob);s
ls.str(s)
s$expected
######### EJERCICIO 9 #########
x<-c(37,71,45,32,50,41,52,41,54,28,44,54,29,32,52,42,48,48,46,43)
n<-length(x);n
table(x)
summary(x)
boxplot(x)
p1<-ppois(40,EMV)-ppois(28,EMV)
p2<-ppois(48,EMV)-ppois(40,EMV)
p3<-1-(ppois(40,EMV)-ppois(28,EMV)+ppois(48,EMV)-ppois(40,EMV))
e1<-20*p1;e2<-20*p2;e3<-20*p3;
chisq.test(c(5,9,6),p=c(p1,p2,p3))
D<-(5-e1)^2/e1+(9-e2)^2/e2+(6-e3)^2/e3;D

###################################################### EXTRA:
x<-rep(c(0,1,2,3,4,5),c(10,15,15,10,6,4))
n<-length(x)
z<-mean(x)
prob<-c(dpois(0,z),dpois(1,z),dpois(2,z),dpois(3,z),dpois(4,z),1-ppois(4,z))
chisq.test(c(10,15,15,10,6,4),prob)
ks.test(c(10,15,15,10,6,4),'ppois',z)