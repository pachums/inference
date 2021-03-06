##### TG 5 #####
##Ejercicio 1 nº clientes:G(p)
## X= Numero de clientes hasta exito
x<-c(4,1,1,1,1,1,3,1,2,3,1,3,1,1,4,1,2,1,6,2,2,2,5,2,1,1,5,2,1,1)
n<-length(x);table(x)
##ajuste a una G(p)
ep<-1/mean(x);ep ## estimacion de p con los datos sin agrupar
y<- cut(x ,c(0,1,2,9)); levels(y)<-c('1','2','>2');ni<-table(y);ni ## datos agrupados en clases
# l<-function(p) ni[1]*log (dgeom(0,p))+ni[2]*log (dgeom(1,p))+ni[3]*log(1-pgeom(1,p))
l<-function(p) (ni[1]+ni[2])*log(p)+(ni[2]+ni[3]*2)*log(1-p) ## log(verosimilitud) con los datos agrupados
a<-optimize(l,c(0,1),maximum=T); ep2<-a[[1]];ep2 #estimacion de p con los datos agrupados
## En R G= Geometrica = numero de fallos antes del exito; G=X-1
t<-chisq.test(as.numeric(ni),p=c(dgeom(0,ep2),dgeom(1,ep2),1-pgeom(1,ep2)))
ls.str(t) ## estructura del objeto t
names(t) ## nombres del contenido del objeto t
t$expected ## frecuencias esperadas
t$p.value<- pchisq(t$statistic,1,lower.tail=F) ## p-valor con los grados de libertad corregidos
cat('estadístico chi-cuadrado= ', t$statistic, '; g. l. = ', 1, '; p-valor= ', t$p.value, '\n' )
## p=0.2
p0<- 0.2
alfa<-0.05
## estimacion del numero de clases= 30 observaciones/5 = 6
corte<-qgeom(c(1:5)/6, p0);corte+1 # aproximadamente seis clases 'equiprobables'
table(cut(x,c(0,corte+1,Inf))); obs<-as.numeric(table(cut(x,c(0,corte+1,Inf))))#frecuencias observadas
p_teoricas0<-c(pgeom(0, p0),pgeom(corte[2:5],p0)-pgeom(corte[1:4],0.2),1-pgeom(corte[5],0.2))
n*p_teoricas0 ## la frecuencia esperada de la categoría (4,5] es muy baja
b<-chisq.test(obs,p= p_teoricas0) ## se puede calcular test exacto
## Se considera una clase menos para verificar las frecuencias esperadas >= 5
corte<-qgeom(c(1:4)/5, p0);corte+1 # cinco clases 'equprobables' aproximadamente
table(cut(x,c(0,corte+1,Inf))); obs<-as.numeric(table(cut(x,c(0,corte+1,Inf))))#frecuencias observadas
p_teoricas0<-c(pgeom(0, p0),pgeom(corte[2:4],p0)-pgeom(corte[1:3],0.2),1-pgeom(corte[4],0.2))
b<-chisq.test(obs,p= p_teoricas0) ## se puede calcular test exacto
b$expected; n* p_teoricas0 ## frecuencias esperadas
b
##APARTADO C
####cálculo de la función potencia
alfa<-0.05
a<-qnbinom(c(alfa/2,1-alfa/2),n,0.2); a+30 ##RC con BN(30,0.2)
c<-qchisq(1-alfa,4);c#límite de la RC para estadistico D
p<-seq(0.01,0.99,length=100)#distintos valores de p
pot<- pnbinom(a[1],30,p) + (1- pnbinom(a[2],30,p))
plot(p,pot,type="l", main="Potencia del test ")

c<-qchisq(1-alfa,4);c#límite de la RC
p<-seq(0.1,0.5, length=50)
pot1<-c() 
for (i in 1:50) {
d1<-replicate(nr,{
m<-rgeom(n,p[i])+1;
obs<-as.numeric(table(cut(m,c(0,corte+1,Inf))))
prob0<-c(pchisq(1,5),pchisq(3,5)-pchisq(1,5),pchisq(5,5)-pchisq(3,5),pchisq(8,5)-pchisq(5,5),1-pchisq(8,5))
bs<-chisq.test(obs,p=prob0)
bs$statistic })
plot[i]<-sum(d1>c)/nr} 
lines(p,pot1,col=2)
legend(.6,.6,c(’exacto,BN’,’simulado,chisq’),col=c(1,2),lty=c(1,1))
##En PDF: ### potencia alfa=0.05; H1: p= p1= 0.1
alfa<- 0.05 ## nivel de significacion
c<-qchisq(1-alfa, 4); c # limite de la RC para la hipotesis nula G(0.2)
p1<-0.1
nr<-10000
 d1<-replicate(nr,{
 m<-rgeom(n,p1)+1; obs<-as.numeric(table(cut(m,c(0,corte+1,1000))))
 prob0<-c(pchisq(1,5),pchisq(3,5)-pchisq(1,5),pchisq(5,5)-pchisq(3,5),pchisq(8,5)-pchisq(5,5),1-pchisq(8,5))
 b<-chisq.test(obs,p=prob0)
 b$statistic })
 cat(' potencia del test para p=0.1 =', sum(d1>c)/nr, '\n')
#### calculo de la funcion potencia
p<-seq(0.1,0.5, length=50)
pot<-c()
for (i in 1:50) {
 d1<-replicate(nr,{
 m<-rgeom(n,p[i])+1;obs<-as.numeric(table(cut(m,c(0,corte+1,1000))))
 bs<-chisq.test(obs,p=prob0)
 bs$statistic })
pot[i]<-sum(d1>c)/nr }
plot(p,pot,type='l')
### EJERCICIO 2 ###
#problema 2 datos de la proporción/dia de piezas defectuosas: P25=0.08
pro<-c(0.14,0.12,0.03,0.15,0.02,0.11,0.11,0.08,0.12,0.23,0.01,0.01,0.09,0.03,0.07,0.02,0.12,0.05,0.11,0.01,0.04,0.12,0.00,0.37,0.18,0.25,0.06)
summary(pro)
boxplot(pro)
n<-length(pro);n
### caso Beta(1,q)
q<-log(0.75)/log(0.92);q #valor de q para que el primer cuartil sea 0.08
#test de K_S
ks.test(pro,'pbeta',1,q)
prom<-pro+runif(n,-0.0005,0.0005)#modificacion para evitar empates
ks.test(prom,'pbeta',1,q)
##test paramétrico 2*q0*(-sum(log(1-pro))) es gamma(n,1/2)=
e<-2*q*(-sum(log(1-pro)));e
pvalor<-2*pchisq(e,2*n);pvalor
icq<-qchisq(c(alfa/2, 1-alfa/2),2*n)/ (-sum(log(1-pro))*2);icq#intervalo de confianza parametrico para q
qgamma(c(0.025, 0.975), n, 1)/(-sum(log(1-pro)))
cat('intervalo de confianza (parametrico): (', 1-0.75^(1/icq[2]),', ', 1-0.75^(1/icq[1]),') \n')
###test de los signos
a<-table(cut(pro,c(-0.1,0.079999,1)));a; a<-as.numeric(table(cut(pro,c(-0.1,0.079999,1))))
binom.test(a,p=0.25,)##Test de los signos
qbinom(c(alfa/2, 1-alfa/2),n,0.25)
pro<-sort(pro)
pbinom(11,n,.25)-pbinom(2,n,.25)
cat('intervalo de confianza (test de los signos): (', pro[2],', ', pro[11],') \n')
