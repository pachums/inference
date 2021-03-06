## Intervalo de confianza del parámetro theta de una variable X con densidad
## f(x, theta) = 4^2 (x-theta) exp(-4(x-theta)) * I(x>theta)
rm(list=ls()) # borrar la memoria de R
muestra<-c(5.72, 6.12, 6.11, 5.41, 6.14, 5.46, 6.07, 5.53, 6.13, 5.54, 5.43,
5.74, 5.96, 6.69, 5.40,5.54, 5.83, 5.58, 5.90, 5.48)
n<-length(muestra)
xmin<-min(muestra);cat ('estimación analógica',xmin,'; Momentos=', mean(muestra)-0.5,'\n')
#estimación máximo verosimil
lv<- function (t) sum(log(muestra-t)) -4* sum(muestra-t)
# lv<- function (t) sum(dgamma(muestra-t,2,4,log=T))
o<-optimize (lv,c(xmin-2,xmin),maximum=T)
cat ('estimación MV= ',o[[1]],'\n')
# intervalo de confianza
alfa<- 0.05 # coeficiente de confianza= 0.95
# intervalo de confianza por simulación de la funcion pivote
 nr<-10000
 miny<-replicate(nr, {min(rgamma(n,shape= 2,rate= 4))}) ## muestra por Montecarlo
 a<-quantile(miny, c(alfa/2,1-alfa/2));a
 cat('IC para theta(simulacion):', c(xmin-a[2],', ', xmin-a[1]), '\n')
##otro intervalo con solo una cola
b<-quantile(miny,1-alfa);b
cat('IC para theta:', xmin-b,', ',xmin, '\n')
###longitud b*xmin=0.168*xmin mas pequeño que el anterior
## intervalo de confianza utilizando la distibución teórica de min(y)= min(gamma(2,4) )
## P(min(y1,...,yn) <= a) = 1- (1-F(a))^n
### gráfica de la densidad del mínimo de la gamma(2,4)
dminimo<- function(y) n*dgamma(y,2,4)* (1-pgamma(y,2,4))^(n-1)
valoresy<- seq(0, 0.5, length=100)
plot(valoresy, dminimo(valoresy), type='l')
## a la vista de la anterior densidad la cola de la derecha es muy larga
##se trabaja con el intervalo mas corto, solo una cola
b<-qgamma(1-alfa^(1/n),rate=4,shape=2);b
cat('IC para theta:', c(xmin-b,', ',xmin), '\n')
# intervalo de confianza sin utilizar la función qgamma
## Y= gamma((2,4); F(y) = 1-(4*y+1)*exp(-4*y)
## P(min(y1,...yn)<= y) = 1-[(4*y+1)*exp(-4*y)]^n
f1<-function(y) (1-(4*y+1)*exp(-4*y)) -(1-(alfa)^(1/n))
a1<-uniroot(f1,c(0,1)); a1[[1]]
cat('IC para theta:', c(xmin-a1[[1]],xmin), '\n')
####otro intervalo basado en la media: media(x1....xn)-theta es gamma(2n,4n)
a<-qgamma(c(alfa/2, 1-alfa/2), 2*n,4*n);a
 cat('IC para theta:', mean(muestra)-a[2], mean(muestra)-a[1], '\n')