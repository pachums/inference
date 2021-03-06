##PL4 Estimacion 

rm(list=ls())#borrado de los datos 
x<-c(0.038, 0.253, 2.336, 2.300, 1.467, 0.540, 1.049, 1.547, 2.165, 3.050, 2.504, 0.882, 0.574, 1.186, 0.426)
n<-length(x)
##grafica de la verosimilitud
l<-function(t) 3^n *prod((t-x)^2)/t^(3*n)*(t>max(x))
pa<-seq(2,max(x)+5,length=200)#valores del parametro
l1<-Vectorize(l) ##argumentos de la funcion son vectores
v<-l1(pa)#valores de la verosimilitud
pa[which.max(v)]##aproximacion de la EMV
 
##plot(pa,v,type='l', main="Funcion de verosimilitud", ylab='Verosimilitud',xlab='Theta')

plot(pa,log(v),type='l', main=" Log Funcion de verosimilitud", ylab='log(L)',xlab='Theta')
#estimaci?n m?ximo verosimil
es<-optimize(l,c(max(x),8),maximum=T);es


####  Puesto que X=theta Y,  Y= beta(1,3);  max(X)=theta max(Y) 
###  max(x)/E(max(y))es un estimador centrado de theta
###como E(Y)= 1/4 entonces   4 mean(x) es un estimador centrado de  theta



espy<-mean(replicate(1000, max(rbeta(n,1,3))));espy###aproximacion de E(max(y)) 

est2<-max(x)/ espy##estimacion basada en el maximo 
cat('Estimaci?n basada en el m?ximo', est2, '\n')
est3<-4*mean(x)##estimacion basada en la media 
cat('Estimaci?n basada en la media', est3, '\n')

###simulacion del  comportamiento de los tres estimadores para un valor del parametro fijo 

theta<-5   #valor te?rico del parametro
nr<-10000
estimaciones<- replicate(nr,{   #matriz de tres filas: la primera las EMV, la segunda max(x)/E(max(y), tercera mean(x)*4
x<-theta*rbeta(n,1,3)###genero datos de x
l<-function(t) 3^n *prod((t-x)^2)/t^(3*n)*(t>max(x))# verosimilitud para la muestra dada
es<-optimize(l,c(max(x),8),maximum=T);est1<-es[[1]] # estimaci?n MV
est2<-max(x)/ espy; est3<-mean(x)*4; c(est1,est2,est3) # las tres estimaciones
})

cat('media de los estimadores:  ', apply(estimaciones,1,mean),'\n')
ecm<-apply((estimaciones-theta)^2,1,mean);ecm  ##se observa que el mv tiene menor ECM
 
