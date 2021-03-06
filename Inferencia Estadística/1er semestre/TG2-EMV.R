rm(list = ls()) #Borrado de los datos
x<- c(2.70, 8.47, 6.99, 4.82, 7.45, 7.35, 4.86, 8.08, 6.87, 3.16, 4.81, 5.65, 6.50, 6.24, 9.05, 3.79, 8.13, 3.22, 6.31, 7.31, 8.41, 9.62, 4.83, 3.66, 3.71, 8.30, 7.44, 4.68, 7.36, 4.96)
n<- length(x)

#grafica de verosimilitud
l<-function(t)(9*n*log(t))+(n*log(40320))+8*sum(x)-t*sum(x)
val<-seq(0.1,6,length=200) #valores del parametro
f<-Vectorize(l)
v<-f(val)#valores de la verosimilitud
#realizamos la grafica con un plot
plot(val,log(v),type='l', main=" Log Funcion de verosimilitud", ylab='log(L)',xlab='Theta')


#estimacion maximo verosimil (metodo grafico y numerico)
a<-which.max(f(val))
cat('La aproximacion grafica de la estimacion maximo verosimil es a =', val[a],'\n')
es<-optimize(l,c(0,15),maximum=T);es
cat('La aproximacion numerica de la estimacion maximo verosimil es a=',es[[1]],'\n')

##APARTADO 2
