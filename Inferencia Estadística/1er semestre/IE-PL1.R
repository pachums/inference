rm(list=ls())#eliminar todos los datos de la memoria de R

n<-100
## Generacion de la variables SEXO
Sexo<- c(rep(1, n), rep(2, n))

## Generacion de la variables PESO
Peso<- c( rnorm(n,70,10), rnorm(n,64,10))

## si la variable sexo no estuviera ordenada
# Peso<- rnorm(2*n,70,10)# para crear la variable Peso 
# Peso[which(Sexo==2)]<- Peso[which(Sexo==2)]-6##solo cambia la media
#otra opcion: Peso<- (1-Sexo)* 6 + 70 + rnorm(2*n,0,10)


Peso<- round(Peso, 0) ## redondeo a enteros

## Generacion de la variables ESTATURA
Estatura = 100 + 0.90 * Peso + rnorm(2*n,0, 20)
Estatura<- round(Estatura, 0)

A<- cbind(Sexo, Peso, Estatura) # matriz datos 200 individuos y las tres va.
summary(A)
table(A[,1])


plot(A[,2], A[,3], xlab="Peso", ylab="Estatura", main=" Grafica del Peso y la Estatura")

##Para trabajar con R comander
A1<- data.frame(A)

save("A1", file="c:/inferencia/PL/PL1.Rdata")###guardar fichero en windows