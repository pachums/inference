######################## EJERCICIOS TEMA 6 ########################
rm(list=ls())

# EJERCICIO 1 #
x<-c(24.3,24.8,24.9,24.4,26,25.4,26.5,23.4,24.9,23.8)
table(x)
x<-sort(x)
y<-(x-mean(x))/sd(x);y
e<-(y-mean(y))/sd(y);e
e<-e[c(1:6,8,9,10)]
c(0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9,1)-pnorm(e,0,1)
## EJERCICIO 2 ##
v<-c(0:5)
frec<-c(217,178,73,51,27,4)
x<-rep(v,frec)
EMV<-mean(x);EMV
g<-function(lambda){-lambda*sum(frec[1:5])+log(lambda)*sum(v[1:5]*frec[1:5])+frec[6]*log(1-ppois(4,lambda))}
z<-seq(EMV-3,EMV+3,0.01);plot(z,g(z),type="l")
mediaMV<-optimize(g,c(EMV-3,EMV+3),tol=0.001,maximum=TRUE)
cat('Estimación Máximo Verosimil= ', mediaMV[[1]], '\n')
EMV<-mediaMV[[1]]
pteorica<-c(dpois(0,EMV),dpois(1,EMV),dpois(2,EMV),dpois(3,EMV),dpois(4,EMV),1-ppois(4,EMV))
e<-550*pteorica;e
D<-sum((frec-e)^2/e);D
chisq.test(frec,p=pteorica) 
### EJERICICIO 3 ###
x<-c(108,122,144,129,107,115,114,97,96,126,85,152,83,69,95,87,71,94,83,94)
zona<-rep(c("1","2"),each=10)
EST<-data.frame(x,zona) #en Rcmdr hago zona factor
#### EJERCICIO 4 ####
x<-c(-0.1,-0.15,0,0.51,0.1,0.35,-0.21,-0.12,0.23,-0.17,0.2,0.25,0.3,-0.51,0.21,-0.4,-0.45)
n<-length(x)
##### EJERCICIO 5 #####
x<-c(0.4,1,0.1,1.2,2.9,3.3,3.9,0.6,0.1,1.6,1.5,8.1,0.2,0.1,2.8,1.4,0.6,0.3,0.7,4.9,2.0,0.9,0.2,0.3)
n<-length(x);n
med<-rep(c("A","B"),each=12)
EF<-data.frame(x,med)

######################## EJERCICIOS TEMA 7 ########################
# EJERCICIO 1 #
num<-factorial(6)*factorial(14)*factorial(4)*factorial(16)
a2<-num/(factorial(20)*factorial(2)*factorial(4)*factorial(14)*factorial(0));a2
a3<-num/(factorial(20)*factorial(3)*factorial(3)*factorial(13)*factorial(1));a3
a4<-num/(factorial(20)*factorial(4)*factorial(2)*factorial(12)*factorial(2));a4
a5<-num/(factorial(20)*factorial(5)*factorial(1)*factorial(11)*factorial(3));a5
a6<-num/(factorial(20)*factorial(6)*factorial(0)*factorial(10)*factorial(4));a6
## EJERCICIO 2 ##
num<-factorial(13)*factorial(32)*factorial(41)*factorial(4)
a<-num/(factorial(45)*factorial(4)*factorial(0)*factorial(9)*factorial(32));a
num/(factorial(45)*factorial(3)*factorial(1)*factorial(10)*factorial(31))

