rm(list=ls())
mu0<- 0; sigma<- 1; alfa<- 0.05
n<- 30
nr<- 30000
estad<- replicate(nr, {
 x<- rnorm(n, mu0, sigma)
 wilcox<- sum(rank(abs(x-mu0)) * (x>mu0))
 wilcox<- abs(wilcox - (n*(n+1)/4))
 bino<- sum(x>mu0); bino<- abs(bino - n/2)
 c(bino, wilcox)
 })
###SIGNOS
bino<- sort(estad[1,]) ## valores ordenados del estadistico transformado de los signos
q1<- quantile(bino, 1-alfa)
q1 ## frontera de la regi�n cr�tica
tabla_b<- table(bino) ## distribuci�n de valores de bino
posicion<- sum(bino<= q1) ## a partir de [posicion+1] los valores pertenecen a la RC(Signos)
tama_b<- 1-posicion/nr ## tama�o del test de los signos, aproximado por Montecarlo
tamb_teorico<- 2*(1-pbinom(n/2+q1, n, .5)) ## tama�o exacto del test de los signos
cat('RC binomial: abs(U-n/2)>',q1,' ; tama�o del test= ', tama_b,' ;tama�o exacto= ', tamb_teorico,'\n')
###WILCOXON
wilcox<- sort(estad[2,]) ## valores ordenado del estadistico transformado de Wilcoxon
q2<- wilcox[posicion]; q2 ## frontera de la regi�n cr�tica
tama_w<- 1- (sum(wilcox <= q2)/nr) ## tama�o del test de Wilcoxon,aproximado por Montecarlo
tama_wa<- 2*(1-pnorm(q2+0.5,0,sqrt(n*(n+1)*(2*n+1)/24)))## tama�o del test de Wilcoxon,aproximado por la normal
cat('RC Wilcoxon: abs(T+ - (n(n+1)/4))>',q2,' ; tama�o del test wilcoxon simulacion= ', tama_w,'\n', 'tama�o exacto=',tama_wa,'\n')
#extra
q2<-98.5 ##para que el tama�o exacto se quede por debabajo del eexacto par el test binomial
tama_w<- 1- (sum(wilcox <= q2)/nr) ## tama�o del test de Wilcoxon,aproximado por Montecarlo
tama_wa<- 2*(1-pnorm(q2+0.5,0,sqrt(n*(n+1)*(2*n+1)/24)))## tama�o del test de Wilcoxon,aproximado por la normal
cat('RC Wilcoxon: abs(T+ - (n(n+1)/4))>',q2,' ; tama�o del test wilcoxon simulacion= ', tama_w,'\n', 'tama�o exacto=',tama_wa,'\n')
###TEST T
q3<- qt(1-tama_wa/2,n-1); tama_t<- 2*(1-pt(q3, n-1)) ## frontera de la regi�n cr�tica
cat('RC test t: abs(sqrt(n)*mean(x-mu0)/s))>',q3,' ; tama�o del test tstudent= ', tama_t,'\n')

## funciones para determinar si una muestra est� en la RC o no
rb<-function(x) as.numeric(abs(sum(x>mu0)-n/2)>q1) ## rb= 1 si x est� en la regi�n critica
rw<-function (x) {w<-sum(rank(abs(x-mu0)) * (x>mu0)); as.numeric( abs(w- (n*(n+1)/4))>q2)}
rt<- function(x) as.numeric(abs(sqrt(n)*mean(x-mu0)/sd(x))>q3)

###calculo de una potencia para mu=mu0+.1, sigma=1
m1<-0.1*sigma
 a<-replicate (nr,{ x<-rnorm(n,m1,sigma); c(rb(x),rw(x),rt(x))})
 pot<-apply(a,1,sum)/nr
cat(' mu sigma potbino potwil pott \n'); c(m1,sigma,pot)
## Efecto de las medias y las desviaciones t�picas en las potencias de los test
p<- c() ## inicializa el objeto p
m<-c(0,.1,.25, .5,.75,1) ## medias de la normal
s<-c(1,2,3) ## desviaciones t�picas de la Normal
nr<-3000
for (i in 1:3) {
 for (j in 1:5){ ## Para cada desv. t�pica se comparan cinco medias
 a<-replicate (nr,{ x<-rnorm(n,m[j],s[i]);
 c(rb(x),rw(x),rt(x))})
 pot<-apply(a,1,sum) ## potencia de cada test para N[mu(j), s(i))]
 p<-rbind(p,c(m[j],s[i],pot/nr )) ## matriz con medias, desv. tipics y potencias
 }
 }
dim(p)
colnames(p)<- c('mu', 'sigma', 'pot_bino', 'pot_wil', 'pot_t') ## nombres de las columnas de p
p<- round(p,5); p ## tabla de las potencias para distintas medias y desviaciones t�picas
salida<- cbind(p[1:5,], p[6:10,2:5], p[11:15, 2:5])
salida