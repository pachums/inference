#TEST DE LA MEDIANA:
q<-Psicologia[,5]<median(Psicologia[,5])
q[which(Psicologia$CURSO=='2�Bach')]
q[which(Psicologia$CURSO!='2�Bach')]
summary(q[which(Psicologia$CURSO=='2�Bach')])
summary(q[which(Psicologia$CURSO!='2�Bach')])
#H0: es que la mediana(2bach)<=mediana(resto)


length(Psicologia[which(Psicologia[,1]!='Avil�s1'),]$Autovaloraci�n<median(Psicologia$Autovaloraci�n))
sum(Psicologia[which(Psicologia[,1]!='Avil�s1'),]$Autovaloraci�n<median(Psicologia$Autovaloraci�n))
length(Psicologia[which(Psicologia[,1]=='Avil�s1'),]$Autovaloraci�n<median(Psicologia$Autovaloraci�n))
sum(Psicologia[which(Psicologia[,1]=='Avil�s1'),]$Autovaloraci�n<median(Psicologia$Autovaloraci�n))


Psicologia$var1<-with(Psicologia,(Psicologia$Depresi�n<=median(Psicologia$Depresi�n)))
Psicologia$var2<-with(Psicologia, ((Psicologia$CURSO=='4�ESO')+(Psicologia$CURSO=='2�Bach')))
fisher.test(Psicologia$var1,Psicologia$var2,alternative='less')

Psicologia$var1<-with(Psicologia,(Psicologia$Autovaloraci�n<median(Psicologia$Autovaloraci�n)))
Psicologia$var2<-with(Psicologia,(Psicologia$CENTRO=='Avil�s1'))


#Esto ignoradlo:
wilcox.test(centrosB[,5],mu=median(centrosA[,5]))
wilcox.test(centrosB[,5],alternative="greater",mu=median(centrosA[,5]))
z<-Psicologia[which(Psicologia[,4]!='2�Bach'),]
z<-z[which(z[,4]!='4�ESO'),]
kruskal.test(Ansiedad ~ CURSO, data=z[which(z[,4]!='4�ESO'),])

