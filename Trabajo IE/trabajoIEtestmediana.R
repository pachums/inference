#TEST DE LA MEDIANA:
q<-Psicologia[,5]<median(Psicologia[,5])
q[which(Psicologia$CURSO=='2ºBach')]
q[which(Psicologia$CURSO!='2ºBach')]
summary(q[which(Psicologia$CURSO=='2ºBach')])
summary(q[which(Psicologia$CURSO!='2ºBach')])
#H0: es que la mediana(2bach)<=mediana(resto)


length(Psicologia[which(Psicologia[,1]!='Avilés1'),]$Autovaloración<median(Psicologia$Autovaloración))
sum(Psicologia[which(Psicologia[,1]!='Avilés1'),]$Autovaloración<median(Psicologia$Autovaloración))
length(Psicologia[which(Psicologia[,1]=='Avilés1'),]$Autovaloración<median(Psicologia$Autovaloración))
sum(Psicologia[which(Psicologia[,1]=='Avilés1'),]$Autovaloración<median(Psicologia$Autovaloración))


Psicologia$var1<-with(Psicologia,(Psicologia$Depresión<=median(Psicologia$Depresión)))
Psicologia$var2<-with(Psicologia, ((Psicologia$CURSO=='4ºESO')+(Psicologia$CURSO=='2ºBach')))
fisher.test(Psicologia$var1,Psicologia$var2,alternative='less')

Psicologia$var1<-with(Psicologia,(Psicologia$Autovaloración<median(Psicologia$Autovaloración)))
Psicologia$var2<-with(Psicologia,(Psicologia$CENTRO=='Avilés1'))


#Esto ignoradlo:
wilcox.test(centrosB[,5],mu=median(centrosA[,5]))
wilcox.test(centrosB[,5],alternative="greater",mu=median(centrosA[,5]))
z<-Psicologia[which(Psicologia[,4]!='2ºBach'),]
z<-z[which(z[,4]!='4ºESO'),]
kruskal.test(Ansiedad ~ CURSO, data=z[which(z[,4]!='4ºESO'),])

