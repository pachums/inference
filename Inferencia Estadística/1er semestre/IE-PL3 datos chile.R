data(Chile, package="car")

summary(Chile)
library(colorspace, pos=18)
local({
  .Table <- with(Chile, table(vote))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
with(Chile, pie(table(vote), labels=levels(vote), xlab="", ylab="", main="Intencion de voto", col=palette()[2:5]))

text(0,0.5,'35.1%');text(0,-0.5,'34.3%');text(0.5,0.1,'7.4%');text(-0.6,0,'23,2%')
legend( 'topright', c('Abstencion', 'no', 'no sabe', 'si'),pch=15, col = c(2:5))

por<- round(100*table(Chile$vote)/sum(.Table), 2)
a<- paste0(as.character(por[2]), "%"); a 
text(0,0.5,a);text(0,-0.5,'34.3%');text(0.5,0.1,'7.4%');text(-0.6,0,'23,2%')




with(Chile, Barplot(education, by=vote, scale =  "percent", conditional=TRUE,
  style = "parallel", legend.pos="above", xlab="vote",   ylab="percent"))

####legend.pos="above" en linux no funciona lo cambia por legend.pos="topleft"


local({
  .Table <- xtabs(~education+vote, data=Chile)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

Chile <- within(Chile, {
sex <- Recode(sex, '"F"="Mujer"; "M"="Hombre"', as.factor.result=TRUE)
})
Chile <- within(Chile, {
  education <- Recode(education, '"P"="Primarios"; "S"="Secundarios"; "PS"="Superiores"', 
  as.factor.result=TRUE)
})


local({
  .Table <- xtabs(~education+vote, data=Chile)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nRow percentages:\n")
  print(rowPercents(.Table))
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

with(Chile, Barplot(vote, by=education, style="divided", legend.pos="above", xlab="vote", 
  ylab="Percent", scale="percent"))
with(Chile, Barplot(education, by=vote, style="parallel", legend.pos="above", 
  xlab="education", ylab="Percent", scale="percent"))
help(Chile, package='car')
with(Chile, Barplot(education, by=vote, style="divided", legend.pos="above", 
  xlab="education", ylab="Percent", scale="percent"))
library(e1071, pos=18)
numSummary(Chile[,"age", drop=FALSE], groups=Chile$vote, statistics=c("mean", "sd", "IQR", 
  "quantiles"), quantiles=c(0,.25,.5,.75,1))
with(Chile, plotMeans(age, vote, error.bars="sd", connect=TRUE))
with(Chile, plotMeans(age, vote, error.bars="se", connect=TRUE))
Boxplot(age~vote, data=Chile, id.method="y")


##INTENCION EN FUNCION DEL SEXO
local({
  .Table <- with(Chile, table(sex))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
with(Chile, pie(table(sex), labels=levels(sex), xlab="", ylab="", main="Sexo en la muestra", 
  col=palette()[2:5]))
text(0,0.5,'48.93%');text(0,-0.5,'51.07%')
legend( 'topright', c('hombre', 'mujer'),pch=15, col = c(2:5))

with(Chile, Barplot(sex, xlab="sex", ylab="Percent", scale="percent"))
with(Chile, Barplot(sex, by=vote, scale =  "percent", conditional=TRUE,
  style = "parallel", legend.pos="above", xlab="region",   ylab="percent"))
legend( 'topright', c('Abstencion', 'no', 'no sabe', 'si'),pch=15, col = c(2:5))

##INTENCION EN FUNCION DE LA RESIDENCIA
local({
  .Table <- with(Chile, table(region))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
with(Chile, pie(table(region), labels=levels(region), xlab="", ylab="", main="Intencion de voto", 
  col=palette()[2:6]))
text(0,0.5,'35.1%');text(0,-0.5,'34.3%');text(0.5,0.1,'7.4%');text(-0.6,0,'23,2%')

with(Chile, Barplot(region, xlab="region", ylab="Percent", scale="percent"))
legend( 'topright', c('Abstencion', 'no', 'no sabe', 'si'),pch=15, col = c(2:5))
with(Chile, Barplot(region, by=vote, scale =  "percent", conditional=TRUE,
  style = "parallel", legend.pos="above", xlab="region",   ylab="percent"))

##INTENCION EN FUNCION DE LOS INGRESOS
local({
  .Table <- with(Chile, table(income))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
with(Chile, pie(table(income), labels=levels(income), xlab="", ylab="", main="Intencion de voto", 
  col=palette()[2:6]))

with(Chile, Barplot(income, xlab="income", ylab="Percent", scale="percent"))
legend( 'topright', c('Abstencion', 'no', 'no sabe', 'si'),pch=15, col = c(2:5))
with(Chile, Barplot(income, by=vote, scale =  "percent", conditional=TRUE,
  style = "parallel", legend.pos="above", xlab="region",   ylab="percent"))