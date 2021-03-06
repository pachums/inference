## BONDAD DE AJUSTE: RUTHERFORD, CHAPWICK, ELLIS
rm(list=ls())
# Paso 1. Estimar landa por Máxima Verosimilitud teniendo con datos agrupados en una clase.
## Datos experimentales:
v<- c(0:10) ## vector de valores. El valor 10 significa mayor a 9
frec<- c(57, 203, 383, 525, 532, 408, 273, 139, 45, 27, 16) ## vector de frecuencias
n<- sum(frec) ## tamaño de la muestra
 ## Como estimación inicial se calcula la media asignando el valor 12 a los datos de la clase (>=10)
x<- rep(c(0:9,12), c(57, 203, 383, 525, 532, 408, 273, 139, 45, 27, 16)) ## vector de observaciones
media0<- mean(x)
cat('Estimación Inicial de la Media= ', media0, '\n')
## Estimación Máximo Verosimil
g<- function(landa) { -landa * sum(frec[1:10]) + log(landa) * sum(v[1:10]* frec[1:10]) + frec[11] *log(1-ppois(9, landa))} ##Log de la Verosimilitud, salvo una constante
landas<- seq(media0-3,media0+3, .01);
plot(landas,g(landas), type="l",xlab="landa", ylab="g(landa)" )
mediaMV <- optimize(g, c(media0-3, media0+3), tol = 0.0001, maximum = TRUE) ## EMV
cat('Estimación Máximo Verosimil= ', mediaMV[[1]], '\n')

# Paso 2: Realización del test
## probabilidades teoricas usando la EMV
prob<- c(dpois(0:9, mediaMV[[1]]),1-ppois(9, mediaMV[[1]])); prob
cat('frec. observadas', frec,'\n')
cat('frec. esperadas',n*prob, '\n')
## Test Chi-Cuadrado de bondad de ajuste
ruth<- chisq.test(frec, correct = F, p = prob) ; ruth ## Test chi-cuadrado de Pearson
## grados de libertad: Se pierde uno por la estimación de landa
## En el test chisq.test no se especifica si el parámetro está estimado o no
## estructura del objeto ruth: names(ruth), ,ls(ruth)
names(ruth)
ruth[1:3] ## son los tres elementos que aparecen en la salida del ordenador
ruth $parameter; ruth$statistic; as.numeric(ruth $statistic)

# Paso 3: Cálculo del p-valor correcto
1-pchisq(ruth [[1]], ruth$parameter-1) ## p-valor con etiquetas
cat("p-valor= ", 1-pchisq(ruth $statistic, ruth$parameter-1), '\n') ## p-valor con etiquetas
1-pchisq(as.numeric(ruth $statistic), ruth$parameter-1) ## p-valor sin etiquetas
## Calculo del p-valor por simulación con las probabilidades teóricas, en lugar de la Chi-cuadrado
chisq.test(frec, correct = TRUE, p = prob, rescale.p = FALSE, simulate.p.value = T, B = 2000) 