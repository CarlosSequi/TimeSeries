## ------------------------------------------------------------------------
library(tseries)
library(dplyr)
library(lubridate)


## ------------------------------------------------------------------------
datos = read.csv("5530E.csv", header = TRUE, sep=";")
datos = datos[,c("Fecha","Tmax")]
datos$Fecha = as.Date(datos$Fecha)


## ------------------------------------------------------------------------
apply(datos, 2, function(atributo){sum(is.na(atributo))})


## ------------------------------------------------------------------------
datos = datos[complete.cases(datos),]


## ------------------------------------------------------------------------
# agrupamos por año y mes y tomamos el maximo de cada uno
datos = datos %>%
mutate(month = format(Fecha, "%m"), year = format(Fecha, "%Y")) %>%
group_by(year, month) %>%
summarise(total = max(Tmax))

serie = datos$total # aqui estan las temperaturas máximas de cada mes de todos los años


## ------------------------------------------------------------------------
Npred = 2 # cantidad de datos a predecir (temperaturas máximas de marzo y abril)
serie.ts = ts(serie, frequency = 12) # frequency set to 12 to set stationality each 12 months
plot(decompose(serie.ts))


## ------------------------------------------------------------------------
serie.ts.log = log(serie.ts)
serie.log = log(serie)
plot(decompose(serie.ts.log))


## ------------------------------------------------------------------------
serie = serie.log


## ------------------------------------------------------------------------
decompose(serie.ts.log)$seasonal


## ------------------------------------------------------------------------
# para train
tiempo = 1:length(serie)
# para test
tiempoTs = (tiempo[length(tiempo)]+1):(tiempo[length(tiempo)]+Npred)


## ------------------------------------------------------------------------
plot.ts(serie,  xlim=c(1, tiempoTs[length(tiempoTs)]))


## ------------------------------------------------------------------------
parametros.H1 = lm(serie ~tiempo)
parametros.H1


## ------------------------------------------------------------------------
# tendencia estimada para los datos de train
tendEstimadaTr =  parametros.H1$coefficients[1] + tiempo*parametros.H1$coefficients[2] 
# tendencia estimada para las predicciones de test
tendEstimadaTs =  parametros.H1$coefficients[1] + tiempoTs*parametros.H1$coefficients[2] 


## ------------------------------------------------------------------------
plot.ts(serie, xlim=c(1, tiempoTs[length(tiempoTs)])) 
lines(tiempo, tendEstimadaTr, col = "blue") 
lines(tiempoTs, tendEstimadaTs, col = "green")


## ------------------------------------------------------------------------
JB = jarque.bera.test(parametros.H1$residuals)
JB


## ------------------------------------------------------------------------
serie.sinTend = serie - tendEstimadaTr


## ------------------------------------------------------------------------
plot.ts(serie.sinTend, xlim=c(1, tiempoTs[length(tiempoTs)])) 


## ------------------------------------------------------------------------
k = 12 # aquí guardamos los datos estacionales
estacionalidad = decompose(serie.ts.log)$seasonal[1:k]


## ------------------------------------------------------------------------
serie.sinTend.sinEst = serie.sinTend - estacionalidad


## ------------------------------------------------------------------------
plot.ts(serie.sinTend.sinEst, xlim=c(1, tiempoTs[length(tiempoTs)])) 



## ------------------------------------------------------------------------
acf(serie.sinTend.sinEst)


## ------------------------------------------------------------------------
adf.test(serie.sinTend.sinEst)


## ------------------------------------------------------------------------
pacf(serie.sinTend.sinEst)


## ------------------------------------------------------------------------
# coeficiente de autoregresión = 0, diferenciaciones = 0, coeficiente de medias moviles = 0
modelo = arima(serie.sinTend.sinEst, order = c(0,0,0))

# calculamos las predicciones
predicciones = predict(modelo, n.ahead = Npred)
valoresPredichos = predicciones$pred
valoresPredichos


## ------------------------------------------------------------------------
plot.ts(serie.sinTend.sinEst, xlim=c(1, tiempoTs[length(tiempoTs)])) 
lines(tiempoTs,valoresPredichos, col="green")


## ------------------------------------------------------------------------
Box.test(modelo$residuals)


## ------------------------------------------------------------------------
# coeficiente de autoregresión = 0, diferenciaciones = 0, coeficiente de medias moviles = 1
modelo = arima(serie.sinTend.sinEst, order = c(0,0,1))

# calculamos las predicciones
predicciones = predict(modelo, n.ahead = Npred)
valoresPredichos = predicciones$pred
valoresPredichos


## ------------------------------------------------------------------------
plot.ts(serie.sinTend.sinEst, xlim=c(1, tiempoTs[length(tiempoTs)])) 
lines(tiempoTs,valoresPredichos, col="green")


## ------------------------------------------------------------------------
Box.test(modelo$residuals)


## ------------------------------------------------------------------------
jarque.bera.test(modelo$residuals) 


## ------------------------------------------------------------------------
 shapiro.test(modelo$residuals) 


## ------------------------------------------------------------------------
hist(modelo$residuals, col="blue", prob=T,ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo$residuals))


## ------------------------------------------------------------------------
estacionalidades = c(estacionalidad[11], estacionalidad[12])

# Incluimos la estacionalidad 
valoresPredichos.Est = valoresPredichos + estacionalidades

# Incluimos la tendencia 
valoresPredichos.Est.Tend = valoresPredichos.Est + tendEstimadaTs

# Transformación de los logaritmos 
valoresPredichos.Est.Tend.exp = exp(valoresPredichos.Est.Tend)

# usamos exp(serie) por haber hecho al principio serie = log(serie)
plot.ts(exp(serie),xlim=c(1, tiempoTs[length(tiempoTs)])) 
lines(tiempoTs, valoresPredichos.Est.Tend.exp, col = "green")



## ------------------------------------------------------------------------
valoresPredichos.Est.Tend.exp

