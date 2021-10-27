#Modelo de series temporales

# Carga de datos y librerias ----------------------------------------------
source("Scripts/procesamiento.R")


#modelaje --------------------------------------------------------------
is.ts(df)

#La siguiente funcion crea un data frame que representa la suma total por cada 
#mes para poder convertirlo en una time series.
dfsum <- df %>%
  group_by(año=year(dia) ,mes=month(dia), dia=day(dia)) %>%
  summarise(vtas = sum(vtas))

library("xts")
library("zoo")
#usamos la funcion time series y visualizamos:

secuencia <- seq(as.Date("2019-10-01"), as.Date("2020-12-31"), by = "days")
# hay que kitarle los domingos!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
secuencia
#creamos el modelo 
modelo <- zoo(dfsum, secuencia) 

#estadisticos del data frame para modelar  
is.ts(modelo)
is.zoo(modelo)
modelo
start(modelo)
end(modelo)
summary(modelo)

plot(modelo$vtas)
plot(modelo$dia)
plot(modelo$mes)
plot(modelo$año)

# los abline solo funcionan para time series 
# reflejamos la tendencia
abline(reg=lm(modelo~time(modelo)), col= "red", lwd=2)
# reflejamos la media
abline(h = mean(modelo), col="blue", lwd=2, lty=10) 

#reflejamos los fitted values 
AR<- arima(df_ts, order = c(1,0,0))
ts.plot(df_ts)
df_fit <- df_ts - residuals(AR)
points(df_fit, type = "l", col = "red", lty = 2, lwd = 1)

df_ts_log <- log(df_ts)
plot.ts(df_ts_log)


library(TTR)
df_ts_SMA7 <- SMA(df_ts, n=7)
plot.ts(df_ts_SMA7)

#decompose 
# hay que poner que los periodos vayan por semana 
## 
df_ts_decomp <- stl(df_ts) #la serie tiene menos de 2 periodos y no funciona
df_ts_log_decomp <- decompose(df_ts_log)

#forecast
df_ts_forecast <- HoltWinters(df_ts, beta=FALSE, gamma=FALSE)
df_ts_forecast
df_ts_forecast$fitted

df_ts_forecast$SSE


df_ts_diff2 <- diff(df_ts, differences=2)
plot(df_ts_diff2)
df_ts_diff2_forecast <- HoltWinters(df_ts_diff2, beta=FALSE, gamma=FALSE)
plot(df_ts_diff2_forecast)
df_ts_log_forecast <- HoltWinters(df_ts_log, beta=FALSE, gamma=FALSE)
plot(df_ts_log_forecast)


#correlacion de la serie temporal
acf(df_ts)
acf(df_ts , plot = F)
pacf(df_ts)

#ARIMA
AR <- arima(df_ts, order = c(1,0,0))
print(AR)



lag.plot(df_ts) #???????

#https://estadistica-dma.ulpgc.es/cursoR4ULPGC/14-seriesTemporales.html












