#Modelo de series temporales

# Carga de datos y librerias ----------------------------------------------
source("Scripts/librerias.R")
source("Scripts/carga-datos.R")

#modelaje --------------------------------------------------------------
is.ts(df)

#La siguiente funcion crea un data frame que representa la suma total por cada 
#mes para poder convertirlo en una time series.
dfsum <- df %>%
  group_by(year(dia) ,month(dia), day(dia)) %>%
  summarise(vtas = sum(vtas))

#usamos la funcion time series y visualizamos:
df_ts <- ts(dfsum$vtas , start = c(2019, 10) , end = c(2020 , 12) , frequency = 365) 
#frecuencia = numero de dias que hay en los datos limpios
is.ts(df_ts)
df_ts
start(df_ts)
end(df_ts)
summary(df_ts)
plot.ts(df_ts)
abline(reg=lm(df_ts~time(df_ts)))

df_ts_log <- log(df_ts)
plot.ts(df_ts_log)


df_ts_SMA7 <- SMA(df_ts, n=7)
plot.ts(df_ts_SMA7)

#decompose
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












