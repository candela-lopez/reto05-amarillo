#Modelo de series temporales
#setwd("C:/Users/34686/Downloads/reto5")
# Carga de datos y librerias ----------------------------------------------
source("Scripts/Cluster.R")
library(xts)
# Quitar dias que influyen en la promocion 
df_antesiva <- df[(df$dia >= '2019-10-01' & df$dia <= '2020-10-18'),]

# creamos un data frame con los datos de los clusters 
clientes <- normalized[,(c(1,8))]
dfmodelos <- inner_join(x = df, y = clientes, by = "id_cliente_enc")
dfmodelos$cluster <- as.numeric(dfmodelos$cluster)
str(dfmodelos)
clientes1<- subset(dfmodelos, cluster == 1)
clientes2<- subset(dfmodelos, cluster == 2)
# Comprobar que no hay domingos y crear la serie temporal
df_antesiva$diasemana<- wday(df_antesiva$dia)
str(df_antesiva)

sumaventas<-df_antesiva%>%
  group_by(dia)%>%
  summarise(total=sum(vtas))

nrow(sumaventas)

dt <- as.Date(sumaventas$dia)

df_ts <- zoo(sumaventas$total, dt)
df_tsx<- as.xts(df_ts)

# con los dos clusters tengo que hacer 2 series temporales, una para cada,
# y dos modelos una para cada 
sumaventasc1<-clientes1%>%
  group_by(dia)%>%
  summarise(total=sum(vtas))
sumaventasc1<- sumaventasc1[(sumaventasc1$dia >= '2019-10-01' & sumaventasc1$dia <= '2020-10-18'),]
dtc1 <- as.Date(sumaventasc1$dia)

sumaventasc2<-clientes2%>%
  group_by(dia)%>%
  summarise(total=sum(vtas))
sumaventasc2<- sumaventasc2[(sumaventasc2$dia >= '2019-10-01' & sumaventasc2$dia <= '2020-10-18'),]
dtc2 <- as.Date(sumaventasc2$dia)

# creamos las series temporales de los clusters 
c1_ts <- zoo(sumaventasc1$total, dtc1)
c2_ts <- zoo(sumaventasc2$total, dtc2)


# estadisticos del data frame para modelar  
is.ts(df_ts)
is.zoo(df_ts)

start(df_ts)
end(df_ts)
summary(df_ts)


# 
acf2(df_ts, col=1:6, gg=T) # al parecer hay estacionalidad, asique intentamos quitarla  

tsplot(df_ts, col=4, lwd=2, gg=TRUE) #visualizamos nuestra ts
tsplot(log(df_ts), col=4, lwd=2, gg=TRUE) # intentamos igualar la varianza 
tsplot(diff(log(df_ts)), col=4, lwd=2, gg=TRUE) # intentamos quitarle la tendencia
tsplot(diff(diff(log(df_ts)), lag=6), col=4, lwd=2, gg=TRUE) # le intentamos quitar la estacionalidad 

# log para igualar varianza
# diff para quitar tendencia 
# diff diff con lag para quitar estacionalidad 

#creamos nuestra serie temporal
my_ts <- diff(diff(log(df_ts), lag=6))
acf2(my_ts, col=1:6, lwd=2, gg=TRUE) # probamos quitando la ultima por que el acf da valores malos 

my_ts <- diff(log(df_ts), lag = 6)
acf2(my_ts, col=1:6, lwd=2, gg=TRUE) # ahora el acf tiene mejores valores y esta sera la ts que utilizaremos para hacer el modelo
# interpretar el acf 

# procedemos a hacer el modelo SARIMA
sarima(log(df_tsx), p=5, d=1, q=1, P=0, D=0, Q=1, S=6, gg=TRUE, col=2)

df_ts$ttable ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! preguntar

# creemos que este es el mejor modelo que tenemos asique procedemos a dibujarlo

sarima.for(log(df_tsx), n.ahead=25, p=5, d=1, q=1, P=2, D=1, Q=1, S=6, gg=TRUE, col=2, plot.all=TRUE)


text(1960.2, 7.1, "Past"); text(1962.1, 7.1, "Future")
abline(v=1961, lty=2, col=4)




## primero, creamos el modelo con el primer cluster
# 
acf2(c1_ts, col=1:6, gg=T, lwd= 2) # al parecer hay estacionalidad, asique intentamos quitarla  

tsplot(c1_ts, col=4, lwd=2, gg=TRUE) #visualizamos nuestra ts
tsplot(log(c1_ts), col=4, lwd=2, gg=TRUE) # intentamos igualar la varianza 
tsplot(diff(log(c1_ts)), col=4, lwd=2, gg=TRUE) # intentamos quitarle la tendencia
tsplot(diff(diff(log(c1_ts)), lag=6), col=4, lwd=2, gg=TRUE) # le intentamos quitar la estacionalidad 

#creamos nuestra serie temporal
# utilizamos el metodo sarima 
my_c1 <- diff(diff(log(c1_ts), lag=6))
acf2(my_c1, col=1:6, lwd=2, gg=TRUE) # probamos quitando la ultima por que el acf da valores malos 

my_c1 <- diff(log(c1_ts), lag = 6)
acf2(my_c1, col=1:6, lwd=2, gg=TRUE) # ahora el acf tiene mejores valores y esta sera la ts que utilizaremos para hacer el modelo
# interpretar el acf 
is.na(my_c1)
# procedemos a hacer el modelo SARIMA
sarima(log(my_c1),  p=5, d=1, q=1, P=2, D=1, Q=1, S=6, gg=TRUE, col=2)

c1_ts$ttable ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! preguntar

# creemos que este es el mejor modelo que tenemos asique procedemos a dibujarlo

sarima.for(log(my_c1), n.ahead=30, p=1, d=1, q=1, P=0, D=1, Q=1, S=12, gg=TRUE, col=2, plot.all=TRUE)
text(1960.2, 7.1, "Past"); text(1962.1, 7.1, "Future")
abline(v=1961, lty=2, col=4)

# utilizamos el metodo HolyWinters 

mean(c1_ts)
plot(c1_ts)

log_c1_ts <- log(c1_ts)
plot.ts(log_c1_ts)

log_c1_ts_forecast <- HoltWinters(ts(c1_ts, frequency=(365.25/7)), gamma=FALSE, beta=FALSE)
log_c1_ts_forecast
plot(log_c1_ts_forecast)
log_c1_ts_forecast$SSE









## despues, creamos el modelo con el segundo cluster
# 
acf2(c2_ts, col=1:6, gg=T, lwd= 2) # al parecer hay estacionalidad, asique intentamos quitarla  

tsplot(c2_ts, col=4, lwd=2, gg=TRUE) #visualizamos nuestra ts
tsplot(log(c2_ts), col=4, lwd=2, gg=TRUE) # intentamos igualar la varianza 
tsplot(diff(log(c2_ts)), col=4, lwd=2, gg=TRUE) # intentamos quitarle la tendencia
tsplot(diff(diff(log(c2_ts)), lag=6), col=4, lwd=2, gg=TRUE) # le intentamos quitar la estacionalidad 

#creamos nuestra serie temporal
my_c2 <- diff(diff(log(c2_ts), lag=6))
acf2(my_c2, col=1:6, lwd=2, gg=TRUE) # probamos quitando la ultima por que el acf da valores malos 

my_c2 <- diff(log(c1_ts), lag = 6)
acf2(my_c2, col=1:6, lwd=2, gg=TRUE) # ahora el acf tiene mejores valores y esta sera la ts que utilizaremos para hacer el modelo
# interpretar el acf 

# procedemos a hacer el modelo SARIMA
sarima(log(my_c2), p=1, d=1, q=1, P=0, D=0, Q=1, S=1, gg=TRUE, col=2)

c2_ts$ttable ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! preguntar

# creemos que este es el mejor modelo que tenemos asique procedemos a dibujarlo

sarima.for(log(my_c2), n.ahead=30, p=1, d=1, q=1, P=0, D=1, Q=1, S=12, gg=TRUE, col=2, plot.all=TRUE)
text(1960.2, 7.1, "Past"); text(1962.1, 7.1, "Future")
abline(v=1961, lty=2, col=4)
