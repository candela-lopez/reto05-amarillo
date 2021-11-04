#Modelo de series temporales
# Carga de datos y librerias ----------------------------------------------
source("Scripts/Cluster.R")

# creamos un data frame con los datos de los clusters 
clientes <- normalized[,(c(1,8))]
dfmodelos <- inner_join(x = df, y = clientes, by = "id_cliente_enc")
dfmodelos$cluster <- as.numeric(dfmodelos$cluster)
str(dfmodelos)
clientes1<- subset(dfmodelos, cluster == 1)
clientes2<- subset(dfmodelos, cluster == 2)


# Quitar dias que influyen en la promocion ------------------------------------
df_antesiva <- df[(df$dia >= '2019-10-01' & df$dia <= '2020-10-18'),]
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

# Con los 2 clusters, hacer 2 series temporales ------------------
# CLIENTES CLUSTER 1 --> CREACION DE TS 
c1_siniva<- clientes1[(clientes1$dia >= '2019-10-01' & clientes1$dia <= '2020-10-18'),]

sumaventasc1<-c1_siniva%>%
  group_by(dia)%>%
  summarise(total=sum(vtas))

nrow(sumaventasc1)

dtc1 <- as.Date(sumaventasc1$dia)
# creacion TS(zoo)
c1_ts <- zoo(sumaventasc1$total, dtc1)
# creacion TS(xts), sino da problemas luego
c1_tsx<- as.xts(c1_ts)


# CLIENTES 2 --> CREACION DE TS 
c2_siniva<- clientes2[(clientes2$dia >= '2019-10-01' & clientes2$dia <= '2020-10-18'),]

sumaventasc2<-c2_siniva%>%
  group_by(dia)%>%
  summarise(total=sum(vtas))

nrow(sumaventasc2)

dtc2 <- as.Date(sumaventasc2$dia)
# creacion TS(zoo)
c2_ts <- zoo(sumaventasc2$total, dtc2)
# creacion TS(xts), sino da problemas luego
c2_tsx<- as.xts(c2_ts)

# creamos dos ts(zoo),(uno para cada cluster) , con todos los dias --------
# CLIENTES 1
sumaventasc1_all<-clientes1%>%
  group_by(dia)%>%
  summarise(total=sum(vtas))

dt <- seq(as.Date("2019-10-01"), as.Date("2020-12-31"), by = "days")
clientes1_ts<- zoo(sumaventasc1_all, dt)

# CLIENTES 2
sumaventasc2_all<-clientes2%>%
  group_by(dia)%>%
  summarise(total=sum(vtas))

dt <- seq(as.Date("2019-10-01"), as.Date("2020-12-31"), by = "days")
clientes2_ts<- zoo(sumaventasc2_all, dt)

# CREACION DE MODELOS PARA LOS DOS CLUSTERS ----------------------

# MODELO ALISADO DE HOLT-WINTER ---------------------------------------------------

# CLUSTER 1----------------------------------------
mean(c1_ts)
plot(c1_ts)

log_c1_ts_forecast <- HoltWinters(ts(c1_ts, frequency=(365.25/7)), gamma=FALSE, beta=FALSE)
log_c1_ts_forecast
plot(log_c1_ts_forecast)
log_c1_ts_forecast$SSE

# como el error es tan alto no hace falta plotear los residuos y descartamos el modelo directamente 


# CLUSTER 2--------------------------------------
mean(c2_ts)
plot(c2_ts)

log_c2_ts_forecast <- HoltWinters(ts(c2_ts, frequency=(365.25/7)), gamma=FALSE, beta=FALSE)
log_c2_ts_forecast
plot(log_c2_ts_forecast)
log_c2_ts_forecast$SSE

# como el error es tan alto no hace falta plotear los residuos y descartamos el modelo directamente


# MODELO SARIMA ------------------------------------

# MODELO SARIMA CLIENTES 1 -------------------------------

# Estadisticos 
is.ts(c1_ts)
is.zoo(c1_ts)

start(c1_ts)
end(c1_ts)
summary(c1_ts)

# acf
acf2(c1_ts, col=1:6, gg=T, lwd= 2) # al parecer hay estacionalidad, asique intentamos quitarla  

tsplot(c1_ts, col=4, lwd=2, gg=TRUE) #visualizamos nuestra ts
tsplot(log(c1_ts), col=4, lwd=2, gg=TRUE) # intentamos igualar la varianza 
tsplot(diff(log(c1_ts)), col=4, lwd=2, gg=TRUE) # intentamos quitarle la tendencia
tsplot(diff(diff(log(c1_ts)), lag=6), col=4, lwd=2, gg=TRUE) # le intentamos quitar la estacionalidad 

# Vemos ya que probablemente lo mejor sea crear el modelo sarima y aplicarlo sobre la serie
# temporal con un solo diff, por sea caso probamos con dos
my_c1 <- diff(diff(log(c1_tsx), lag=6))
acf2(my_c1, col=1:6, lwd=2, gg=TRUE) # Interpretaremos con diff(diff 

# interpretamos los ACF y PACF y probamos valores para el modelo sarima
# procedemos a hacer el modelo SARIMA
############################

a<-sarima(log(c1_tsx),  p=0, d=1, q=2, P=0, D=1, Q=1, S=6, gg=TRUE, col=2)
a$ttable
a
# $AIC [1] 0.1538886     $AICc [1] 0.1541483       $BIC [1] 0.2025629
b<-sarima(log(c1_tsx),  p=0, d=1, q=2, P=2, D=1, Q=1, S=6, gg=TRUE, col=2)
b$ttable
b
# $AIC [1] 0.1261917     $AICc [1] 0.1268453       $BIC [1] 0.1992032
c<-sarima(log(c1_tsx),  p=1, d=1, q=1, P=1, D=1, Q=0, S=6, gg=TRUE, col=2)
c$ttable
c
# $AIC [1] 0.2125886     $AICc [1] 0.2128483       $BIC [1] 0.2612629

#utilizamos el segundo modelo de sarima que hemos utilizado, es el que valores mas bajos tiene
cluster1s<- sarima.for(log(c1_tsx), n.ahead=9,  p=0, d=1, q=2, P=2, D=1, Q=1, S=6, gg=TRUE, col=2, plot.all=TRUE)
abline(v=314, lty= 3, col= 1, lwd = 2)

cluster1s
cluster1s$pred <- exp(cluster1s$pred)
cluster1s$pred

clientes1_tsDF <- as.data.frame(clientes1_ts)
rownames(clientes1_tsDF) <- NULL
clientes1_tsDF <- clientes1_tsDF[(1:375),]


fechacluster1s<- clientes1_tsDF[(clientes1_tsDF$dia >= '2020-10-18' & clientes1_tsDF$dia <= '2020-10-28'),]

str(fechacluster1s)
pred1<- data.frame(fechacluster1s, cluster1s$pred)
pred1

# Grafico originales vs pred1
orvsp1<- plot_ly(pred1, x= ~dia, y= ~total, type = 'scatter', mode = 'lines', name = 'original', color = 1)
orvsp1 <- orvsp1 %>% 
  add_trace(y = ~cluster1s.pred , name = 'prediccion', color = 2)
orvsp1

# MODELO SARIMA CLIENTES 2---------------------------------------------

# Estadisticos
is.ts(c2_ts)
is.zoo(c2_ts)

start(c2_ts)
end(c2_ts)
summary(c2_ts)

# acf
acf2(c2_ts, col=1:6, gg=T, lwd= 2) # al parecer hay estacionalidad, asique intentamos quitarla  

tsplot(c2_ts, col=4, lwd=2, gg=TRUE) #visualizamos nuestra ts
tsplot(log(c2_ts), col=4, lwd=2, gg=TRUE) # intentamos igualar la varianza 
tsplot(diff(log(c2_ts)), col=4, lwd=2, gg=TRUE) # intentamos quitarle la tendencia
tsplot(diff(diff(log(c2_ts)), lag=6), col=4, lwd=2, gg=TRUE) # le intentamos quitar la estacionalidad 

# Vemos ya que probablemente lo mejor sea crear el modelo sarima y aplicarlo sobre la serie
# temporal con un solo diff, por sea caso probamos con dos
my_c2 <- diff(diff(log(c2_tsx), lag=6))
acf2(my_c2, col=1:6, lwd=2, gg=TRUE) # sera con el que interpretaremos los valores SARIMA


# interpretamos los ACF y PACF y probamos valores para el modelo sarima
# procedemos a hacer el modelo SARIMA

d<- sarima(log(c2_tsx),  p=1, d=1, q=2, P=2, D=1, Q=1, S=6, gg=TRUE, col=2)
d$ttable
d
# $AIC [1] 0.05311383     $AICc [1] 0.05403192       $BIC [1] 0.1382939
e<- sarima(log(c2_tsx),  p=1, d=1, q=1, P=1, D=1, Q=0, S=6, gg=TRUE, col=2)
e$ttable
e
# $AIC [1] 0.1169125     $AICc [1] 0.1171722       $BIC [1] 0.1655868
f<- sarima(log(c2_tsx),  p=2, d=1, q=1, P=2, D=1, Q=1, S=6, gg=TRUE, col=2)
f$ttable
f
# $AIC [1] 0.04237495     $AICc [1] 0.04329304       $BIC [1] 0.127555

#utilizamos el tercer modelo de sarima que hemos utilizado, es el que valores mas bajos tiene
cluster2s<- sarima.for(log(c2_tsx), n.ahead=9,  p=2, d=1, q=1, P=2, D=1, Q=1, S=6, gg=TRUE, col=2, plot.all=TRUE)
abline(v=314, lty= 3, col= 1, lwd = 2)

cluster2s
cluster2s$pred <- exp(cluster2s$pred)
cluster2s$pred

clientes2_tsDF <- as.data.frame(clientes2_ts)
rownames(clientes2_tsDF) <- NULL
clientes2_tsDF <- clientes2_tsDF[(1:375),]

fechacluster2s<- clientes2_tsDF[(clientes2_tsDF$dia >= '2020-10-18' & clientes2_tsDF$dia <= '2020-10-28'),]

str(fechacluster2s)
pred2<- data.frame(fechacluster2s, cluster2s$pred)
pred2

# Grafico orig vs pred2
orvsp2<- plot_ly(pred2, x= ~dia, y= ~total, type = 'scatter', mode = 'lines', name = 'original', color = 1)
orvsp2 <- orvsp2 %>% 
  add_trace(y = ~cluster2s.pred , name = 'prediccion', color = 3)
orvsp2

