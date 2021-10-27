#Metricas para graficar y crear nuevas variables
# Carga de datos y librerias ----------------------------------------------

source("Scripts/librerias.R")
source("Scripts/carga-datos.R")
source("Scripts/procesamiento.R")

#Nuevo data frame para graficar los datos
#group_by de ventas por dia
ventasxdia <- df[,c(3,4)]
ventasxdia<- ventasxdia %>% group_by(dia) %>%  summarise(vtas= sum(vtas))

#creamos una col nueva con los dias de la semana de cada fecha
ventasxdia$dia_semana <- wday(ventasxdia$dia)
ventasxdia$dia_semana <- ventasxdia$dia_semana -1 


#Nuevos data frames - df sin dias sin iva y df solo dias sin iva
#Este data frame contiene los clientes que compraron el dia sin iva ademas de todas sus compras
#eliminando los dias sin iva
dfsiniva <- df[!(df$dia == "2020-10-22") & !(df$dia == "2020-10-23") & !(df$dia == "2020-10-24"),]

#Este data frame contiene las ventas del dia sin iva 
dfiva1<-df[(df$dia == "2020-10-22"),]
dfiva2<-df[(df$dia == "2020-10-23"),]
dfiva3<-df[(df$dia == "2020-10-24"),]

dfiva<- rbind(dfiva1,dfiva2,dfiva3)

#Nuevas variables para los clusters
#creamos una nueva variable llamada arpu la cual indica si el cliente compra por encima de la 
#media o no, es decir si es cliente de eroski o cliente compartido
factor(df$id_cliente_enc) #6790 clientes en total
arpu <- sum(df$vtas)/6790 #arpu = 3440.717

dfarpu <- df %>%
  group_by(id_cliente_enc) %>%
  summarise(vtasT = sum(vtas)) %>%
  mutate(arpu = NA)

max(dfarpu$vtasT)
min(dfarpu$vtasT)
str(dfarpu)

medio <- (dfarpu$vtasT > 3000 & dfarpu$vtasT < 4000)
alto <- (dfarpu$vtasT > 4000 & dfarpu$vtasT < 10000)
muyalto <- (dfarpu$vtasT > 10000)
bajo <- (dfarpu$vtasT < 3000 & dfarpu$vtasT > 1000)
muybajo <- (dfarpu$vtasT <= 1000)

dfarpu$arpu[medio] <- 0.5
dfarpu$arpu[alto] <- 0.75
dfarpu$arpu[muyalto] <- 1
dfarpu$arpu[bajo] <- 0.25
dfarpu$arpu[muybajo] <- 0

#creamos una nueva variable llamada media la cual nos indica la media de compras por cliente
#cada mes
dfmes <- df %>%
  group_by(ano = year(dia), mes = month(dia),id_cliente_enc)%>%
  mutate(media = mean(vtas))

dfmes<- dfmes %>% distinct(media, .keep_all = TRUE)

#creamos una nueva variable la cual nos indica la frecuencia de compra por clientes por mes 
dffrec <- df %>%
  group_by(ano=year(dia),mes=month(dia))%>%
  count(id_cliente_enc)

#nueva variable media general
dfm <- df %>%
  group_by(id_cliente_enc) %>%
  summarise(media2 = mean(vtas))

#nueva variable media semanal
dfsemana <- df %>%
  group_by(ano = year(dia), semana = week(dia),id_cliente_enc)%>%
  mutate(media3 = mean(vtas))

dfsemana <- dfsemana[,c(1,3,7)] %>%
  group_by(id_cliente_enc) %>%
  mutate(media3 = mean(media3))
  
dfsemana<- dfsemana %>% distinct(media3, .keep_all = TRUE)

#variable de varianza de ventas totales
dfvarianzavtas<- df %>% 
  group_by(id_cliente_enc) %>% 
  summarize(varianza_vtas= var(vtas))

sum(is.na(dfvarianzavtas)) #esta nueva variables contiene NA ya que
                       #algunos clientes solo han realizado una unica compra

dfvarianzavtas[is.na(dfvarianzavtas)] <- 0
sum(is.na(dfvarianzavtas))  #pasamos los valores NA a cero

#Crear df de todas las variables
dfvariables <- merge(x = dffrec, y= dfmes, by= c('id_cliente_enc', 'ano', 'mes'))
dfvariables <- merge(x = dfvariables, y= dfsemana, by= c('id_cliente_enc' , 'dia'))
dfvariables <- merge(x = dfvariables, y = dfarpu, by= 'id_cliente_enc')
dfvariables <- merge(x = dfvariables, y = dfm, by= 'id_cliente_enc')
dfvariables <- merge(x = dfvariables, y = dfvarianzavtas, by= 'id_cliente_enc')

dfvariables <- dfvariables[,c(1,5,8:13)]

dfvariables2 <- dfvariables %>%
  group_by(id_cliente_enc) %>%
  transmute(frecuencia_media = mean(n) , media_mes = mean(media) , arpu = arpu , media_general = media2 , media_semanal = media3 , varianza_vtas = varianza_vtas)

dfvariables2<- dfvariables2 %>% distinct(id_cliente_enc, .keep_all = TRUE)




