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
  mutate(arpu = vtasT > 3440)

#creamos una nueva variable llamada media la cual nos indica la media de compras por cliente
#cada mes
dfmes <- df %>%
  group_by(year(dia), month(dia),id_cliente_enc)%>%
  mutate(media = mean(vtas))

dfmes<- dfmes %>% distinct(media, .keep_all = TRUE)


#para calcular el ARPU por mes
dfarpumes <- df %>%
  group_by(año=year(dia),mes=month(dia))%>%
  mutate(media = mean(vtas))
  
dfarpumesmedia<- dfarpumes %>% distinct(media, .keep_all = F)

dfarpumesmedia <- arrange(dfarpumesmedia, año , mes)

dfclientes <- df %>%
  group_by(año = year(dia), mes = month(dia))%>%
  count(id_cliente_enc)%>%
  count(mes)

dfarpumes <- merge.data.frame(dfarpumesmedia , dfclientes)

dfarpumes <- dfarpumes %>%
  summarise(arpu = media/n)




