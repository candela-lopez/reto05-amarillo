source("Scripts/carga-datos.R")

#Eliminamos los valores negativos y los valores =0 en vtas
df <- df[!(df$vtas <= 0),]
#Eliminamos el dia 20200805 ya que es un outlier
df<- df[!(df$dia == '20200805'),]

#cambiar formato dia
df$dia <- ymd(df$dia)

#group_by de ventas por dia
ventasxdia <- df[,c(3,4)]
ventasxdia<- ventasxdia %>% group_by(dia) %>%  summarise(vtas= sum(vtas))
#creamos una col nueva con los dias de la semana de cada fecha para graficar despues 

ventasxdia$dia_semana <- wday(ventasxdia$dia)

ventasxdia$dia_semana <- ventasxdia$dia_semana -1

#GRAFICOS----------------


#EVOLUCION DE VENTAS GENERAL
ventas_por_dia<-df%>%
  group_by(dia)%>%
  summarise(suma_ventas=sum(vtas))
ventas_por_dia$mesya?o<- mday(ventas_por_dia$dia)


grafico_evolucion_ventas<-ggplot(ventas_por_dia,aes(x=dia,y=suma_ventas))+
  geom_line(colour="sea green")+
  geom_text(x=as.Date("2020-10-1"), y=251829.32, label="D?a sin IVA")+
  ggtitle("Evoluci?n de ventas de Eroski")+
  xlab("D?a")+
  ylab("Suma de las ventas")

grafico_evolucion_ventas




#group_by de ventas por dia
ventasxdia <- df[,c(3,4)]
ventasxdia<- ventasxdia %>% group_by(dia) %>%  summarise(vtas= sum(vtas))


#creamos una col nueva con los dias de la semana de cada fecha para graficar despues 

ventasxdia$dia_semana <- wday(ventasxdia$dia)

ventasxdia$dia_semana <- ventasxdia$dia_semana -1





ventas_por_dia_de_la_semana<-ventasxdia%>%
  group_by(dia_semana)%>%
  summarise(suma_ventas=sum(vtas))
ventas_por_dia_de_la_semana$dia_sem <- ventas_por_dia_de_la_semana$dia_semana
ventas_por_dia_de_la_semana$dia_sem <- c("lunes","martes","miercoles","jueves","viernes","sabado")
ventas_por_dia_de_la_semana$dia_sem <- factor(ventas_por_dia_de_la_semana$dia_sem, levels = c("lunes","martes","miercoles","jueves","viernes","sabado"))
ventas_por_dia_de_la_semana$suma_ventas<- as.character(ventas_por_dia_de_la_semana$suma_ventas)

grafico_evolucion_ventas_dias_de_la_semana<-ggplot(ventas_por_dia_de_la_semana,aes(x=dia_sem,y=suma_ventas,group=1))+
  geom_line(size=3,colour="seagreen")+
  ggtitle("Ventas por d?as de la semana")+
  xlab("D?as de la semana")+
  ylab("Suma de las ventas")
grafico_evolucion_ventas_dias_de_la_semana

#DF SIN LOS DIAS DE DIA SIN IVA
dfsiniva<- df[!(df$dia == '2020-10-22'),]
dfsiniva<- dfsiniva[!(dfsiniva$dia == '2020-10-23'),]
dfsiniva<- dfsiniva[!(dfsiniva$dia == '2020-10-24'),]

#group_by de ventas por dia
ventasxdia_siniva <- dfsiniva[,c(3,4)]
ventasxdia_siniva<- ventasxdia_siniva %>% group_by(dia) %>%  summarise(vtas= sum(vtas))



grafico_distribuacion_compras<-
  ggplot(df,aes(x=vtas))+geom_histogram(boundary = 0,binwidth = 1,color = "tomato1", fill = "tomato2", alpha = 0.2) +
  scale_x_continuous(name = "Ventas (euros)", limits = c(0,350), breaks = seq(0,350,50))+
  scale_y_continuous(name = "Cantidad",limits = c(0,10000))+
  theme_classic()+
  labs(title = "Distribuci?n de ventas")+
  theme(panel.grid.major = element_line(colour = "chocolate", linetype = "dotted"))+
  geom_vline(xintercept = mean(df$vtas), 
             color = "springgreen3", size=1.5)+
  annotate("text", x = 90, y = 9000, label = "Media")+
  annotate("text", x = 90, y = 8500, label = "47.07 euros")

grafico_distribuacion_compras

dfiva1<-df[(df$dia == "2020-10-22"),]
dfiva2<-df[(df$dia == "2020-10-23"),]
dfiva3<-df[(df$dia == "2020-10-24"),]

dfiva<-rbind(dfiva1,dfiva2,dfiva3)


grafico_distribucion_compras_diasiniva<-ggplot(dfiva,aes(x=vtas)) 

grafico_distribucion_compras_diasiniva+geom_histogram(boundary = 0,binwidth = 1,color = "tomato1", fill = "tomato2", alpha = 0.2) +
  scale_x_continuous(name = "Ventas (euros)", limits = c(0,350), breaks = seq(0,300,50))+
  scale_y_continuous(name = "Cantidad",limits = c(0,300))+
  theme_classic()+
  labs(title = "Distribuci?n de ventas d?a sin IVA")+
  theme(panel.grid.major = element_line(colour = "chocolate", linetype = "dotted"))+
  geom_vline(xintercept = mean(dfiva$vtas), 
             color = "springgreen3", size=1.5)+
  annotate("text", x = 120, y = 275, label = "Media")+
  annotate("text", x = 120, y = 260, label = "78.67 euros")




fact_siniva<-dfiva%>%group_by(dia)%>%
  summarise(suma_ventas=sum(vtas))


grafico_evolucion_ventas_diasiniva<-ggplot(fact_siniva,aes(x=dia,y=suma_ventas,group=1))+
  geom_line(size=3,colour="firebrick")+
  ggtitle("Ventas por d?a")+
  xlab("D?a")+
  ylab("Suma de las ventas")
grafico_evolucion_ventas_diasiniva


dfmes <- df %>%
  group_by(year(dia), month(dia)) %>% 
  summarise(suma_venta=sum(vtas),fecha=max(dia))

dfmes$`month(dia)`<-as.character(dfmes$`month(dia)`)

colnames(dfmes)<-c("year","month","suma_venta","fecha")
#graficos plotly

#GRAFICO DE LAS VENTAS TOTALES POR CADA MES EN EL A?O 2020
por_mes_2020<-dfmes[dfmes$year==2020,]

por_mes_2020[por_mes_2020$fecha =="2020-01-31", "month"]<- "Enero"
por_mes_2020[por_mes_2020$fecha =="2020-02-29", "month"]<- "Febrero"
por_mes_2020[por_mes_2020$fecha =="2020-03-31", "month"]<- "Marzo"
por_mes_2020[por_mes_2020$fecha =="2020-04-30", "month"]<- "Abril"
por_mes_2020[por_mes_2020$fecha =="2020-05-30", "month"]<- "Mayo"
por_mes_2020[por_mes_2020$fecha =="2020-06-30", "month"]<- "Junio"
por_mes_2020[por_mes_2020$fecha =="2020-07-31", "month"]<- "Julio"
por_mes_2020[por_mes_2020$fecha =="2020-08-31", "month"]<- "Agosto"
por_mes_2020[por_mes_2020$fecha =="2020-09-30", "month"]<- "Septiembre"
por_mes_2020[por_mes_2020$fecha =="2020-10-31", "month"]<- "Octubre"
por_mes_2020[por_mes_2020$fecha =="2020-11-30", "month"]<- "Noviembre"
por_mes_2020[por_mes_2020$fecha =="2020-12-31", "month"]<- "Diciembre"



por_mes_2020$month<-as.factor(por_mes_2020$month)
por_mes_2020$month <- factor(por_mes_2020$month, levels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))


plot_ly(por_mes_2020,x=~month,y=~suma_venta,type = "bar") %>% 
  layout(title="Total de ventas por cada mes en el a?o 2020")



