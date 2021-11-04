source("Scripts/carga-datos.R")
############SOLO EJECUTAR UNA VEZ
#YA QUE SINO SE SOBREESCRIBEN LOS DATOS

cliente <- InfluxDBClient$new(url= "http://192.168.56.103:8086" , 
                             token = 	"ClZRcfwktskfp4WMmlnlHpqe3BAOc25-2E9W_sknWulh6iD7Te2-W3Ydu8ZRAZhAhh8gsGlL2x6Kxv6neEYkrg==" ,
                             org = "MU" ) 
cliente$health()

#CREAR UN BUCKET
INFLUX_TOKEN="ClZRcfwktskfp4WMmlnlHpqe3BAOc25-2E9W_sknWulh6iD7Te2-W3Ydu8ZRAZhAhh8gsGlL2x6Kxv6neEYkrg=="

Influx_Authorization_Header <- paste("Token",INFLUX_TOKEN)

INFLUX_URL = "http://192.168.56.103:8086/api/v2/buckets"

INFLUX_ORG_ID = '{
          "orgID":"6e88af07a5258706",
          "name":"datos_reto5"
}'

response<-POST(
  INFLUX_URL,
  body = INFLUX_ORG_ID,
  accept("application/csv"),
  content_type("application/json"),
  add_headers(Authorization = Influx_Authorization_Header)
)

response

response<-content(response)

BucketID<-response$id


####IMPORTAR DATOS EN INFLUX
#Cambiamos la variable fecha en formato GMT para que pueda leer influx

df_influx<-ventas
nrow(ventas)
6100436/10000
sep<- seq(1, 6100436, by=6100)

df_influx <- df_influx[sep,]
nrow(df_influx)
df_influx$dia <- ymd(df_influx$dia)

df_influx$dia<-as.POSIXct(df_influx$dia,tz="GMT")

df_influx<-as.data.frame(df_influx)

df_influx<-mutate(df_influx,measurement = "VENTAS")

response<-cliente$write(df_influx,
                          bucket="datos_reto5",precision="s",
                          measurementCol="measurement",
                          tagCols=c("id_cliente_enc","cod_loc_enc"),
                          fieldCols="vtas",
                          timeCol="dia")

