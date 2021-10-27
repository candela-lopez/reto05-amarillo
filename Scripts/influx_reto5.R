library(influxdbclient)
library(Rcpp)

############
source("Scripts/carga-datos.R")
source("Scripts/procesamiento.R")
source("Scripts/librerias.R")
############
cliente<-InfluxDBClient$new(url="http://192.168.56.101:8086", 
                            token="TJ8fNMm3xSOHc-LenemKqxeCdYUFHxctoKN9oGK1HojBOdB7PTvN_iw6_vMenvzQ0xLeAFbkD9hbUj1RKU9jGA==",
                            org="MU")
cliente$health()

sep<-seq(1000,nrow(df),1000)

for (i in length(sep)) {
  response<-cliente$write(df,bucket="datos_reto5",precision="s",
                          measurementCol="vtas",
                          tagCols=c("id_cliente_enc","cod_loc_enc"),
                          fieldCols="vtas",
                          timeCol="dia")
  print(i)
}



