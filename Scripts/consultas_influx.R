#En este script se desarollaran las consultas de influx desde la API de R

source("Scripts/influx_reto5.R")

client <- InfluxDBClient$new(url= "http://192.168.56.103:8086" , 
                              token = 	"ClZRcfwktskfp4WMmlnlHpqe3BAOc25-2E9W_sknWulh6iD7Te2-W3Ydu8ZRAZhAhh8gsGlL2x6Kxv6neEYkrg==" ,
                              org = "MU" ) 
client$health()

#consulta1:
#Esta consulta devuelve todas las instancias agrupadas por tienda en todo el espacio temporal.
my_query <- 'from(bucket: "datos_reto5")
              |> range(start: 2019-10-01, stop: 2020-12-31)
              |> filter(fn: (r) => r["_measurement"] == "VENTAS")
              |> filter(fn: (r) => r["_field"] == "vtas")
              |> group(columns: ["location"])'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta2
#Esta consulta devuelve las ventas realizadas en de la localización "06249b487b77106345119a0074fb44da".
my_query <- 'from(bucket: "datos_reto5")
                |> range(start: 2019-10-01, stop: 2020-12-31)
                |> filter(fn: (r) => r["_measurement"] == "VENTAS")
                |> filter(fn: (r) => r["_field"] == "vtas")
                |> filter(fn: (r) => r["cod_loc_enc"] == "06249b487b77106345119a0074fb44da")
                |> group(columns: ["location"])'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta3
#Esta consulta devuelve las ventas realizadas en de la localización " 8f141081695357d592c0c9e7dc3b8b72" durante el mes de febrero de 2020.
my_query <- 'from(bucket: "datos_reto5")
                |> range(start: 2020-02-01, stop: 2020-02-29)
                |> filter(fn: (r) => r["_measurement"] == "VENTAS")
                |> filter(fn: (r) => r["_field"] == "vtas")
                |> filter(fn: (r) => r["cod_loc_enc"] == "8f141081695357d592c0c9e7dc3b8b72")
                |> group(columns: ["location"])'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta4
#Esta consulta devuelve el valor minimo de vtas en toda la serie temporal
my_query <- 'from(bucket: "datos_reto5")
                |> range(start: 2019-10-01, stop: 2020-12-31)
                |> filter(fn: (r) => r["_measurement"] == "VENTAS")
                |> filter(fn: (r) => r["_field"] == "vtas")
                |> group(columns: ["location"])
                |>min()'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta5
#Esta consulta devuelve el valor maximo de vtas en toda la serie temporal
my_query <- 'from(bucket: "datos_reto5")
                |> range(start: 2019-10-01, stop: 2020-12-31)
                |> filter(fn: (r) => r["_measurement"] == "VENTAS")
                |> filter(fn: (r) => r["_field"] == "vtas")
                |> group(columns: ["location"])
                |>max()'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta6
#Esta consukta devuelve las instancia ordenadas por vtas de menor a mayor.
my_query <- 'from(bucket: "datos_reto5")
              |> range(start: 2019-10-01, stop: 2020-12-31)
              |> filter(fn: (r) => r["_measurement"] == "VENTAS")
              |> filter(fn: (r) => r["_field"] == "vtas")
              |> group(columns: ["location"])
              |> sort(columns: ["_value"], desc: false)'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta9
#Esta consulta devuelve la media cada 6 dias, lo que es una semana completa, durante 
#un periodo de 24 horas, es decir todo el dia.
my_query <- 'from(bucket: "datos_reto5")
                |> range(start: 2019-10-01, stop: 2020-12-31)
                |> filter(fn: (r) => r["_measurement"] == "VENTAS")
                |> filter(fn: (r) => r["_field"] == "vtas")
                |> group(columns: ["location"])
                |> timedMovingAverage(every: 6d, period: 24h)'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta10
#Esta consulta devuelve la media cada 30 dias, lo que es una mes completo, durante 
#un periodo de 24 horas, es decir todo el dia.
my_query <- 'from(bucket: "datos_reto5")
                |> range(start: 2019-10-01, stop: 2020-12-31)
                |> filter(fn: (r) => r["_measurement"] == "VENTAS")
                |> filter(fn: (r) => r["_field"] == "vtas")
                |> group(columns: ["location"])
                |> timedMovingAverage(every: 30d, period: 24h)'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

#consulta11
#Esta consulta devuelve una nueva columna con las ventas en dolares.
my_query <- 'from(bucket: "datos_reto5")
                |> range(start: 2019-10-01, stop: 2020-12-31)
                |> filter(fn: (r) => r["_measurement"] == "VENTAS")
                |> filter(fn: (r) => r["_field"] == "vtas")
                |> group(columns: ["location"])
                |> map(fn: (r) => ({ r with vtas_dollar: r._value * 1.16 }))'


my_query <- gsub("[\r\n]" , "" , my_query)

data <- client$query(my_query)

data_system <- data[[1]][c("time" , "_value")] 

plot(data_system$time , data_system$'_value')

