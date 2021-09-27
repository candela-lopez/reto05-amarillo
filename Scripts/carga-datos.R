#Carga de datos -----------------------------------------------

data <- readRDS("Datos originales/data.RDS")

ventas <- data$vtasdiaclte

impactados <- data$impactados

  #Creamos un df con los datos de los clientes que compraron el dia de la promocion
df <- merge(x = ventas, y= impactados, by= c('id_cliente_enc', 'cod_loc_enc'))


