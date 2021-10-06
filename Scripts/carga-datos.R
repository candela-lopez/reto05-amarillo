#Carga de datos -----------------------------------------------

data <- readRDS("Datos originales/data.RDS")

ventas <- data$vtasdiaclte

impactados <- data$impactados

# juntamos los dos data frames por clientes y locales en los que compran, 
# de esta manera encima, algunos outliers desaparecen, no todos pero bueno
df <- merge(x = ventas, y= impactados, by= c('id_cliente_enc', 'cod_loc_enc'))
