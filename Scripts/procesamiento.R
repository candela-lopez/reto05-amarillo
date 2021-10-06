#Estadisticos descriptivos y procesamiento de datos

# Carga de datos y librerias ----------------------------------------------

source("Scripts/librerias.R")
source("Scripts/carga-datos.R")


# Tipos de datos  ---------------------------------------------------------
str(df)

# Duplicados ------------------------------------------------------------
df[duplicated(df),] #0 duplicados por filas
df[,duplicated(df)] #0 duplicados por columnas

# Valores ausentes --------------------------------------------------------
any_na(df) #ningun NA

# Outliers ----------------------------------------------------------------
boxplot(df$vtas)

# Descriptivos ------------------------------------------------------------
estadisticos_vars <- as.data.frame(summary(df)) 
estadisticos_vars <- estadisticos_vars[,2:3] #quitar la columna vacia que aparece
colnames(estadisticos_vars) <- c('nombre_var', 'estadistico')
estadisticos_vars$nombre_var <-gsub(" ", "", estadisticos_vars$nombre_var)
estadisticos_vars
#write.xlsx(estadisticos_vars, file = "Tabla de frecuencias.xlsx")

# Tipos de datos  ---------------------------------------------------------
#Cambiamos el formato de la fecha
df$dia <- ymd(df$dia)
str(df)

# Outliers ----------------------------------------------------------------
#Eliminamos los valores negativos y los valores =0 en vtas
df <- df[!(df$vtas <= 0),]
#Eliminamos el dia 20200805 ya que es un outlier
df<- df[!(df$dia == '2020-08-05'),]
#Una vez eliminados los outliers volvemos a visualizar los datos
boxplot(df$vtas)

# Descriptivos ------------------------------------------------------------
#volvemos a ver los estadisticos descriptivos tras la limpieza de datos
estadisticos_vars <- as.data.frame(summary(df)) 
estadisticos_vars <- estadisticos_vars[,2:3] #quitar la columna vacia que aparece
colnames(estadisticos_vars) <- c('nombre_var', 'estadistico')
estadisticos_vars$nombre_var <-gsub(" ", "", estadisticos_vars$nombre_var)
estadisticos_vars
#write.xlsx(estadisticos_vars, file = "Tabla de frecuencias.xlsx")