#Estadisticos descriptivos y procesamiento de datos

# Carga de datos y librerias ----------------------------------------------

source("Scripts/carga-datos.R")
source("Scripts/librerias.R")

# Tipos de datos  ---------------------------------------------------------

str(df)

df$dia <- ymd(df$dia )

# Descriptivos ------------------------------------------------------------

estadisticos_vars <- as.data.frame(summary(df)) 
estadisticos_vars <- estadisticos_vars[,2:3] #quitar la columna vacia que aparece
colnames(estadisticos_vars) <- c('nombre_var', 'estadistico')
estadisticos_vars$nombre_var <-gsub(" ", "", estadisticos_vars$nombre_var)
estadisticos_vars
  #write.xlsx(estadisticos_vars, file = "Tabla de frecuencias.xlsx")

# Duplicados ------------------------------------------------------------

df[duplicated(df),] #0 duplicados por filas
df[,duplicated(df)] #0 duplicados por columnas


# Valores ausentes --------------------------------------------------------

any_na(df) #ningun NA


# Outliers ----------------------------------------------------------------

boxplot(df$vtas)




