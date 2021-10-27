#Segmentacion de clientes mediante k means
# Carga de datos, librerias y metricas ----------------------------------------------

source("Scripts/librerias.R")
source("Scripts/carga-datos.R")
source("Scripts/procesamiento.R")
source("Scripts/metricas.R")

#Normalizar los datos
#normalizar
normalized1 <- (dfvariables2[,2]-min(dfvariables2[,2]))/(max(dfvariables2[,2])-min(dfvariables2[,2]))
max(normalized1)
min(normalized1)
normalized2 <- (dfvariables2[,3]-min(dfvariables2[,3]))/(max(dfvariables2[,3])-min(dfvariables2[,3]))
max(normalized2)
min(normalized2)
normalized3 <- (dfvariables2[,5]-min(dfvariables2[,5]))/(max(dfvariables2[,5])-min(dfvariables2[,5]))
max(normalized3)
min(normalized3)
normalized4 <- (dfvariables2[,6]-min(dfvariables2[,6]))/(max(dfvariables2[,6])-min(dfvariables2[,6]))
max(normalized4)
min(normalized4)

#vemos que esta bien la normalizacion
ddd<-plot(normalized$frecuencia_media)
ddd<-plot(normalized2$media_mes)
ddd<-plot(normalized3$media_general)
ddd<-plot(normalized4$media_semanal)
ddd<-plot(normalized5$varianza_vtas)

normalized <- data.frame(id_cliente  = dfvariables2$id_cliente_enc, arpu = dfvariables2$arpu , frecuencia_media = normalized1, media_mes= normalized2 , media_general = normalized3 ,
                           media_semanal = normalized4 , varianza_vtas = normalized5)
#el dataframe creado nos servira para crear los cluster



#grafica de correlaciones
corrmatrix <- cor(normalized[,-1])
corrplot(corrmatrix, method = 'number')

#grafica de los datos con las medias de mes y la media de frecuencia de compra de cliente
ggplot() + geom_point(aes(x = frecuencia_media , y = media_mes , colour = factor(arpu)), data = normalized, alpha = 0.5) + ggtitle('Conjunto de Datos')

#silhoutte score / metodo del codo
silhouette_score <- function(k){
  km <- kmeans(normalized[,2:7], centers = k, nstart=10)
  silscore <- silhouette(km$cluster, dist(normalized[,2:7]))
  mean(silscore[, 3])
}
k <- 2:8
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Cantidad de centroides', ylab='Metodo del codo', frame=FALSE)
#Ya que el metodo del codo es a veces ambiguo, procedemos a realizar el analisis
#de la situeta que es mas objetivo de el codo.
#average silhouette width
fviz_nbclust(normalized[,2:7] , kmeans , method = 'silhouette' )
#Numero estimado de clusters = 2

#dendograma
dendrogram <- hclust(dist(normalized[,2:7], method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrogram, rotate = FALSE, labels = FALSE, theme_dendro = TRUE) + 
  labs(title = "Dendrograma")
#Mediante el dendograma, se podria decir que el numero indicado de cluster es 2 o 4

#cluster
set.seed(200)
k.means.fit <- kmeans(normalized[,2:7], 2, nstart = 10)
k.means.fit$size
k.means.fit$centers
k.means.fit$totss
k.means.fit$tot.withinss
k.means.fit$betweenss
k.means.fit

normalized$cluster <- k.means.fit$cluster
ggplot() + geom_point(aes(x = frecuencia_media , y = media_mes , colour = factor(cluster)), data = normalized, alpha = 0.5) + ggtitle('Conjunto de Datos')


#validar los cluster
km_clusters <- eclust(x = normalized[,2:7], FUNcluster = "kmeans", k = 2, seed = 200 ,
                      hc_metric = "euclidean", nstart = 10, graph = F)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "Accent" ,
                ggtheme = theme_classic())


  # Silhouette width of observation
sil <- km_clusters$silinfo$widths
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index,  ,drop = F]   #ningun valor negativo

