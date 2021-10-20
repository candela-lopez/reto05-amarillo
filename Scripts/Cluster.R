#Segmentacion de clientes mediante k means
# Carga de datos, librerias y metricas ----------------------------------------------

source("Scripts/librerias.R")
source("Scripts/carga-datos.R")
source("Scripts/procesamiento.R")
source("Scripts/metricas.R")

#grafica de correlaciones
corrmatrix <- cor(dfvariables3[,-1])
corrplot(corrmatrix, method = 'number')

#grafica de los datos
ggplot() + geom_point(aes(x = frecuencia_media , y = media_mes , colour = factor(arpu)), data = dfvariables3, alpha = 0.5) + ggtitle('Conjunto de Datos')

#silhoutte score
silhouette_score <- function(k){
  km <- kmeans(dfvariables3[,2:4], centers = k, nstart=25)
  silscore <- silhouette(km$cluster, dist(dfvariables3[,2:4]))
  mean(silscore[, 3])
}
k <- 2:8
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Cantidad de centroides', ylab='Metodo del codo', frame=FALSE)
#Mediante el metodo del codo, se podria decir que el numero indicado de cluster es 4

#dendograma
dendrogram <- hclust(dist(dfvariables3[,2:4], method = 'euclidean'), method = 'ward.D')
ggdendrogram(dendrogram, rotate = FALSE, labels = FALSE, theme_dendro = TRUE) + 
  labs(title = "Dendrograma")
#Mediante el dendograma, se podria decir que el numero indicado de cluster es 2 o 4

#cluster
set.seed(20)
k.means.fit <- kmeans(dfvariables3[,2:4], 4, nstart = 10)
k.means.fit$size
k.means.fit$centers
k.means.fit$totss
k.means.fit$tot.withinss
k.means.fit$betweenss
k.means.fit

dfvariables3$cluster <- k.means.fit$cluster
ggplot() + geom_point(aes(x = frecuencia_media , y = media_mes , colour = factor(cluster)), data = dfvariables3, alpha = 0.5) + ggtitle('Conjunto de Datos')


#validar los cluster
km_clusters <- eclust(x = dfvariables3[,2:4], FUNcluster = "kmeans", k = 4, seed = 200,
                      hc_metric = "euclidean", nstart = 10, graph = F)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "Accent" ,
                ggtheme = theme_classic()) 



