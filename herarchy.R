library(readr)
library(fpc) #para hacer el plotcluster
library(fastcluster)
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering
library(dplyr)
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians

movies = read.csv("./tmdb-movies.csv")
# View(movies)
mydata = as.data.frame(unclass(movies))
summary(mydata)
dim(mydata)
myDataClean = na.omit(mydata)

distinct(myDataClean, original_title)
dim(myDataClean)
summary(myDataClean)
myDataCleanNumeric =myDataClean[, !sapply(myDataClean, is.character)]
scaled_data = as.matrix(scale(myDataCleanNumeric))


sample = myDataCleanNumeric[sample(nrow(myDataCleanNumeric), 20), ]

filtered <-myDataClean[myDataClean$id %in% sample$id,]

sample$id = NULL
sample$budget_adj = NULL
sample$revenue_adj = NULL


#Clustering jerárquico pop
names <- filtered$popularity
rownames(sample)<-names
hc<-hclust(dist(sample)) #Genera el clustering jerárquico de los datos

plot(hc) #Genera el dendograma
rect.hclust(hc,k=4) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=4) #corta el dendograma, determinando el grupo de cada fila

#datos$gruposHC<-groups

#Clustering jerárquico nombres
names <- filtered$original_title
rownames(sample)<-names
hc<-hclust(dist(sample)) #Genera el clustering jerárquico de los datos

plot(hc) #Genera el dendograma
rect.hclust(hc,k=4) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=4) #corta el dendograma, determinando el grupo de cada fila
#datos$gruposHC<-groups

#Clustering jerárquico budget
names <- filtered$budget
rownames(sample)<-names
hc<-hclust(dist(sample)) #Genera el clustering jerárquico de los datos

plot(hc) #Genera el dendograma
rect.hclust(hc,k=4) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=4) #corta el dendograma, determinando el grupo de cada fila
#datos$gruposHC<-groups
#Clustering jerárquico director
names <- filtered$director
rownames(sample)<-names
hc<-hclust(dist(sample)) #Genera el clustering jerárquico de los datos

plot(hc) #Genera el dendograma
rect.hclust(hc,k=4) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=4) #corta el dendograma, determinando el grupo de cada fila

#datos$gruposHC<-groups
#Clustering jerárquico id
names <- filtered$id
rownames(sample)<-names
hc<-hclust(dist(sample)) #Genera el clustering jerárquico de los datos

plot(hc) #Genera el dendograma
rect.hclust(hc,k=4) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=4) #corta el dendograma, determinando el grupo de cada fila
filtered$gruposHC<-groups
groups

g1HC<-filtered[filtered$gruposHC == 1,]
g2HC<-filtered[filtered$gruposHC==2,]
g3HC<-filtered[filtered$gruposHC==3,]
g4HC<-filtered[filtered$gruposHC==4,]
filteredGroup1 <-myDataClean[myDataClean$id %in% g1HC$id,]
filteredGroup2 <-myDataClean[myDataClean$id %in% g2HC$id,]
filteredGroup3 <-myDataClean[myDataClean$id %in% g3HC$id,]
filteredGroup4 <-myDataClean[myDataClean$id %in% g4HC$id,]
filteredGroup1$release_year
filteredGroup2$release_year
filteredGroup3$release_year
filteredGroup4$release_year
#Método de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(sample))
mean(silch[,3])

