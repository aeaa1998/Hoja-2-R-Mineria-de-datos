library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering
library(VIM)


#clustering
#k-meanss
datos<-read.csv("./mov.csv")
aggr(datos)
datosCompleto<-datos[complete.cases(datos),]
km<-kmeans(datos[,1:5],4,iter.max =100)
datos$grupo<-km$cluster

g1<- datos[datos$grupo==1,]
prop.table(table(g1$popularity))*100
nrow(g1)
summary(g1)

g2<- datos[datos$grupo==2,]
prop.table(table(g2$popularity))*100

g3<- datos[datos$grupo==3,]
prop.table(table(g3$popularity))*100

g4<- datos[datos$grupo==4,]
prop.table(table(g4$popularity))*100

plotcluster(datos[,1:5],km$cluster)

silkm<-silhouette(km$cluster,dist(datos[,1:5]))
mean(silkm[,3])
