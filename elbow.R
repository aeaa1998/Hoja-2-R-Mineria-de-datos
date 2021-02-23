library(readr)
movies = read.csv("./tmdb-movies.csv")
# View(movies)
mydata = as.data.frame(unclass(movies))
summary(mydata)
dim(mydata)
myDataClean = na.omit(mydata)

dim(myDataClean)
summary(myDataClean)
myDataCleanNumeric =myDataClean[, !sapply(myDataClean, is.character)]

scaled_data = as.matrix(scale(myDataCleanNumeric))

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

