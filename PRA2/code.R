#https://cran.r-project.org/web/packages/ggplot2/index.html
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
# https://cran.r-project.org/web/packages/dplyr/index.html
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require(cluster)) install.packages(cluster);library(cluster)
if(!require(MVA)) install.packages("MVA"); library(MVA)
if(!require(tidyr)) install.packages("tidyr");library(tidyr)
if(!require(cluster)) install.packages("cluster"); library(cluster)
if(!require(plyr)) install.packages("plyr"); library(plyr)
if(!require(readr)) install.packages("readr"); library(readr)

print("This is the PRA2")

#IMPORT DATA
#https://datascienceplus.com/how-to-import-multiple-csv-files-simultaneously-in-r-and-create-a-data-frame/
mycsvfiles = list.files(pattern="*.csv", full.names=TRUE)
#mycsvfiles
datafromcsv = ldply(mycsvfiles, read_csv)
colnames(datafromcsv) <- c("Any", "Mes", "CodiDistricte", "NomDistricte", "CodiBarri", "NomBarri", "Poblacio", "PesAtur")

#ONLY NUMERIC DATA
numericData <- datafromcsv %>% select(is.numeric)
numericRows = dim(numericData)[1]

#List of DataFrame per each Districte
#https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames
myDistrictesList <- list()
for(i in c(1,2,3,4,5,6,7,8,9,10)){
  newDataframe <- filter(numericData, CodiDistricte == i)
  myDistrictesList[[i]] <- newDataframe
}

meanByDistricteList <- list()
for(i in c(1,2,3,4,5,6,7,8,9,10)){
  newMeanAtur <- mean(myDistrictesList[[i]]$PesAtur)
  meanByDistricteList[i] <- newMeanAtur
}

#barriPoblAtur

barriPoblAturNames <- c("CodiBarri", "Poblacio", "PesAtur")
barriPoblAtur <- select(numericData, barriPoblAturNames)

pobAturPlot <- ggplot(barriPoblAtur, aes(Poblacio, PesAtur)) + geom_point(aes(color=CodiBarri))
pobAturPlot

## KMEANS CLUSTERING ##

### Average Silhouette Method
if (!require(cluster)) install.packages(cluster);library(cluster)
if(!require(MVA)) install.packages("MVA"); library(MVA)

# d <- daisy(barriPoblAtur)
# resultados <- rep(0, 73)
# for (i in c(2:73))
# {
#   fit           <- kmeans(barriPoblAtur, i)
#   y_cluster     <- fit$cluster
#   sk            <- silhouette(y_cluster, d)
#   resultados[i] <- mean(sk[,3])
# }
# plot(2:73,resultados[2:73],type="o",col="blue",pch=0,xlab="Nº of clusters",ylab="Silhouette", label = max(resultados))

#https://uc-r.github.io/kmeans_clustering#silo
if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(factoextra)) install.packages("factoextra"); library(factoextra)
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(barriPoblAtur, centers = k)
  ss <- silhouette(km.res$cluster, dist(barriPoblAtur))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 73
k.values <- 2:73

# extract avg silhouette for 2-73 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes") +
abline(h = max(avg_sil_values))
print(max(avg_sil_values))
which.max(avg_sil_values)

### Elbow Method 
# result <- rep(0, 73)
# result2 <- rep(0, 73)
# for (i in c(2:73))
# {
#   fit <- kmeans(barriPoblAtur, i)
#   result[i] <- fit$tot.withinss
#   result2[i] <- fit$betweenss
# }
# plot(2:73,result[2:73],type="o",col="blue",pch=0,xlab="Nombre de clústers",ylab="")
# mtext("tot.withinss",side=2, line=3,col="blue")
# par(new=TRUE)
# plot(2:73,result2[2:73],type="o",axes=FALSE,col="green",xlab="",ylab="",)
# axis(side=4)
# mtext("betweenss",side=4, line=3,col="green")

### Calinski-Harabasz
# if (!require(fpc)) install.packages(fpc);library(fpc)
# fit_ch  <- kmeansruns(barriPoblAtur, krange = 1:73, criterion = "ch")
# print(fit_ch$bestk)
# cl2 <- kmeans(barriPoblAtur, fit_ch$bestk)
# with(barriPoblAtur, pairs(barriPoblAtur, col=c(1:6)[cl2$cluster]))
# plot(barriPoblAtur, col=cl2$cluster)
# clusplot(barriPoblAtur, cl2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)