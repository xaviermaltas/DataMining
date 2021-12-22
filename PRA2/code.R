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

#barriPoblAtur Plot
# pobAturPlot <- ggplot(barriPoblAtur, aes(Poblacio, PesAtur)) + geom_point(aes(color=CodiBarri))
# pobAturPlot

## KMEANS CLUSTERING ##

### Average Silhouette Method
# if (!require(cluster)) install.packages(cluster);library(cluster)
# if(!require(MVA)) install.packages("MVA"); library(MVA)
# 
# #https://uc-r.github.io/kmeans_clustering#silo
# if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
# if(!require(factoextra)) install.packages("factoextra"); library(factoextra)
# # function to compute average silhouette for k clusters
# avg_sil <- function(k) {
#   km.res <- kmeans(barriPoblAtur[2:3], centers = k)
#   sk <- silhouette(km.res$cluster, dist(barriPoblAtur[2:3]))
#   mean(sk[, 3])
# }
# 
# # Compute and plot wss for k = 2 to k = 73
# k.values <- 2:73
# 
# # extract avg silhouette for 2-73 clusters
# avg_sil_values <- map_dbl(k.values, avg_sil)
# 
# plot(k.values, avg_sil_values,
#      type = "b", pch = 19, frame = FALSE,
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouettes") +
# abline(h = max(avg_sil_values))
# print(max(avg_sil_values))
# which.max(avg_sil_values)

# result <- rep(0,73)
# for( i in k.values ){
#   km.res <- kmeans(barriPoblAtur[1:3], centers = i)
#   sk <- silhouette(km.res$cluster, dist(barriPoblAtur[1:3]))
#   result[i]<-mean(sk[, 2])
# }
# print(max(result))
# which.max(result)
# 
# plot(2:73, result[2:73],
#     type = "b", pch = 19, frame = FALSE,
#     main = "Average Silhouette Method",
#     xlab = "Number of clusters K",
#     ylab = "Average Silhouettes")
# 
# clusplot(barriPoblAtur[2:3], km.res$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

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
# fit_ch  <- kmeansruns(barriPoblAtur[2:3], krange = 1:73, criterion = "ch")
# print(fit_ch$bestk)
# 
# plot(1:73, fit_ch$crit,
#      main = "Calinski-Harabasz Index",
#      type = "b", pch = 19, frame = FALSE,
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouettes")
# 
# 
# plot(barriPoblAtur, col=fit_ch$cluster)
# clusplot(barriPoblAtur[2:3], fit_ch$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

###Silhouette Different Metric
# if(!require(amap)) install.packages("amap"); library(amap)
# k.values <- 2:73
# result <- rep(0,73)
# for( i in k.values ){
#   km.res <- Kmeans(barriPoblAtur[1:3], centers = i, method = "canberra")
#   sk <- silhouette(km.res$cluster, dist(barriPoblAtur[1:3]))
#   result[i]<-mean(sk[, 2])
# }
# print(max(result))
# which.max(result)
# 
# plot(2:73, result[2:73],
#      type = "b", pch = 19, frame = FALSE,
#      main = "Average Silhouette Method - Canberra Distance",
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouettes")

###Mètode d'agrupaments jeràrquics

## REGLES ASSOCIACIO ##
# if (!require('arules')) install.packages('arules'); library('arules')
# 
# #barriDistPoblAtur
# # barriDistPoblAturNames <- c("CodiBarri","CodiDistricte", "Poblacio", "PesAtur")
# # barriDistPoblAtur <- select(numericData, barriDistPoblAturNames)
# 
# #distPoblAtur
distPoblAturNames <- c("CodiDistricte", "Poblacio", "PesAtur")
distPoblAtur <- select(numericData, distPoblAturNames)
# 
# ###Raw
# numericData_rules <- apriori(numericData, parameter = list(support = 0.01, confidence = 0.5))
# summary(numericData_rules)
# inspect(head(sort(numericData_rules, decreasing = TRUE, by = "confidence"), 10))
# 
# ###pobAturDistricte
# PobAturDistricte_rules <- apriori(distPoblAtur, parameter = list(support = 0.01, confidence = 0.5))
# summary(PobAturDistricte_rules)
# inspect(PobAturDistricte_rules)
# 
# ###pobAturBarri
# PobAturBarri_rules <- apriori(barriPoblAtur, parameter = list(support = 0.01, confidence = 0.5))
# summary(PobAturBarri_rules)
# inspect(PobAturBarri_rules)

##ARBRE DECISIÓ
#Creating a seed
set.seed(101)
trainRows = numericRows * 0.1
#Creating training indexes
train_indexes <- sample(numericRows, trainRows)
#Create training sample barriPoblAtur
atur_train <- barriPoblAtur[train_indexes, ]
#Creating test sample
atur_test  <- barriPoblAtur[-train_indexes, ]

#Load of C50 library
if (!require(C50)) install.packages(C50);library(C50)
#'default' to a factor
atur_train$PesAtur<-as.factor(atur_train$PesAtur)
#Model creation
model <- C50::C5.0(atur_train[-3], atur_train$PesAtur,rules = TRUE)
#Summary of the model
summary(model)

# #Plot the decision tree of the model
model <- C50::C5.0(atur_train[-3], atur_train$PesAtur)
# plot(model)

#Precision computation
predicted_model <- predict(model, atur_test, type="class")

# print(100*sum(predicted_model == train_indexes) / length(predicted_model))
# print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model == train_indexes) / length(predicted_model)))

summary(predicted_model)

if(!require(gmodels)){
  install.packages('gmodels', repos='http://cran.us.r-project.org')
  library(gmodels)
}
CrossTable(atur_test$PesAtur, predicted_model,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Reality', 'Predicted'))