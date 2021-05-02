# https://cran.r-project.org/web/packages/ggplot2/index.html
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')

# https://cran.r-project.org/web/packages/dplyr/index.html
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')

if (!require(cluster)) install.packages(cluster);library(cluster)
if(!require(MVA)) install.packages("MVA"); library(MVA)
library(tidyr)
library(cluster)
library(plyr)
library(readr)

print("Hello World!")

#https://datos.gob.es/es/catalogo/l01280796-taxi-objetos-perdidos1
#objectesTaxi<-read.csv("TAXI_Objetos_Perdidos_original.csv", header=T, sep=";")

#https://archive.ics.uci.edu/ml/datasets/Online+Retail+II
#onlineRetail<-read.csv("online_retail_II.csv", header=T, sep=";")
#colnames(onlineRetail) <- c("InvoiceNo", "StockCode", "Description", "Quantity", "InvoiceDate", "UnitPrice", "CustomerID", "Country")

#Travel Ratings https://archive.ics.uci.edu/ml/datasets/Tarvel+Review+Ratings
#Rating 1-5
#googleRatings<-read.csv("google_review_ratings.csv", header=T, sep=",")
#colnames(googleRatings) <- c("UserID", "Churches", "Resorts", "Beaches", "Parks", "Theatres", "Museums", "Malls",
#                            "Zoo", "Restaurants", "Pubs/Bars", "LocalServices", "Burger/PizzaShops", "Hotels/OtherLodgings",
#                            "JuiceBars", "ArtGalleries", "DanceClubs", "SwimmingPools", "Gyms", "Bakeries",
#                            "Beauty&Spas", "Cafes", "ViewPoints", "Monuments", "Gardens")

#Atur BCN entre 2012-2021 per districtes entre 16i64 anys
#atur2012<-read.csv("2012_atur.csv", header = T, sep=",")
atur2019<-read.csv("2019_atur.csv", header = T, sep=",")

#https://datascienceplus.com/how-to-import-multiple-csv-files-simultaneously-in-r-and-create-a-data-frame/
mycsvfiles = list.files(pattern="*.csv", full.names=TRUE)
mycsvfiles
datafromcsv = ldply(mycsvfiles, read_csv)
colnames(datafromcsv) <- c("Any", "Mes", "CodiDistricte", "NomDistricte", "CodiBarri", "NomBarri", "Poblacio16_64anys", "PesAtur")

if (!require(cluster)) install.packages(cluster);library(cluster)
if(!require(MVA)) install.packages("MVA"); library(MVA)


#str(datafromcsv)
#summary(datafromcsv)

numericData <- datafromcsv %>% select(is.numeric)

d <- daisy(numericData) 
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(numericData, i)
  y_cluster     <- fit$cluster
  sk            <- silhouette(y_cluster, d)
  resultados[i] <- mean(sk[,3])
}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Nº of clusters",ylab="Silhouette")

#k=3
#clusters molt sobreposats
fit2 <- kmeans(numericData, 3)
y_cluster2 <- fit$cluster
clusplot(numericData, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


numeric2019Data <- atur2019 %>% select(is.numeric)
d <- daisy(numeric2019Data) 
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(numeric2019Data, i)
  y_cluster     <- fit$cluster
  sk            <- silhouette(y_cluster, d)
  resultados[i] <- mean(sk[,3])
}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Nº of clusters",ylab="Silhouette")

#k=3
#clusters molt sobreposats
fit2 <- kmeans(numeric2019Data, 10)
y_cluster2 <- fit$cluster
clusplot(numeric2019Data, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

