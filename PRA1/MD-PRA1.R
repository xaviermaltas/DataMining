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


print("Hello World! -PRA1")

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


#https://opendata-ajuntament.barcelona.cat/data/ca/dataset/est-atur-pes
#Atur BCN entre 2012-2021 per districtes entre 16i64 anys
#Les dades que es presenten en aquest dataset corresponen a una estimaci? de l'atur registrat de Barcelona 
#a nivell de barri a partir de les dades facilitades pel Departament de Treball, Afers Socials i Fam?lies per codi postal. 
#Per aquesta ra? algun total pot no coincidir amb la suma del total d'aturats de Barcelona que publica 
#l'Observatori del treball i model productiu de la Generalitat de Catalunya.


#https://datascienceplus.com/how-to-import-multiple-csv-files-simultaneously-in-r-and-create-a-data-frame/
mycsvfiles = list.files(pattern="*.csv", full.names=TRUE)
mycsvfiles
datafromcsv = ldply(mycsvfiles, read_csv)
colnames(datafromcsv) <- c("Any", "Mes", "CodiDistricte", "NomDistricte", "CodiBarri", "NomBarri", "Poblacio16_64anys", "PesAtur")

#Only Atur2019
atur2019<-read.csv("2019_atur.csv", header = T, sep=",")
colnames(atur2019) <- c("Any", "Mes", "CodiDistricte", "NomDistricte", "CodiBarri", "NomBarri", "Poblacio16_64anys", "PesAtur")


#INFORMATION ABOUT DATA
#str(datafromcsv)
#summary(datafromcsv)

#ONLY NUMERIC DATA
numericData <- datafromcsv %>% select(is.numeric)
numericData2019 <- atur2019 %>% select(is.numeric)
numericRows =dim(numericData)[1]

str(numericData2019)
summary(numericData2019)
head(numericData2019)

#CLUSTERING
d <- daisy(numericData) 
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(numericData, i)
  y_cluster     <- fit$cluster
  sk            <- silhouette(y_cluster, d)
  resultados[i] <- mean(sk[,3])
}

districtes <- rep(0,10)
for (i in 10){
  districtes[i] <- numericData2019 %>% filter(CodiDistricte == i)
}
a <- districtes[1]

districte1 <- numericData2019 %>% filter(CodiDistricte == 1)
meanPoblacio1 <- mean(numericData2019$Poblacio16_64anys)
meanAtur1 <- mean(numericData2019$PesAtur)

ggplot(data = districte1, aes(x= CodiDistricte, y=Poblacio16_64anys, fill=PesAtur)) + geom_bar(position="fill")


#k=3
#clusters molt sobreposats
fit2 <- kmeans(numericData, 3)
y_cluster2 <- fit$cluster
#clusplot(numericData, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#AUTOMATIC CLUSTERING
if(!require(fpc)) install.packages(fpc); library(fpc)
if(!require(class)) install.packages(class); library(class)

#fit_ch  <- kmeansruns(numericData2019, krange = 1:73, criterion = "ch")
#fit_asw <- kmeansruns(numericData, krange = 1:73, criterion = "asw") 
#print(fit_ch$bestk)
#print(fit_asw$bestk)
#cl3 <- kmeans(numericData2019, fit_ch$bestk)
#with(numericData2019, pairs(numericData2019, col=c(1:6)[cl3$cluster])) 
#clusplot(numericData2019, fit_ch$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#PLOTS
# e <- ggplot(numericData2019, aes(x = Mes, y= PesAtur))
# e2 <- geom_boxplot(
#   aes(fill=CodiDistricte)
# )
# e2
# 
# p1 <- ggplot(numericData2019, aes(y = PesAtur)) + geom_boxplot() + facet_wrap(~CodiDistricte, scale="free")
# p1 + geom_jitter(
#   aes(shape = Mes, color = Mes)
# )
#
# b <- boxplot(PesAtur ~ CodiDistricte, data = numericData2019, xlab = "Pes Atur", ylab = "Nº Districte", horizontal=TRUE) 
# grid(nx=16, ny=16)

#gplot(data=numericData[1:numericRows,], aes(y=Poblacio16_64anys, fill=PesAtur)) + geom_bar(position="fill")+facet_wrap(~CodiDistricte)



# mean2019 <- lapply(numericData2019, function(numericData2019){numericData$PesAtur})
#stripchart(numericData2019$PesAtur ~ numericData2019$CodiDistricte, vertical = TRUE, method = "jitter", pch = 10, add = TRUE, col = 1:length(levels(numericData2019$CodiDistricte)))