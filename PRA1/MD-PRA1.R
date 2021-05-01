# https://cran.r-project.org/web/packages/ggplot2/index.html
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')

# https://cran.r-project.org/web/packages/dplyr/index.html
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')

if (!require(cluster)) install.packages(cluster);library(cluster)
if(!require(MVA)) install.packages("MVA"); library(MVA)
library(tidyr)
library(cluster)

print("Hello World!")

#https://datos.gob.es/es/catalogo/l01280796-taxi-objetos-perdidos1
objectesTaxi<-read.csv("TAXI_Objetos_Perdidos_original.csv", header=T, sep=";")

#https://archive.ics.uci.edu/ml/datasets/Online+Retail+II
onlineRetail<-read.csv("online_retail_II.csv", header=T, sep=";")
colnames(onlineRetail) <- c("InvoiceNo", "StockCode", "Description", "Quantity", "InvoiceDate", "UnitPrice", "CustomerID", "Country")

#Travel Ratings https://archive.ics.uci.edu/ml/datasets/Tarvel+Review+Ratings
#Rating 1-5
googleRatings<-read.csv("google_review_ratings.csv", header=T, sep=",")
colnames(googleRatings) <- c("UserID", "Churches", "Resorts", "Beaches", "Parks", "Theatres", "Museums", "Malls",
                            "Zoo", "Restaurants", "Pubs/Bars", "LocalServices", "Burger/PizzaShops", "Hotels/OtherLodgings",
                            "JuiceBars", "ArtGalleries", "DanceClubs", "SwimmingPools", "Gyms", "Bakeries",
                            "Beauty&Spas", "Cafes", "ViewPoints", "Monuments", "Gardens")

#Atur BCN entre 2012-2021 per districtes entre 16i64 anys
atur2012<-read.csv("2012_atur.csv", header = T, sep=",")
atur2021<-read.csv("2021_atur.csv", header = T, sep=",")