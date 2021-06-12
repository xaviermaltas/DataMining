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
mycsvfiles
datafromcsv = ldply(mycsvfiles, read_csv)
colnames(datafromcsv) <- c("Any", "Mes", "CodiDistricte", "NomDistricte", "CodiBarri", "NomBarri", "Poblacio16_64anys", "PesAtur")

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