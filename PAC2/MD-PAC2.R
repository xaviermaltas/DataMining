if (!require(cluster)) install.packages(cluster);library(cluster)
if(!require(MVA)) install.packages("MVA"); library(MVA)
library(tidyr)
library(cluster)
clients<-read.csv("clientes.csv", header=T, sep=",")
colnames(clients) <- c("customerID", "gender", "age", "annualIncome","spendingScore")
str(clients)
summary(clients)

#Count NA elements and drop that rows
colSums(is.na(clients))
#clients %>% drop_na()

#We set the gender to a numeric value
clients$gender <- ifelse(clients$gender=="Male", 0, 1)

#EV1

d <- daisy(clients) 
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(clients, i)
  y_cluster     <- fit$cluster
  sk            <- silhouette(y_cluster, d)
  resultados[i] <- mean(sk[,3])
}

plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Nº of clusters",ylab="Silhouette")

#k=2

fit2 <- kmeans(clients, 2)
y_cluster2 <- fit$cluster
clusplot(clients, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)



#Total within-cluster sum of squares (withinss)
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(clients, i)
  resultados[i] <- fit$tot.withinss
}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Nº of clusters",ylab="Total within-cluster sum of squares")

#fpc
if (!require(fpc)) install.packages(fpc);library(fpc)
fit_ch  <- kmeansruns(clients, krange = 1:10, criterion = "ch") 
fit_asw <- kmeansruns(clients, krange = 1:10, criterion = "asw") 

fit_ch

library(DDoutlier)
outlier_score <- KNN_AGG(dataset=clients, k_min=2, k_max=2)
# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)

clusplot(clients, outlier_score, color=TRUE, shade=TRUE, labels=2, lines=0)


### LASTFM

lastfm<-read.csv("lastfm.csv", header=T, sep=",")

#We count NA values - 0 NA
colSums(is.na(lastfm))

#We set the gender to a numeric value
clients$gender <- ifelse(clients$gender=="m", 0,1)


library(arules)
data("Groceries")
inspect(head(Groceries, 5))
itemFrequencyPlot(Groceries,topN=20,type="absolute")





#####
iris<-read.csv("flores.csv", header=T, sep=",")

df <- data(iris) ##load data
head(iris) ## see the studcture
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))

summary(iris_norm)
##   Sepal.Length     Sepal.Width      Petal.Length     Petal.Width     
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min. :0.00
##  1st Qu.:0.2222   1st Qu.:0.3333   1st Qu.:0.1017   1st Qu.:0.08  
##  Median :0.4167   Median :0.4167   Median :0.5678   Median :0.50
##  Mean   :0.4287   Mean   :0.4406   Mean   :0.4675   Mean   :0.45
##  3rd Qu.:0.5833   3rd Qu.:0.5417   3rd Qu.:0.6949   3rd Qu.:0.70
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00
##extract training set
iris_train <- iris_norm[ran,] 
##extract testing set
iris_test <- iris_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
iris_target_category <- iris[ran,5]
##extract 5th column if test dataset to measure the accuracy
iris_test_category <- iris[-ran,5]
##load the package class
library(class)
##run knn function
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=13)

##create confusion matrix
tab <- table(pr,iris_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

##because diamonds dataset is in ggplot2 package
library(ggplot2)
##load data
data(diamonds)

##store it as data frame
dia <- data.frame(diamonds)

##create a random number equal 90% of total number of rows
ran <- sample(1:nrow(dia),0.9 * nrow(dia))

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##normalization function is created
dia_nor <- as.data.frame(lapply(dia[,c(1,5,6,7,8,9,10)], nor))

##training dataset extracted
dia_train <- dia_nor[ran,]

##test dataset extracted
dia_test <- dia_nor[-ran,]
##the 2nd column of training dataset because that is what we need to predict about testing dataset
##also convert ordered factor to normal factor
dia_target <- as.factor(dia[ran,2])

##the actual values of 2nd couln of testing dataset to compaire it with values that will be predicted
##also convert ordered factor to normal factor
test_target <- as.factor(dia[-ran,2])

##run knn function
library(class)
pr <- knn(dia_train,dia_test,cl=dia_target,k=20)

##create the confucion matrix
tb <- table(pr,test_target)

##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

clusplot(clients, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
