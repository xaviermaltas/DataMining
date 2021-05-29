data<-read.csv("./credit_kaggle.csv",header=T,sep=",")
attach(data)

if(!require(ggplot2)){
  install.packages('ggplot2', repos='http://cran.us.r-project.org')
  library(ggplot2)
}
if(!require(ggpubr)){
  install.packages('ggpubr', repos='http://cran.us.r-project.org')
  library(ggpubr)
}
if(!require(grid)){
  install.packages('grid', repos='http://cran.us.r-project.org')
  library(grid)
}
if(!require(gridExtra)){
  install.packages('gridExtra', repos='http://cran.us.r-project.org')
  library(gridExtra)
}
if(!require(C50)){
  install.packages('C50', repos='http://cran.us.r-project.org')
  library(C50)
}

dim(data)
str(data)
summary(data)

#Amount
Amount=data['amount']
d <- density(data$amount)
plot(d, main="Credit amount distribution")
polygon(d, col="red", border ="black")

amount <- data$amount
h <- hist(amount, breaks = 50, col="red", xlab="DM", main="Credit amount")

#Purpose
Purpose = data['purpose']
grid.newpage()
plotByPurpose<-ggplot(data, aes(purpose))+geom_bar() +labs(x="Purpose", y="Count") + geom_text(stat='count', aes(label=..count..), vjust=-1)
grid.arrange(plotByPurpose)

#Major purposes
topPurposeNames <- c("car (new)", "car (used)", "business", "furniture", "radio/tv")
topPurpose=subset(data, purpose %in% topPurposeNames)

ggplot(topPurpose, aes(x = amount)) + geom_histogram(aes(color = purpose), fill = "white", position = "identity", bins = 30)
