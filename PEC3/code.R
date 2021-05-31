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
ggplot(topPurpose, aes(amount)) + geom_histogram(aes(color = amount), bins=20)+facet_wrap(~purpose)


#ARBRE DE DECISIO
#Split of data (train/test)

# if(!require(caTools)){
#   install.packages('caTools')
#   library(caTools)
# }
# set.seed(101)
# sample = sample.split(data$default, SplitRatio = 0.75)
# train = subset(data, sample == TRUE)
# testS = subset(data,sample == FALSE)
# test <- testS[, -which(names(testS) %in% c("default"))]


# set.seed(666)
# y <- data[,21]
# X <- data[,-which(names(data) %in% c("default"))]
# 
# split_prop <- 3
# max_split<-floor(nrow(X)/split_prop)
# tr_limit <- nrow(X)-max_split
# ts_limit <- nrow(X)-max_split+1
# 
# trainX <- X[1:tr_limit,]
# trainy <- y[1:tr_limit]
# testX <- X[ts_limit+1:nrow(X),]
# testy <- y[ts_limit+1:nrow(X)]
# 
# summary(trainX)
# summary(trainy)
# summary(testX)
# summary(testy)
# 
# trainy = as.factor(trainy)
# model <- C50::C5.0(trainX, trainy,rules=TRUE )
# summary(model)
# 
# 
# model <- C50::C5.0(trainX, trainy)
# plot(model)


table(data$default)
set.seed(101)
train_indexes <- sample(1000, 800)
credit_train <- data[train_indexes, ]
credit_test  <- data[-train_indexes, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
library(C50)
credit_train$default<-as.factor(credit_train$default)
model <- C50::C5.0(credit_train[-17], credit_train$default)
summary(model)
model_withRules <- C50::C5.0(credit_train[-17], credit_train$default,rules = TRUE)
summary(model_withRules)
plot(model)

#EVALUATION
predicted_model <- predict(model, credit_test, type="class")
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model == train_indexes) / length(predicted_model)))
summary(predicted_model)

if(!require(gmodels)){
  install.packages('gmodels', repos='http://cran.us.r-project.org')
  library(gmodels)
}
CrossTable(credit_test$default, predicted_model,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Reality', 'Predicted'))


#MODELS COMPLEMENTARIS
model2 <- C50::C5.0(credit_train[-17], credit_train$default, trials=10, rules = TRUE)
summary(model2)
predicted_model2 <- predict(model2, credit_test, type="class")
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model2 == train_indexes) / length(predicted_model2)))
summary(predicted_model2)
CrossTable(credit_test$default, predicted_model2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Reality', 'Predicted'))
