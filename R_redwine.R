library(fscaret)            #201611047 Irem ÖZTÜRK
library(MASS)               #201711006 Mert BAYRAMUSTA
library(dplyr)
library(tidyverse)
library(cluster)    
library(factoextra)
library(fpc)
library(NbClust)
library(dendextend) 
library(mlbench)
library(caret)
library(lattice)

set.seed(12345) #random number generator, which is useful for creating simulations or random objects that can be reproduced.

setwd("C:/Users/Mert/Desktop")
MyDATA <- read.csv(file = "winequality-red.csv", header = TRUE, sep = ";", na.strings = c("","NULL", "PrivacySuppressed", "NA"))
dim(MyDATA)

summary(MyDATA)

class(MyDATA)

for(i in 1:5){ #removing all characther attributes they are kind of useless.
  MyDATA <- MyDATA[-i] 
  
}

duplicated(MyDATA) #looking if there is a duplicated data
#there are some duplicated datas in the table

#CORRELATION PART
correlationMatrix <- cor(MyDATA[,1:7])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75) #found the attributes that is highly correlated
print(highlyCorrelated) #printing highly correlated attributes because highly correlated = not good for calculating clusters

#removing outliers from out attributes  (OUTLIER PART)
for(i in 1:7) {
  testdata<-MyDATA[,i] #all attributes enteres this step
  
  #first we can see our boxplot WITH outliers
  b<-boxplot(testdata, main=names(MyDATA)[i],vertical = T)  
  
  #find extremes from the boxplot's stats output(which is lowerwhisker, Q1, Median, Q3, upperwhisker)
  lowerwhisker<-b$stats[1]
  upperwhisker<-b$stats[5]
  
  lh <- quantile(testdata,probs=0.25)	
  uh <- quantile(testdata,probs=0.75)	
  step<- 1.5 * (uh-lh)
  #remove the extremes
  testdata<-testdata[testdata>lowerwhisker & testdata<(upperwhisker-(1.5 * step))]
  #now, we can see our boxplot/plot/histogram/barplot WITHOUT outliers(also colored with our favourite color)
  par(mfrow = c(1, 4))
  boxplot(testdata, col = 6, main=names(MyDATA)[i],vertical = T)
  plot(density(testdata),main=names(MyDATA)[i])
  hist(MyDATA[,i], main=names(MyDATA)[i])
  barplot(table(MyDATA[,i]), main=names(MyDATA)[i])
}
#Mosaic plot but for our correlated data set
mosaicplot(MyDATA, xlab = MyDATA$alcohol, ylab = MyDATA$volatile.acidity, shade=TRUE, legend=TRUE)


#To show scatter plot with chosen 2 variables 
library(ggplot2)

ggplot(MyDATA, aes(x=MyDATA$alcohol, y=MyDATA$volatile.acidity)) + 
  geom_point()

#------CLASSIFICATION PART------

classData = MyDATA
head(classData)
#creating both the training and testing data sets to develop a model.
inTrain = createDataPartition(y = classData$quality, p = .75, list = FALSE)
training = classData[inTrain,]
testing = classData[-inTrain,] 

#Classification with RPART

library(rpart)
MyDATA_dtree <- rpart(quality ~ ., data = training, method="class")
summary(MyDATA_dtree)
# check the prediction
pred<-predict(MyDATA_dtree, training[, 1:7], type="class")
table(training$quality,pred)
confusionMatrix(table(training$quality,pred))
# plot tree 
plot(MyDATA_dtree, main="Classification Tree ")
text(MyDATA_dtree, use.n=TRUE, all=TRUE)

#classification with KNN

#cross validation 10 and k = 5
ctrl <- trainControl(method="repeatedcv",repeats = 5)
knnFit <- train(quality ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit #Output of kNN fit
kNNPredictions <-predict(knnFit, testing)
cmkNN <-table(kNNPredictions, testing$quality)
accuracykNN = (sum(diag(cmkNN)))/sum(cmkNN)
accuracykNN

#classification with NEURAL NETWORKS

NNModel <- train(quality ~ ., data = training, method = "nnet",preProcess=c("scale","center"), na.action = na.omit)
NNPredictions <-predict(NNModel, testing)
NNModel
cmNN <-table(NNPredictions, testing$quality)
accuracyNN = (sum(diag(cmNN)))/sum(cmNN)
accuracyNN


