library(tidyverse)
library(tidyquant)
library(dplyr)

#function to merge all the files in the given address
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y, by = c("Date"), all = T)}, datalist)
}  

#merging all the independent variable
data2 <- multmerge("C:/Users/Dhiraj/Desktop/R/Project/Test")
fix(data2)
#data2$ <- NULL

#arranging data2 by date.
data2$Date <- lubridate::dmy(data2$Date)
data2 <- dplyr::arrange(data2, data2$Date)
data2 <- data2[-1,]
fix(data2)

#Copy the price of EURO STOCK

#to deal with NA values of sensex
#this code is not complete.
for(i in 1:nrow(data2)){
  if(is.na(data2$Movement[i]) == T){
    data2 <-  data2[-i,]
    }
}
fix(data2)

#To deal with NA values
for(j in 1:ncol(data2)){
  for(i in 1:nrow(data2)) {
    if(is.na(data2[i,j]) == T)
      data2[i,j] <- data2[i-1,j]
  }}

fix(data2)

#adding movement variable, to be used as dependent variable in the binary logistic model.

for(i in 1:nrow(data2)){
  if(i != nrow(data2)){
  data2$Movement[i] <- ifelse(data2$Return_Sensex[i + 1] < data2$Return_Sensex[i], 0, 1)
  }
  else
    break
}

fix(data2)

#write.csv(x = data2, "Test_data.csv") 

final1 <- read.csv("C:/Users/Dhiraj/Desktop/R/Project/final_Return4.csv")
str(final1)
summary(final1)
fix(final1)
final1$X <- NULL
final1$X.1 <- NULL
final1$X.2 <- NULL
final1$X.3 <- NULL
#final1 <- final1[-246,]
#attach(final1)
final2 <- subset(final1, select = c(-Date))
fix(final2)
str(final2)
#final1$Date <- lubridate::dmy(data2$Date)
#final1 <- dplyr::arrange(data2, data2$Date)
final_log_model <- glm(Movement~ ., data = final2, family = binomial)
summary(final_log_model)

#global testing
null <- glm(Movement~1, data = final1, family = binomial)
anova(final_log_model, null, test = "Chisq")


library(ROCR)
final2$predprob <- fitted(final_log_model)
pred <- prediction(final2$predprob, final2$Movement)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)
auc <- performance(pred, "auc")
auc@y.values
#69.97

final2$predprob <- NULL

#hosmer lemeshow test
library(ResourceSelection)
hltest <- hoslem.test(final1$Movement, fitted(final_log_model), g = 10)
hltest

library(car)
vif(final_log_model)

#Variable Selection
null <- glm(Movement~1, family = binomial, data = final2)
full <- glm(Movement~., family = binomial, data = final2)
step(full, scope = list(lower = null, upper = full), direction = 'both')
#Sensex, ftse, nifty, hongkong, nikkei, australia, shanghai, kospi
#ftse, hongkong, australia, shanghai

#model validation
#Hold-out Validation
library(caret)
index <- createDataPartition(final2$Movement, p = 0.7, list = F)
traindata <- final2[index,]
testdata <- final2[-index,]

valid_model <- glm(Movement~., family = binomial, data = traindata)
traindata$predprob <- predict(valid_model, traindata, type = 'response')
traindata$predY <- ifelse(traindata$predprob>0.5,1,0)
confusionMatrix(traindata$predY, traindata$Movement, positive = '1')

testdata$predprob <- predict(valid_model, testdata, type = 'response')
testdata$predY <- ifelse(testdata$predprob>0.5,1,0)
confusionMatrix(testdata$predY, testdata$Movement, positive = '1')

pred <- prediction(traindata$predprob, traindata$Movement)
perf <- performance(pred, 'tpr', 'fpr')
plot(perf)
abline(0,1)
auc <- performance(pred, 'auc')
auc@y.values

pred <- prediction(testdata$predprob, testdata$Movement)
perf <- performance(pred, 'tpr', 'fpr')
plot(perf)
abline(0,1)
auc <- performance(pred, 'auc')
auc@y.values


#SVM
library(e1071)
svm_data <- read.csv("C:/Users/Dhiraj/Desktop/R/Project/final_Return.csv")
fix(svm_data)
final1$X <- NULL
final1$X.1 <- NULL
final1$X.2 <- NULL
final1$X.3 <- NULL
svm_data2 <- subset(svm_data, select = c(-Date))

svm_model <- svm(Movement ~., data = svm_data2, type = 'C', probability = T, kernel = 'linear')
svm_pred1 <- predict(svm_model, svm_data2,probability = T)
svm_pred2 <- attr(svm_pred1, 'probabilities')[,2]
svm_pred <- prediction(svm_pred2, final2$Movement)
svm_perf <- performance(svm_pred, 'tpr', 'fpr')
plot(svm_perf)
abline(0, 1)
svm_auc <- performance(svm_pred, 'auc')
svm_auc@y.values #70.022

svm_data2$predprob <- predict(svm_model, svm_data2, probability = T)
library(caret)
confusionMatrix(svm_data2$predprob, svm_data2$Movement, positive = '1') #accuracy: 63.52
svm_data2$predprob <- NULL

#Cross-Validation
library(caret)
svm_index <- createDataPartition(svm_data2$Movement, p = 0.7, list = F)
svm_traindata <- svm_data2[svm_index,]
svm_testdata <- svm_data2[-svm_index,]

svm_train_model <- svm(Movement ~., data = svm_traindata, type = 'C', probability = T, kernel = 'linear')
svm_traindata$predprob <- predict(svm_train_model, svm_traindata, probability = T)
confusionMatrix(svm_traindata$predprob, svm_traindata$Movement, positive = '1')
svm_traindata$predprob <- NULL

svm_traindata_predprob <- predict(svm_train_model, svm_traindata, probability = T)
#svm_traindata_predprob
svm_train_pred2 <- attr(svm_traindata_predprob, 'probabilities')[,2]
svm_train_pred <- prediction(svm_train_pred2, svm_traindata$Movement)
svm_train_perf <- performance(svm_train_pred, 'tpr', 'fpr')
plot(svm_train_perf)
abline(0, 1)
svm_train_auc <- performance(svm_train_pred, 'auc')
svm_train_auc@y.values

svm_testdata$predprob <- predict(svm_train_model, svm_testdata, probability = T)
confusionMatrix(svm_testdata$predprob, svm_testdata$Movement, positive = '1')
svm_traindata$predprob <- NULL

svm_testdata_predprob <- predict(svm_train_model, svm_testdata, probability = T)
svm_test_pred2 <- attr(svm_testdata_predprob, 'probabilities')[,2]
svm_test_pred <- prediction(svm_test_pred2, svm_testdata$Movement)
svm_test_perf <- performance(svm_test_pred, 'tpr', 'fpr')
plot(svm_test_perf)
abline(0, 1)
svm_test_auc <- performance(svm_test_pred, 'auc')
svm_test_auc@y.values



#Naive Bayes Method
library(e1071)
naive_data <- read.csv("C:/Users/Dhiraj/Desktop/R/Project/final_Return4.csv")
fix(naive_data)
naive_data$X <- NULL
naive_data$X.1 <- NULL
naive_data$X.2 <- NULL
naive_data$X.3 <- NULL
naive_data2 <- subset(naive_data, select = c(-Date))
fix(naive_data2)
naive_model <- naiveBayes(Movement~., data = final2)
naive_model

prednb <- predict(naive_model, naive_data2, type='raw')
pred <- prediction(prednb[,2], final2$Movement)
perf <- performance(pred, 'tpr', 'fpr')
plot(perf)
abline(0,1)
auc <- performance(pred, 'auc')
auc@y.values
#65.18
predY <- ifelse(prednb[,2]>0.5,1,0)
confusionMatrix(predY, final2$Movement)

#Cross-Validation
library(caret)
naive_index <- createDataPartition(naive_data2$Movement, p = 0.7, list = F)
naive_traindata <- naive_data2[naive_index,]
naive_testdata <- naive_data2[-naive_index,]

naive_train_model <- naiveBayes(Movement~., data = naive_traindata)
naive_traindata$predprob <- predict(naive_train_model, naive_traindata, type = 'raw')
naive_traindata$predY <- ifelse(naive_traindata$predprob[,2]>0.5,1,0)
confusionMatrix(naive_traindata$predY, naive_traindata$Movement, positive = '1')
naive_traindata$predprob <- NULL
naive_traindata$predY <- NULL

naive_traindata_predprob <- predict(naive_train_model, naive_traindata, type = 'raw')
naive_train_pred <- prediction(naive_traindata_predprob[,2], naive_traindata$Movement)
naive_train_perf <- performance(naive_train_pred, 'tpr', 'fpr')
plot(naive_train_perf)
abline(0, 1)
naive_train_auc <- performance(naive_train_pred, 'auc')
naive_train_auc@y.values

naive_testdata$predprob <- predict(naive_train_model, naive_testdata, type = 'raw')
naive_testdata$predY <- ifelse(naive_testdata$predprob[,2]>0.5,1,0)
confusionMatrix(naive_testdata$predY, naive_testdata$Movement, positive = '1')
naive_testdata$predprob <- NULL
naive_testdata$predY <- NULL

naive_testdata_predprob <- predict(naive_train_model, naive_testdata, type = 'raw')
naive_test_pred <- prediction(naive_testdata_predprob[,2], naive_testdata$Movement)
naive_test_perf <- performance(naive_test_pred, 'tpr', 'fpr')
plot(naive_test_perf)
abline(0, 1)
naive_test_auc <- performance(naive_test_pred, 'auc')
naive_test_auc@y.values

#KNN Classifier
library(caret)
knn_data <- read.csv("C:/Users/Dhiraj/Desktop/R/Project/final_Return4.csv")
fix(knn_data)
knn_data$X <- NULL
knn_data$X.1 <- NULL
knn_data$X.2 <- NULL
knn_data$X.3 <- NULL
knn_data2 <- subset(knn_data, select = c(-Date))
knn_data3 <- subset(knn_data2, select = c(-Movement))
fix(knn_data3)

library(class)
library(caret)
knn_index <- createDataPartition(knn_data2$Movement, p = 0.7, list = F)
knn_traindata <- knn_data3[knn_index,]
knn_testdata <- knn_data3[-knn_index,]
knn_ytrain <- knn_data2$Movement[knn_index]
knn_ytest <- knn_data2$Movement[-knn_index]

knnmodel <- knn(knn_traindata, knn_testdata, k = cl=knn_ytrain)
table(knn_ytest, knnmodel)

naive_train_model <- naiveBayes(Movement~., data = naive_traindata)
naive_traindata$predprob <- predict(naive_train_model, naive_traindata, type = 'raw')
naive_traindata$predY <- ifelse(naive_traindata$predprob[,2]>0.5,1,0)
confusionMatrix(naive_traindata$predY, naive_traindata$Movement, positive = '1')
naive_traindata$predprob <- NULL
naive_traindata$predY <- NULL

naive_traindata_predprob <- predict(naive_train_model, naive_traindata, type = 'raw')
naive_train_pred <- prediction(naive_traindata_predprob[,2], naive_traindata$Movement)
naive_train_perf <- performance(naive_train_pred, 'tpr', 'fpr')
plot(naive_train_perf)
abline(0, 1)
naive_train_auc <- performance(naive_train_pred, 'auc')
naive_train_auc@y.values

naive_testdata$predprob <- predict(naive_train_model, naive_testdata, type = 'raw')
naive_testdata$predY <- ifelse(naive_testdata$predprob[,2]>0.5,1,0)
confusionMatrix(naive_testdata$predY, naive_testdata$Movement, positive = '1')
naive_testdata$predprob <- NULL
naive_testdata$predY <- NULL

naive_testdata_predprob <- predict(naive_train_model, naive_testdata, type = 'raw')
naive_test_pred <- prediction(naive_testdata_predprob[,2], naive_testdata$Movement)
naive_test_perf <- performance(naive_test_pred, 'tpr', 'fpr')
plot(naive_test_perf)
abline(0, 1)
naive_test_auc <- performance(naive_test_pred, 'auc')
naive_test_auc@y.values

######################################################################################################################
#as model is suffering from high multicollinearity, we have to go for PCA
final3 <- subset(final2, select = c(-Movement))
head(final3)
pc <- princomp(final3, cor = T, scores = T)
pc$loadings
summary(pc)
names(pc)

#predicting using PCA
train.data <- data.frame(final2$Movement, pc$scores[,1:2])
head(train.data)
final_log_model_pca <- glm(final2.Movement~., data = train.data, family = binomial)
summary(final_log_model_pca)

null <- glm(final2.Movement~1, data = train.data, family = binomial)
anova(final_log_model_pca, null, test = "Chisq")

hltest_pca <- hoslem.test(train.data$final2.Movement, fitted(final_log_model_pca), g = 10)
hltest_pca

library(car)
vif(final_log_model_pca)

train.data$predprob <- fitted(final_log_model_pca)
pred <- prediction(train.data$predprob, train.data$final2.Movement)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)
auc <- performance(pred, "auc")
auc@y.values