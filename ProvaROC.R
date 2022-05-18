install.packages("C50")
library(C50)
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)
install.packages("pROC ")
library(pROC)

library(dplyr)
library(forcats)
library(ggplot2)
library(skimr)
library(scales)
library(tidyverse)
library(ggpubr)
library(corrplot)
library(RColorBrewer)
library(varImp)
library(GGally)
library(FactoMineR) 
library(factoextra)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


water_potability <- read.csv("water_potability.csv")

water_potability$Potability <- as.factor(water_potability$Potability)
print(water_potability)

water_potability %>% summarise_all(~ sum(is.na(.)))

#Remove NA values and substitute them with mean
water_potability <- water_potability %>% 
  group_by(Potability) %>%
  mutate(across(where(is.numeric), 
                ~if_else(is.na(.), 
                         mean(., na.rm = T),   
                         as.numeric(.)))) %>% 
  ungroup()

#check if there is any NA value
water_potability %>% summarise_all(~ sum(is.na(.)))

split.data = function(data, p = 0.7, s = 1){ #create function to split data
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train=train, test=test))
}

allset = split.data(water_potability, p=0.7)
trainset = allset$train
testset = allset$test


#train DT
DT.model <- rpart(Potability ~ Sulfate + ph, data=trainset, method="class")
DT.pred <- predict(DT.model, testset, type="prob")
#pred.prob = attr(DT.pred, "probabilities") #Obtain the probability of labels with “yes” ("1"):
pred.to.roc = DT.pred[, 2]#pred.prob[, 2] #prendo le probabilità di 1

pred.rocr = prediction(pred.to.roc, testset$Potability) #Use the prediction function to generate a prediction result

perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")

plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))

abline(a=0, b=1) #random classifier


#optimal cutoff
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

print(opt.cut(perf.tpr.rocr, pred.rocr))


#overall accuracy
acc.perf = performance(pred.rocr, measure = "acc")
plot(acc.perf)


#maximum accuracy
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))


#comparing models
control = trainControl(method = "repeatedcv", number = 10,repeats = 3,
                       classProbs = TRUE, summaryFunction = twoClassSummary)

svm.model= train(churn ~ ., data = trainset, method = "svmRadial", metric =
                   "ROC", trControl = control)#train SVM

rpart.model= train(churn ~ ., data = trainset, method = "rpart", metric = "ROC",
                   trControl = control) #train DT

svm.probs = predict(svm.model, testset[,! names(testset) %in% c("churn")],type = "prob")#predictions on SVM
rpart.probs = predict(rpart.model, testset[,! names(testset) %in% c("churn")],type = "prob") #predictions on DT


#generate ROC for both models and plot together
svm.ROC = roc(response = testset[,c("churn")], predictor =svm.probs$yes,
              levels = levels(testset[,c("churn")]))
plot(svm.ROC,type="S", col="green")

rpart.ROC = roc(response = testset[,c("churn")], predictor =rpart.probs$yes,
                levels = levels(testset[,c("churn")]))
plot(rpart.ROC, add=TRUE, col="blue")


#compare AUC
svm.ROC
rpart.ROC

#values
cv.values = resamples(list(svm=svm.model, rpart = rpart.model))
summary(cv.values)

cv.values$timings #time to train


