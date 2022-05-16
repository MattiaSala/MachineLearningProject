install.packages("C50")
library(C50)
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)
install.packages("pROC ")
library(pROC)


#data(water_potability)
#churnTrain = churnTrain[,! names(churnTrain) %in% c("state", "area_code", "account_length") ]
#set.seed(2)
#ind = sample(2, nrow(churnTrain), replace = TRUE, prob=c(0.7, 0.3))
#trainset = churnTrain[ind == 1,]
#testset = churnTrain[ind == 2,]

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
decisionTree.model <- rpart(Potability ~ Sulfate + ph, data=trainset, method="class")
decisionTree.pred <- predict(decisionTree.model, testset, reshape = TRUE)
table(decisionTree.pred, testset[,c("Potability")])

result = confusionMatrix(decisionTree.pred, testset[,c("Potability")])
result2 = confusionMatrix(decisionTree.pred, testset[,c("Potability")], mode = "prec_recall")


#ROC CURVE
DTfit = rpart(Potability ~ Sulfate + ph, data=trainset, method="class")
DTfit.preds <- predict(DTfit, testset, type="prob")[, 2]
roc_pred <- prediction(predict(DTfit, testset, type="prob")[, 2], water_potability$Potability)

perf.rocr = performance(roc_pred, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr = performance(roc_pred, "tpr","fpr")

plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))
#print(DTfit.roc)
plot(DTfit.roc)
abline(a=0, b=1)



#svmfit=svm(churn~ ., data=trainset, prob=TRUE)
#pred=predict(svmfit,testset[, !names(testset) %in% c("churn")], probability=TRUE)
#pred.prob = attr(pred, "probabilities")
#pred.to.roc = pred.prob[, 2]
#pred.rocr = prediction(pred.to.roc, testset$churn)
#perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
#perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")

#plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))
#abline(a=0, b=1)


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


