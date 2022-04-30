install.packages("C50")
library(C50)
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)
install.packages("pROC ")
library(pROC)


data(churn)
churnTrain = churnTrain[,! names(churnTrain) %in% c("state", "area_code", "account_length") ]
set.seed(2)
ind = sample(2, nrow(churnTrain), replace = TRUE, prob=c(0.7, 0.3))
trainset = churnTrain[ind == 1,]
testset = churnTrain[ind == 2,]


#SVM
svm.model= train(churn ~ ., data = trainset, method = "svmRadial")
svm.pred = predict(svm.model, testset[,! names(testset) %in% c("churn")])
table(svm.pred, testset[,c("churn")])

result = confusionMatrix(svm.pred, testset[,c("churn")])
result2 = confusionMatrix(svm.pred, testset[,c("churn")], mode = "prec_recall")


#ROC CURVE
svmfit=svm(churn~ ., data=trainset, prob=TRUE)
pred=predict(svmfit,testset[, !names(testset) %in% c("churn")], probability=TRUE)
pred.prob = attr(pred, "probabilities")
pred.to.roc = pred.prob[, 2]
pred.rocr = prediction(pred.to.roc, testset$churn)
perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")

plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))
abline(a=0, b=1)


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


