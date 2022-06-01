install.packages("dplyr")
install.packages("forcats")
install.packages("ggplot2")
install.packages("skimr")
install.packages("scales")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("varImp")
install.packages("GGally")
install.packages(c("FactoMineR", "factoextra")) #da slide
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("xgboost")
install.packages("caTools")
install.packages("gbm")
install.packages("irr")
install.packages("C50")
install.packages("caret")
install.packages("ROCR")
install.packages("pROC")

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
library(irr)
library(xgboost)
library(caTools)
library(caret)
library(gbm)
library(C50)
library(caret)
library(ROCR)
library(pROC)

#-------------------  DATASET EXPLORATION ---------------------------

#load dataset
water_potability <- read.csv("water_potability.csv")

#target variable from numeric to factor
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
#--------------------------------------------------------------


#-------------------------  PCA  -------------------------------

#PCA
pca.res <- PCA(water_potability[1:9], graph = TRUE)

#eigenvalues
eig.val <- get_eigenvalue(pca.res)

fviz_eig(pca.res, addlabels = TRUE, ylim = c(0, 20)) #scree plot

# Contributions to the principal components
var <- get_pca_var(pca.res)
head(var$contrib)
print(var$contrib)


ind <- get_pca_ind(pca.res)
fviz_pca_ind(pca.res)

fviz_pca_ind(pca.res, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE # Avoid text overlapping (slow if many points)
             )

name <- list(c("Solids","Sulfate", "ph"))
fviz_pca_var(pca.res , select.var = list(cos2=3))

#---------------------------------------------------------------

#-------------------------  DECISION TREE  -------------------------------

#Decision Tree (DT)
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

table(trainset$Potability)
prop.table(table(trainset$Potability))

#add prediction column to testset
testset$Prediction = rep(0, 982)
testset$Prediction = factor(testset$Prediction)

#confusion matrix
confusion.matrix = table(testset$Potability, testset$Prediction)

#calculate accuracy
sum(diag(confusion.matrix))/sum(confusion.matrix)

#---------------------------------------------------------------

#-------------------------  GRADIENT BOOSTING  -------------------------------
allset = split.data(water_potability, p=0.7)
trainset = allset$train
testset = allset$test

y_train <- as.integer(trainset$Potability) - 1 #togliamo 1 perchè xgb.DMatrix richiewde valori che partono da 0, as.integer restituisce valori che partono da 1
y_test <- as.integer(testset$Potability) - 1
X_train <- trainset %>% select(-Potability)
X_test <- testset %>% select(-Potability)

xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

#default parameters
xgb_params <- list(
  booster = "gbtree", 
  objective = "multi:softprob", 
  eta=0.3, 
  gamma=0, 
  max_depth=6, 
  num_class = length(levels(water_potability$Potability)), 
  min_child_weight=1, 
  subsample=1, 
  colsample_bytree=1)

#printa il valore ideale per nround con  parametri default
xgbcv <- xgb.cv( 
  params = xgb_params, 
  data = xgb_train, 
  nrounds = 100, 
  nfold = 5, 
  showsd = T, 
  stratified = T, 
  print_every_n = 10, 
  early_stopping_rounds = 20, 
  maximize = F)

#valore ideale 20
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 20,
  verbose = 1,
)

xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)

xgb_preds <- as.data.frame(xgb_preds)

colnames(xgb_preds) <- levels(water_potability$Potability)
head(xgb_preds)
xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(water_potability$Potability)[y_test + 1]
head(xgb_preds)

confusionMatrix(as.factor(xgb_preds$PredictedClass), as.factor(xgb_preds$ActualClass))

#view variable importance plot
mat <- xgb.importance (feature_names = colnames(xgb_train),model = xgb_model)
xgb.plot.importance (importance_matrix = mat[1:20]) 

#---------------------------------------------------------------

#------------------------------  10 FOLD CROSS VALIDATION - DECISION TREE  ---------------------------------

set.seed(123)
folds <- createFolds(water_potability$Potability, k=10) #just 2 folds

results <- lapply(folds, function(x) {
  credit_train <- water_potability[-x, ]
  credit_test <- water_potability[x, ]

  credit_model <- rpart(Potability ~ Sulfate + ph, data=credit_train, method="class")

  preds <- predict(credit_model, credit_test, reshape = TRUE)
  preds <- as.data.frame(preds)
  colnames(preds) <- levels(water_potability$Potability)

  preds$PredictedClass <- apply(preds, 1, function(y) colnames(preds)[which.max(y)])
  preds$ActualClass <- credit_test$Potability
  preds$PredictedClass <- as.factor(preds$PredictedClass)
  preds$ActualClass <- as.factor(preds$ActualClass)
  
  return(confusionMatrix(preds$PredictedClass, preds$ActualClass))
})
results


rfConfusionMatrixFinal <- results$Fold1$table + results$Fold2$table

#accuracy
accuracy <- (rfConfusionMatrixFinal[1,1] + rfConfusionMatrixFinal[2,2])/(rfConfusionMatrixFinal[1,1] + 
                                                                           rfConfusionMatrixFinal[2,2] + 
                                                                           rfConfusionMatrixFinal[1,2] + 
                                                                           rfConfusionMatrixFinal[2,1])
print(accuracy)

#precision
precision <- rfConfusionMatrixFinal[1,1]/(rfConfusionMatrixFinal[1,1] +
                                            rfConfusionMatrixFinal[1,2])

print(precision)

#recall
recall <- rfConfusionMatrixFinal[1,1]/(rfConfusionMatrixFinal[1,1] +
                                            rfConfusionMatrixFinal[2,1])

print(recall)

#f-measure
f_measure <- ((2*precision*recall)/(precision + recall))
print(f_measure)

#---------------------------------------------------------------

#--------------------------------  10 FOLD CROSS VALIDATION - GRADIENT BOOSTING  ------------------------------------

set.seed(123)
folds <- createFolds(water_potability$Potability, k=10)

results <- lapply(folds, function(x) {
  credit_train <- water_potability[-x, ]
  credit_test <- water_potability[x, ]
  y_credit_train <- as.integer(credit_train$Potability) - 1 #togliamo 1 perchè xgb.DMatrix richiewde valori che partono da 0, as.integer restituisce valori che partono da 1
  y_credit_test <- as.integer(credit_test$Potability) - 1
  complete_test_set <- credit_test
  
  credit_train <- credit_train %>% select(-Potability)
  credit_test <- credit_test %>% select(-Potability)

  xgb_credit_train <- xgb.DMatrix(data = as.matrix(credit_train), label = y_credit_train)
  xgb_params <- list(
    booster = "gbtree", 
    objective = "multi:softprob", 
    eta=0.3, 
    gamma=0, 
    max_depth=6, 
    num_class = length(levels(water_potability$Potability)), 
    min_child_weight=1, 
    subsample=1, 
    colsample_bytree=1)
  
  #valore ideale 20
  credit_model <- xgb.train(
    params = xgb_params,
    data = xgb_train,
    nrounds = 20,
    verbose = 1,
  )
  preds <- predict(credit_model, as.matrix(credit_test), reshape = TRUE)
  preds <- as.data.frame(preds)
  
  colnames(preds) <- levels(water_potability$Potability)
  
  preds$PredictedClass <- apply(preds, 1, function(y) colnames(preds)[which.max(y)])
  preds$ActualClass <- complete_test_set$Potability
  preds$PredictedClass <- as.factor(preds$PredictedClass)
  preds$ActualClass <- as.factor(preds$ActualClass)
  
  return(confusionMatrix(preds$PredictedClass, preds$ActualClass))
})
results


rfConfusionMatrixFinal <- results$Fold1$table + results$Fold2$table

#accuracy
accuracy <- (rfConfusionMatrixFinal[1,1] + rfConfusionMatrixFinal[2,2])/(rfConfusionMatrixFinal[1,1] + 
                                                                           rfConfusionMatrixFinal[2,2] + 
                                                                           rfConfusionMatrixFinal[1,2] + 
                                                                           rfConfusionMatrixFinal[2,1])
print(accuracy)

#precision
precision <- rfConfusionMatrixFinal[1,1]/(rfConfusionMatrixFinal[1,1] +
                                            rfConfusionMatrixFinal[1,2])
print(precision)

#recall
recall <- rfConfusionMatrixFinal[1,1]/(rfConfusionMatrixFinal[1,1] +
                                         rfConfusionMatrixFinal[2,1])
print(recall)

#f-measure
f_measure <- ((2*precision*recall)/(precision + recall))
print(f_measure)

#----------------------------------------------------------------------------------------------

#------------------------------ROC e AUC---------------------------------

y_train <- as.integer(trainset$Potability) - 1 #togliamo 1 perchè xgb.DMatrix richiewde valori che partono da 0, as.integer restituisce valori che partono da 1
y_test <- as.integer(testset$Potability) - 1
X_train <- trainset %>% select(-Potability)
X_test <- testset %>% select(-Potability)

xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

#default parameters
xgb_params <- list(
  booster = "gbtree", 
  objective = "multi:softprob", 
  eta=0.3, 
  gamma=0, 
  max_depth=6, 
  num_class = length(levels(water_potability$Potability)), 
  min_child_weight=1, 
  subsample=1, 
  colsample_bytree=1)

#valore ideale 23
GB.model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 20,
  verbose = 1,
)

GB.pred <- predict(GB.model, as.matrix(X_test), reshape = TRUE)

#pred.prob = attr(DT.pred, "probabilities") #Obtain the probability of labels with “yes” ("1"):
pred.to.roc = GB.pred[, 2]#pred.prob[, 2] #prendo le probabilità di 1

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

dt.model= train(Potability ~ ., data = trainset, method = "rpart", metric =
                   "ROC", trControl = control)#train DT

xgb.model= train(Potability ~ ., data = trainset, method = "xgbTree", metric = "ROC",
                   trControl = control) #train xgb

dt.probs = predict(dt.model, testset[,! names(testset) %in% c("Potability")],type = "prob")#predictions on DT
xgb.probs = predict(xgb.model, testset[,! names(testset) %in% c("Potability")],type = "prob") #predictions on xgb

#generate ROC for both models and plot together
dt.ROC = roc(response = testset$Potability, predictor =dt.probs$potable,
              levels = levels(testset$Potability))
plot(dt.ROC,type="S", col="green")

xgb.ROC = roc(response = testset$Potability, predictor =xgb.probs$potable,
                levels = levels(testset$Potability))
plot(xgb.ROC, add=TRUE, col="blue")


#compare AUC
dt.ROC
xgb.ROC

#values
cv.values = resamples(list(dt = dt.model, xgb = xgb.model))
summary(cv.values)
dotplot(cv.values, metric = "ROC") 
bwplot(cv.values, layout = c(3, 1)) 
splom(cv.values,metric="ROC")
cv.values$timings #time to train

#---------------------------------------------------------------