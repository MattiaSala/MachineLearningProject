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

#check outliers
outliers <- water_potability %>% 
  select(-Potability)
outliers[] <- lapply(outliers, function(x){
  qq <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  is.na(x) <-  x < (qq[1] -(3*(qq[2]-qq[1]))) | x > (qq[2]+(3*(qq[2]-qq[1])))
  x
})
outliers %>% summarise_all(~ sum(is.na(.)))

#replace outliers with mean
outliers <- outliers %>% 
  mutate(across(where(is.numeric), 
                ~if_else(is.na(.), 
                         mean(., na.rm = T),   
                         as.numeric(.)))) %>% 
  ungroup()
outliers %>% summarise_all(~ sum(is.na(.)))

outliers$Potability <- water_potability$Potability
water_potability <- outliers

#print plot to cheeck ouliers
water_potability %>%
  pivot_longer(cols = -Potability, names_to = "feature") %>%
  ggplot(aes(x = feature, y = value)) +
  geom_jitter(aes(y = value, col = Potability), alpha = 0.1) +
  geom_boxplot(aes(fill = Potability)) +
  facet_wrap(vars(feature), ncol = 3, scales = "free") +
  scale_color_manual(values = c("#E4652E", "#0E8A41")) +
  scale_fill_manual(values = c("#E4652E", "#0E8A41")) +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "#0B2D5B"),
    strip.text = element_text(color = "white", face = "bold", size = 8)
  ) +
  labs(
    title = "Valori anomali",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL,
    fill = NULL,
    color = NULL
  )

p1 <- ggplot(water_potability, aes(ph, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "pH", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom")+
  labs(title = "pH")
p2 <- ggplot(water_potability, aes(Hardness, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Hardness", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom")+ 
  labs(title = "Hardness")
p3 <- ggplot(water_potability, aes(Solids, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Solids", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Solids")
p4 <- ggplot(water_potability, aes(Chloramines, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Chloramines", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Chloramines")
p5 <- ggplot(water_potability, aes(Sulfate, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Sulfate", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Sulfate")
p6 <- ggplot(water_potability, aes(Conductivity, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Conductivity", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Conductivity")
p7 <- ggplot(water_potability, aes(Organic_carbon, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Organic Carbon", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Organic Carbon")
p8 <- ggplot(water_potability, aes(Trihalomethanes, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Trihalomethanes", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Trihalomethanes")
p9 <- ggplot(water_potability, aes(Turbidity, color = as.factor(Potability)))+
  geom_histogram(bins = 30, fill = "white") +
  labs(x = "Turbidity", y = "Count", col = "Potability") +
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "Turbidity")
figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, nrow = 3, ncol = 3, labels = "AUTO")

figure

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

#-------------------------  TRAIN&TEST and BASELINE MODEL  -------------------------------

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

#-------------------------  DECISION TREE  -------------------------------
decisionTree <- rpart(Potability ~ Sulfate + Solids + ph, data=trainset, method="class") #Sulfate + Solids + ph

fancyRpartPlot(decisionTree)
printcp(decisionTree)
plotcp(decisionTree)


#accuracy with trained DT
testset$Prediction <- predict(decisionTree, testset, type = "class")
confusion.matrix = table(testset$Potability, testset$Prediction)
sum(diag(confusion.matrix))/sum(confusion.matrix)

#after prune of tree
myPruned = prune(decisionTree, cp=.014) #014 #021 #solo sulfate = 018
fancyRpartPlot(myPruned)

testset$Prediction <- predict(myPruned, testset, type = "class")
confusion.matrix = table(testset$Potability, testset$Prediction)
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

#print best nround params
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

#best nround = 20
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 20,
  verbose = 1,
)

#predict
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
folds <- createFolds(water_potability$Potability, k=10)

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


rfConfusionMatrixFinal <- results$Fold01$table + 
  results$Fold02$table + 
  results$Fold03$table + 
  results$Fold04$table + 
  results$Fold05$table + 
  results$Fold06$table + 
  results$Fold07$table + 
  results$Fold08$table + 
  results$Fold09$table + 
  results$Fold10$table

confusionMatrix(rfConfusionMatrixFinal)

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
  fold_train <- water_potability[-x, ]
  fold_test <- water_potability[x, ]
  y_fold_train <- as.integer(fold_train$Potability) - 1 
  complete_test_set <- fold_test
  
  fold_train <- fold_train %>% select(-Potability)
  fold_test <- fold_test %>% select(-Potability)
  
  xgb_fold_train <- xgb.DMatrix(data = as.matrix(fold_train), label = y_fold_train)
  
  credit_model <- xgb.train(
    params = xgb_params,
    data = xgb_fold_train,
    nrounds = 20,
    verbose = 1,
  )
  preds <- predict(credit_model, as.matrix(fold_test), reshape = TRUE)
  preds <- as.data.frame(preds)
  
  colnames(preds) <- levels(water_potability$Potability)
  
  preds$PredictedClass <- apply(preds, 1, function(y) colnames(preds)[which.max(y)])
  preds$ActualClass <- complete_test_set$Potability
  preds$PredictedClass <- as.factor(preds$PredictedClass)
  preds$ActualClass <- as.factor(preds$ActualClass)
  
  return(confusionMatrix(preds$PredictedClass, preds$ActualClass))
})
results

#final confusion matrix
rfConfusionMatrixFinal <- results$Fold01$table + 
  results$Fold02$table + 
  results$Fold03$table + 
  results$Fold04$table + 
  results$Fold05$table + 
  results$Fold06$table + 
  results$Fold07$table + 
  results$Fold08$table + 
  results$Fold09$table + 
  results$Fold10$table

confusionMatrix(rfConfusionMatrixFinal)

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

y_train <- as.integer(trainset$Potability) - 1 
y_test <- as.integer(testset$Potability) - 1
X_train <- trainset %>% select(-Potability)
X_test <- testset %>% select(-Potability)

xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

#train Gradient boosting
GB.model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 20,
  verbose = 1,
)

GB.pred <- predict(GB.model, as.matrix(X_test), reshape = TRUE)

pred.to.roc = GB.pred[, 2]

pred.rocr = prediction(pred.to.roc, testset$Potability) #Use the prediction function to generate a prediction result

perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")

plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))

abline(a=0, b=1) #random classifier

#AUC DT

DT.model <- rpart(Potability ~ Sulfate + ph, data=trainset, method="class")

DT.pred <- predict(DT.model, trainset, reshape = TRUE)

pred.to.roc = DT.pred[, 2]

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

print(water_potability)
trainset$Potability <- as.numeric(trainset$Potability) - 1
trainset$Potability[trainset$Potability == 1] <- 'potable'
trainset$Potability[trainset$Potability == 0] <- 'notPotable'
trainset$Potability <- as.factor(trainset$Potability)

testset$Potability <- as.numeric(testset$Potability) - 1
testset$Potability[testset$Potability == 1] <- 'potable'
testset$Potability[testset$Potability == 0] <- 'notPotable'
testset$Potability <- as.factor(testset$Potability)

dt.model= train(Potability ~ ., data = trainset, method = "rpart", metric =
                   "ROC", trControl = control)#train DT

xgb.model= train(Potability ~ ., data = trainset, method = "xgbTree", metric = "ROC",
                   trControl = control) #train xgb

dt.probs = predict(dt.model, testset[,! names(testset) %in% c("Potability")],type = "prob")#predictions on DT
xgb.probs = predict(xgb.model, testset[,! names(testset) %in% c("Potability")],type = "prob") #predictions on xgb

#generate ROC for both models and plot together
dt.ROC = roc(response = testset$Potability, predictor =dt.probs$potable,
             levels = levels(testset$Potability))
plot(dt.ROC,type="S", col="green", main=paste('ROC compare'))

xgb.ROC = roc(response = testset$Potability, predictor =xgb.probs$potable,
              levels = levels(testset$Potability))
plot(xgb.ROC, add=TRUE, col="blue")
legend(0.3, 0.2, legend=c("Gradient boosting", "Decision tree"),
       col=c("blue", "green"), lty=1, cex=0.8)


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
