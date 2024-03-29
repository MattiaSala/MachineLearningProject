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

#--------------------------------------------------------------------------------------------------------------

#TODO - better model (incomplete):
mean_hardness_ts <- mean(trainset$Hardness)
var_hardness_ts <- var(trainset$Hardness)
sd_hardness <- sd(trainset$Hardness)
h0 <- subset(trainset, Potability==0) #subset of trainset where Potability = 0
summary(h0$Hardness) #Look at the mean of Hardness where Potability = 0

h1 <- subset(trainset, Potability==1) #subset of trainset where Potability = 1
summary(h1$Hardness) #Look at the mean of Hardness where Potability = 1


mean_sulfate_ts <- mean(trainset$Sulfate)
var_sulfate_ts <- var(trainset$Sulfate)
sd_sulfate <- sd(trainset$Sulfate)
s0 <- subset(trainset, Potability==0) #subset of trainset where Potability = 0
summary(s0$Sulfate) #Look at the mean of Sulfate where Potability = 0

s1 <- subset(trainset, Potability==1) #subset of trainset where Potability = 1
summary(s1$Sulfate) #Look at the mean of Sulfate where Potability = 1

#ANALISI ESPLORATIVA DT
#vedere in percentuale quanti elelemnti del trainset hanno valore > della media (di ciascuna variabile) se target == 0 | == 1
barplot(
  table(trainset$Potability, trainset$Sulfate), main="Sulfate",
)

hist(s0$Sulfate, main="Sulfate", xlab = "Sulfate")
hist(s1$Sulfate, main="Sulfate", xlab = "Sulfate")

s0_320_340 <- subset(s0, s0$Sulfate >= 320 & s0$Sulfate <= 340) #subset of trainset where Potability = 0
hist(s0_320_340$Sulfate, main="Sulfate 320-340", xlab = "Sulfate")
s1_300_350 <- subset(s1, s1$Sulfate >= 300 & s1$Sulfate <= 350) #subset of trainset where Potability = 0
hist(s1_300_350$Sulfate, main="Sulfate 300-350", xlab = "Sulfate")


sol_0 <- subset(trainset, Potability==0) #subset of trainset where Potability = 0
summary(sol_0$Solids) #Look at the mean of Sulfate where Potability = 0

sol_1 <- subset(trainset, Potability==1) #subset of trainset where Potability = 1
summary(sol_1$Solids) #Look at the mean of Sulfate where Potability = 1

barplot(
  table(trainset$Potability, trainset$Solids), main="Solids",
)

hist(sol_0$Solids, main="Solids pota 0", xlab = "Solids")
hist(sol_1$Solids, main="Solids pota 1", xlab = "Solids")

sol_0_10k_30k <- subset(s0, sol_0$Solids >= 10000 & sol_0$Solids <= 30000) #subset of trainset where Potability = 0
hist(sol_0_10k_30k$Solids, main="Solids 10k-30k", xlab = "Solids")

#--------------------------------------------------------------------------------------------------------------

#train DT and plot it:
decisionTree = rpart(Potability ~ Sulfate + ph, data=trainset, method="class")
fancyRpartPlot(decisionTree)
printcp(decisionTree)
plotcp(decisionTree)


#accuracy with trained DT
testset$Prediction <- predict(decisionTree, testset, type = "class")
confusion.matrix = table(testset$Potability, testset$Prediction)
sum(diag(confusion.matrix))/sum(confusion.matrix)

#after prune of tree
myPruned = prune(decisionTree, cp=.014)
fancyRpartPlot(myPruned)

testset$Prediction <- predict(myPruned, testset, type = "class")
confusion.matrix = table(testset$Potability, testset$Prediction)
sum(diag(confusion.matrix))/sum(confusion.matrix)

#IG
decisionTreeIG = rpart(Potability ~ Sulfate + ph, data=trainset, 
                       method="class", 
                       parms = list(split = 'information')
                 )
fancyRpartPlot(decisionTreeIG)
printcp(decisionTreeIG)
plotcp(decisionTreeIG)


#accuracy with trained DTIG
testset$Prediction <- predict(decisionTreeIG, testset, type = "class")
confusion.matrix = table(testset$Potability, testset$Prediction)
sum(diag(confusion.matrix))/sum(confusion.matrix)
#after prune of tree
myPruned = prune(decisionTreeIG, cp=.012)
fancyRpartPlot(myPruned)

testset$Prediction <- predict(myPruned, testset, type = "class")
confusion.matrix = table(testset$Potability, testset$Prediction)
sum(diag(confusion.matrix))/sum(confusion.matrix)


#Gradient Boosting
library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(gbm)

#water_potability$Potability[water_potability$Potability == 1] <- 'potable'
#water_potability$Potability[water_potability$Potability == 0] <- 'notPotable'
#water_potability$Potability <- as.factor(water_potability$Potability)

allset = split.data(water_potability, p=0.7)
trainset = allset$train
testset = allset$test

y_train <- as.integer(trainset$Potability) - 1 #togliamo 1 perchè xgb.DMatrix richiewde valori che partono da 0, as.integer restituisce valori che partono da 1
y_test <- as.integer(testset$Potability) - 1
X_train <- trainset %>% select(-Potability)
X_test <- testset %>% select(-Potability)
print(X_train)
head(water_potability$Potability)
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

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

#valore ideale 23
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 23,
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
