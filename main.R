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
install.packages(rpart)
install.packages(rattle)
install.packages(rpart.plot)
install.packages(RColorBrewer)


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
library(FactoMineR) #da slide
library(factoextra) #da slide
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
pca.res <- PCA(water_potability[1:9], ncp=6, graph = TRUE)

#eigenvalues
eig.val <- get_eigenvalue(pca.res)

fviz_eig(pca.res, addlabels = TRUE, ylim = c(0, 20)) #scree plot

var <- get_pca_var(pca.res)
head(var$coord, 6)

ind <- get_pca_ind(pca.res)

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

#TODO - better model (incomplete):
h0 <- subset(trainset, Potability==0) #subset of trainset where Potability = 0
summary(h0$Hardness) #Look at the mean of Hardness where Potability = 0

h1 <- subset(trainset, Potability==1) #subset of trainset where Potability = 1
summary(h1$Hardness) #Look at the mean of Hardness where Potability = 1

s0 <- subset(trainset, Potability==0) #subset of trainset where Potability = 0
summary(s0$Sulfate) #Look at the mean of Sulfate where Potability = 0

s1 <- subset(trainset, Potability==1) #subset of trainset where Potability = 1
summary(s1$Sulfate) #Look at the mean of Sulfate where Potability = 1

#train DT and plot it:
decisionTree = rpart(Potability ~ Hardness + Sulfate, data=trainset, method="class")
fancyRpartPlot(decisionTree)

