install.packages("irr")
library(irr)
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
library(caret)

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


#10 FOLD CROSS VALIDATION
set.seed(123)
folds <- createFolds(water_potability$Potability, k=2) #just 2 folds

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
