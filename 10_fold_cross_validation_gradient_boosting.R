install.packages("irr")
library(irr)
set.seed(123)
folds <- createFolds(water_potability$Potability, k=2)

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
