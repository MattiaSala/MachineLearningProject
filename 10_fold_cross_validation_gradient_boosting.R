
install.packages("irr")
library(irr)
set.seed(123)
folds <- createFolds(water_potability$Potability, k=2)
results <- lapply(folds, function(x) {
  credit_train <- water_potability[-x, ]
  credit_test <- water_potability[x, ]
  
  y_credit_train <- as.integer(credit_train$Potability) - 1 #togliamo 1 perchè xgb.DMatrix richiewde valori che partono da 0, as.integer restituisce valori che partono da 1
  y_credit_test <- as.integer(credit_test$Potability) - 1
  
  credit_train <- credit_train %>% select(-Potability)
  credit_test <- credit_test %>% select(-Potability)
  print(credit_train)
  xgb_credit_train <- xgb.DMatrix(data = as.matrix(credit_train), label = y_credit_train)
  credit_model <- xgb.train(
    params = xgb_params,
    data = xgb_credit_train,
    nrounds = 5000,
    verbose = 1,
  )
  preds <- predict(credit_model, as.matrix(credit_test), reshape = TRUE)
  #preds <- as.data.frame(preds)
  return(data.frame(preds, real = water_potability$Potability[y_credit_test + 1]))
  #credit_pred$PredictedClass <- apply(credit_pred, 1, function(y) colnames(credit_pred)[which.max(y)])
  #credit_pred$ActualClass <- levels(water_potability$Potability)[y_credit_test + 1]
  #kappa <- kappam.fleiss(data.frame(credit_actual, credit_pred))$value
  #accuracy <- sum(credit_pred$PredictedClass == credit_pred$ActualClass) / nrow(credit_pred)
  #return(accuracy)
})
results
#confusion matrix confusion.matrix = table(testset$Potability, testset$Prediction)
confusionMatrix(results$preds, results$real)
