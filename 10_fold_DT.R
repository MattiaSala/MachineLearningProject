install.packages("irr")
library(irr)

set.seed(123)
folds <- createFolds(water_potability$Potability, k=10)
results <- lapply(folds, function(x) {
  credit_train <- water_potability[-x, ]
  credit_test <- water_potability[x, ]
  
  #add prediction column to testset
  n_row_test = nrow(credit_test)
  credit_test$Prediction = rep(0, n_row_test)
  credit_test$Prediction = factor(credit_test$Prediction)
  
  #train DT and plot it:
  decisionTree = rpart(credit_train ~ Sulfate + ph, data=trainset, method="class")
  credit_test$Prediction <- predict(decisionTree, credit_test, type = "class")
  
  return matrice 
})
results
mean(unlist(results))