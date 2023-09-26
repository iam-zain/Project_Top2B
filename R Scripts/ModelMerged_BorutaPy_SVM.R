setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_255BorutaPy_onAll.csv", header = T)

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1){
  print(i)
  train <- createDataPartition(df [,"Age"], p=0.8, list = F) 
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (Age~., data = data_train, method = 'svmLinear', trControl = control)
  print(fit.cv)
  pred <- predict(fit.cv, data_test, method = "response")
  output[1, i] <- i
  output[2:290, i] <- pred
  output[291:579, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaPy_Age_SVM_10FCV.csv", row.names = F)

dframe = read.csv("BorutaPy_Age_SVM_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9479162

RMSE      Rsquared   MAE     
8.426396  0.8926125  6.251549

#fit.cv gives RMSE, RSquare and MAE values
