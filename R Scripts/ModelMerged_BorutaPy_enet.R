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
  fit.cv <- train (Age~., data = data_train, method = 'enet', trControl = control)
  print(fit.cv)
  pred <- predict(fit.cv, data_test, method = "response")
  output[1, i] <- i
  output[2:290, i] <- pred
  output[291:579, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaPy_Age_enet_10FCV.csv", row.names = F)

dframe = read.csv("BorutaPy_Age_enet_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9523259

RMSE      Rsquared    MAE     
7.827175  0.9059695   5.859458

#fit.cv gives RMSE, RSquare and MAE values










##########  Boruta R  #############
df <- read.csv("CpG_483Boruta_onAll.csv", header = T)

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
  fit.cv <- train (Age~., data = data_train, method = 'enet', trControl = control)
  print(fit.cv)
  pred <- predict(fit.cv, data_test, method = "response")
  output[1, i] <- i
  output[2:290, i] <- pred
  output[291:579, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaR_Age_enet_10FCV.csv", row.names = F)

dframe = read.csv("BorutaR_Age_enet_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9543271

RMSE      Rsquared    MAE
7.727919  0.9089281   5.760243






##########  Variance 0.01   #############
df <- read.csv("TopVar_Merged_CpGListData_ChroAge.csv", header = T)

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
  fit.cv <- train (Age~., data = data_train, method = 'enet', trControl = control)
  print(fit.cv)
  pred <- predict(fit.cv, data_test, method = "response")
  output[1, i] <- i
  output[2:290, i] <- pred
  output[291:579, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var01_Age_enet_10FCV.csv", row.names = F)

dframe = read.csv("Var01_Age_enet_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9275535

RMSE      Rsquared    MAE
9.306887  0.8681264   7.213909
