setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum")
df <- read.csv("Var009_PostNatal_CpGListData_ChroAge_cb.csv", header = T)
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
  output[2:74, i] <- pred
  output[75:147, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var009_Age_enet_10FCV.csv", row.names = F)

dframe = read.csv("Var009_Age_enet_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.8517243

RMSE       Rsquared     MAE     
9.546028   0.70635042   7.602073
#fit.cv gives RMSE, RSquare and MAE values



lm = 0.7985695
svmL = 0.7830335






##########  Boruta R  #############
df <- read.csv("CpG_170Boruta_onAll_cb.csv", header = T)

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
  output[2:74, i] <- pred
  output[75:147, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaR_Age_enet_10FCV_cb.csv", row.names = F)

dframe = read.csv("BorutaR_Age_enet_10FCV_cb.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9007394

RMSE      Rsquared    MAE
7.363893  0.8245865   5.893100






##########  Boruta Py   #############
df <- read.csv("CpG_49BorutaPy_onAll_cb.csv", header = T)

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
  output[2:74, i] <- pred
  output[75:147, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaPy_Age_enet_10FCV_cb.csv", row.names = F)

dframe = read.csv("BorutaPy_Age_enet_10FCV_cb.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.8806811

RMSE      Rsquared    MAE
8.567855  0.7648104   6.850298




##########  var 0.01   #############
df <- read.csv("Var01_Merged_CpGListData_ChroAge_cb.csv", header = T)

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
  output[2:74, i] <- pred
  output[75:147, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var01_Age_enet_10FCV_cb.csv", row.names = F)

dframe = read.csv("Var01_Age_enet_10FCV_cb.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.8396792

RMSE      Rsquared    MAE
9.663685  0.69823361   7.663516
