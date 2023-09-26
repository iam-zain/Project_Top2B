setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\temporal_lobe\\Healthy")

##########  Boruta Py   #############
df <- read.csv("CpG_69BorutaPy_onAll_fl.csv", header = T)

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
  output[2:114, i] <- pred
  output[115:227, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaPy_Age_enet_10FCV_tl.csv", row.names = F)

dframe = read.csv("BorutaPy_Age_enet_10FCV_tl.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.7929228

RMSE      Rsquared    MAE
7.913589  0.7176530   6.022895






##########  Var 0.005   #############

df <- read.csv("Var007_PostNatal_CpGListData_ChroAge_tl.csv", header = T)
output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
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
  output[2:114, i] <- pred
  output[115:227, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var007_Age_enet_10FCV.csv", row.names = F)

dframe = read.csv("Var007_Age_enet_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.8394224

RMSE       Rsquared     MAE     
8.385562   0.6822004    6.485586






##########  Boruta R  #############
df <- read.csv("CpG_176Boruta_onAll_tl.csv", header = T)

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
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
  output[2:114, i] <- pred
  output[115:227, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaR_Age_enet_10FCV_tl.csv", row.names = F)

dframe = read.csv("BorutaR_Age_enet_10FCV_tl.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.8153814

RMSE      Rsquared    MAE
7.362102  0.7536940   5.729658








##########  var 0.01   #############
df <- read.csv("Var01_Merged_CpGListData_ChroAge_tl.csv", header = T)

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
  output[2:114, i] <- pred
  output[115:227, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var01_Age_enet_10FCV_tl.csv", row.names = F)

dframe = read.csv("Var01_Age_enet_10FCV_tl.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.8208707

RMSE      Rsquared    MAE
8.738846  0.6552785   6.812232
