setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Healthy")

##########  Boruta Py   #############
df <- read.csv("CpG_147BorutaPy_onAll_fl.csv", header = T)

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
  output[2:131, i] <- pred
  output[132:261, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaPy_Age_enet_10FCV_fl.csv", row.names = F)

dframe = read.csv("BorutaPy_Age_enet_10FCV_fl.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9756371

RMSE      Rsquared    MAE
6.485077  0.9401636   4.879272






##########  Var 0.005   #############

df <- read.csv("Var005_PostNatal_CpGListData_ChroAge_fl.csv", header = T)
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
  output[2:131, i] <- pred
  output[132:261, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var005_Age_enet_10FCV.csv", row.names = F)

dframe = read.csv("Var005_Age_enet_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9528756

RMSE       Rsquared     MAE     
8.327099   0.9067070    6.349267
#fit.cv gives RMSE, RSquare and MAE values








##########  Boruta R  #############
df <- read.csv("CpG_269Boruta_onAll_fl.csv", header = T)

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
  output[2:131, i] <- pred
  output[132:261, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "BorutaR_Age_enet_10FCV_fl.csv", row.names = F)

dframe = read.csv("BorutaR_Age_enet_10FCV_fl.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9762066

RMSE      Rsquared    MAE
6.770908  0.9355298   5.083683








##########  var 0.01   #############
df <- read.csv("Var01_Merged_CpGListData_ChroAge_fl.csv", header = T)

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
  output[2:131, i] <- pred
  output[132:261, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var01_Age_enet_10FCV_fl.csv", row.names = F)

dframe = read.csv("Var01_Age_enet_10FCV_fl.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9337515

RMSE      Rsquared    MAE
9.445316  0.8781667   7.378279
