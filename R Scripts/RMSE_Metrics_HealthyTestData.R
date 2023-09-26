## Healthy RMSE RSquare, Correlation, mAE, MSE  ##
## Train on 80% and Test on 20% ##


# Cerebellum #
setwd('Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum')
df <- read.csv("CpG_49BorutaPy_onAll_cb.csv", header = TRUE)

num_iterations <- 100
output <- data.frame(matrix(ncol = num_iterations * 2, nrow = 73))
num_metrics <- 5  # Number of evaluation metrics
num_runs <- num_iterations
dframe <- data.frame(matrix(ncol = num_metrics * num_runs, nrow = 0))

cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for (i in 1:num_iterations) {
  print(i)
  
  train <- createDataPartition(df[,"Age"], p = 0.8, list = FALSE) 
  data_train <- df[train,]
  data_test <- df[-train,]
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train(Age ~ ., data = data_train, method = 'enet', trControl = control)
  pred <- predict(fit.cv, data_test, method = "response")
  
  # Store results in the output data frame
  output[, (2 * i - 1)] <- pred
  output[, (2 * i)] <- data_test$Age
  
  # Calculate evaluation metrics
  metrics <- data.frame(
    RMSE = sqrt(mean((pred - data_test$Age)^2)),
    R_Square = cor(pred, data_test$Age)^2,
    Pearson_Correlation = cor(pred, data_test$Age),
    MAE = mean(abs(pred - data_test$Age)),
    MSE = mean((pred - data_test$Age)^2))
  
  # Add metrics to the dframe dataframe
  dframe <- rbind(dframe, metrics)
}

stopCluster(cl)

# Rename columns for clarity
colnames(output) <- c(rbind(paste0("pred_", 1:num_iterations), paste0("Age_", 1:num_iterations)))

write.csv(output, 'AgeCompare_onlyHealthy_cb.csv', row.names = F)
write.csv(dframe, 'RMSE_Metrics_Healthy_cb.csv', row.names = F)



# Frontal #
setwd('Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Healthy')
df <- read.csv("CpG_147BorutaPy_onAll_fl.csv", header = TRUE)

num_iterations <- 100
output <- data.frame(matrix(ncol = num_iterations * 2, nrow = 130))
num_metrics <- 5  # Number of evaluation metrics
num_runs <- num_iterations
dframe <- data.frame(matrix(ncol = num_metrics * num_runs, nrow = 0))

cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for (i in 1:num_iterations) {
  print(i)
  
  train <- createDataPartition(df[,"Age"], p = 0.8, list = FALSE) 
  data_train <- df[train,]
  data_test <- df[-train,]
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train(Age ~ ., data = data_train, method = 'enet', trControl = control)
  pred <- predict(fit.cv, data_test, method = "response")
  
  # Store results in the output data frame
  output[, (2 * i - 1)] <- pred
  output[, (2 * i)] <- data_test$Age
  
  # Calculate evaluation metrics
  metrics <- data.frame(
    RMSE = sqrt(mean((pred - data_test$Age)^2)),
    R_Square = cor(pred, data_test$Age)^2,
    Pearson_Correlation = cor(pred, data_test$Age),
    MAE = mean(abs(pred - data_test$Age)),
    MSE = mean((pred - data_test$Age)^2))
  
  # Add metrics to the dframe dataframe
  dframe <- rbind(dframe, metrics)
}

stopCluster(cl)

# Rename columns for clarity
colnames(output) <- c(rbind(paste0("pred_", 1:num_iterations), paste0("Age_", 1:num_iterations)))

write.csv(output, 'AgeCompare_onlyHealthy_Frontal.csv', row.names = F)
write.csv(dframe, 'RMSE_Metrics_Healthy_Frontal.csv', row.names = F)



# Temporal #
setwd('Z:\\Project_Top2B\\April_2023\\regionwise\\temporal_lobe\\Healthy')
df <- read.csv("CpG_69BorutaPy_onAll_tl.csv", header = TRUE)

num_iterations <- 100
output <- data.frame(matrix(ncol = num_iterations * 2, nrow = 113))
num_metrics <- 5  # Number of evaluation metrics
num_runs <- num_iterations
dframe <- data.frame(matrix(ncol = num_metrics * num_runs, nrow = 0))

cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for (i in 1:num_iterations) {
  print(i)
  
  train <- createDataPartition(df[,"Age"], p = 0.8, list = FALSE) 
  data_train <- df[train,]
  data_test <- df[-train,]
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train(Age ~ ., data = data_train, method = 'enet', trControl = control)
  pred <- predict(fit.cv, data_test, method = "response")
  
  # Store results in the output data frame
  output[, (2 * i - 1)] <- pred
  output[, (2 * i)] <- data_test$Age
  
  # Calculate evaluation metrics
  metrics <- data.frame(
    RMSE = sqrt(mean((pred - data_test$Age)^2)),
    R_Square = cor(pred, data_test$Age)^2,
    Pearson_Correlation = cor(pred, data_test$Age),
    MAE = mean(abs(pred - data_test$Age)),
    MSE = mean((pred - data_test$Age)^2))
  
  # Add metrics to the dframe dataframe
  dframe <- rbind(dframe, metrics)
}

stopCluster(cl)

# Rename columns for clarity
colnames(output) <- c(rbind(paste0("pred_", 1:num_iterations), paste0("Age_", 1:num_iterations)))

write.csv(output, 'AgeCompare_onlyHealthy_Temporal.csv', row.names = F)
write.csv(dframe, 'RMSE_Metrics_Healthy_Temporal.csv', row.names = F)




# All #
setwd('Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow')
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = TRUE)

num_iterations <- 100
output <- data.frame(matrix(ncol = num_iterations * 2, nrow = 360))
num_metrics <- 5  # Number of evaluation metrics
num_runs <- num_iterations
dframe <- data.frame(matrix(ncol = num_metrics * num_runs, nrow = 0))

cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for (i in 1:num_iterations) {
  print(i)
  
  train <- createDataPartition(df[,"Age"], p = 0.8, list = FALSE) 
  data_train <- df[train,]
  data_test <- df[-train,]
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train(Age ~ ., data = data_train, method = 'enet', trControl = control)
  pred <- predict(fit.cv, data_test, method = "response")
  
  # Store results in the output data frame
  output[, (2 * i - 1)] <- pred
  output[, (2 * i)] <- data_test$Age
  
  # Calculate evaluation metrics
  metrics <- data.frame(
    RMSE = sqrt(mean((pred - data_test$Age)^2)),
    R_Square = cor(pred, data_test$Age)^2,
    Pearson_Correlation = cor(pred, data_test$Age),
    MAE = mean(abs(pred - data_test$Age)),
    MSE = mean((pred - data_test$Age)^2))
  
  # Add metrics to the dframe dataframe
  dframe <- rbind(dframe, metrics)
}

stopCluster(cl)

# Rename columns for clarity
colnames(output) <- c(rbind(paste0("pred_", 1:num_iterations), paste0("Age_", 1:num_iterations)))

write.csv(output, 'AgeCompare_onlyHealthy_All.csv', row.names = F)
write.csv(dframe, 'RMSE_Metrics_Healthy_All.csv', row.names = F)



