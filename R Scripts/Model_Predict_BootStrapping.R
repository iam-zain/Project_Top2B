########   Frontal  ########
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Healthy")
df <- read.csv("CpG_147BorutaPy_onAll_fl.csv", header = TRUE)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_fl.RData") 

# Predicting Age on Diseased dataset #
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Diseased")
NewData <- vroom("Merged_Top2B_Frontal_DiseasedMean.csv")
head(NewData[1:4,1:9])

CpgNames = colnames(df[,2:148]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_Frontal.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_Frontal.csv', row.names = F)







########   Cerebellum  ########
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum")
df <- read.csv("CpG_49BorutaPy_onAll_cb.csv", header = TRUE)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_cb.RData") 

### Diseased - Cerebellum ###
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum\\Diseased")
NewData <- vroom("Merged_Top2B_Cerebellum_DiseasedMean.csv")

CpgNames <- colnames(df[, 2:50])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_Cerebellum.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_Cerebellum.csv', row.names = F)






########   Temporal  ########
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\temporal_lobe\\Healthy")
df <- read.csv("CpG_69BorutaPy_onAll_fl.csv", header = TRUE)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_tl.RData") 

### Diseased - Temporal ###
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\temporal_lobe\\Diseased")
NewData <- vroom("Merged_Top2B_Temporal_DiseasedMean.csv")

CpgNames <- colnames(df[, 2:70])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_Temporal.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_Temporal.csv', row.names = F)









########   Healthy (all)  ########
setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 

### Diseased - All ###
setwd ("Z:\\Project_Top2B\\April_2023\\Diseased_All")
NewData <- vroom("Merged_Top2B_DiseasedMean1.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_All.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_All.csv', row.names = F)








### Schizophrenia - All ###
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\schiz")
NewData <- vroom("Merged_Schiz_Top2B_AgeMean.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_SchizAll.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_SchizAll.csv', row.names = F)






### AD - All ###
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\AD")
NewData <- vroom("Merged_AD_Top2B_AgeMean.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_AD_All.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_AD_All.csv', row.names = F)





########   Model of All on Frontal  ########
setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 

# Predicting Age on Diseased dataset #
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Diseased")
NewData <- vroom("Merged_Top2B_Frontal_DiseasedMean.csv")
head(NewData[1:4,1:9])

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset
SelectCpGsRaw <- na.omit(SelectCpGsRaw) # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_ModelHealthyAll_Frontal.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_ModelHealthyAll_Frontal.csv', row.names = F)








########   Model of All on Temporal  ########
setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 

### Diseased - Temporal ###
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\temporal_lobe\\Diseased")
NewData <- vroom("Merged_Top2B_Temporal_DiseasedMean.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_ModelHealthyAll_Temporal.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_ModelHealthyAll_Temporal.csv', row.names = F)






########   Model of All on Cerebellum  ########
setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow") 
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)
# Loading the model
TheModel <- readRDS("Model_Top2B_enet_MergedData.RData") 

### Diseased - Cerebellum ###
setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum\\Diseased")
NewData <- vroom("Merged_Top2B_Cerebellum_DiseasedMean.csv")

CpgNames <- colnames(df[, 2:237])  # Selecting Column names from our model
SelectCpGsRaw <- NewData[, CpgNames]  # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- as.numeric(as.character(NewData$Age))  # Convert age to numeric
SelectCpGsRaw <- na.omit(SelectCpGsRaw)  # Removing NAs
Age_Compare <- data.frame(OriginalAge = numeric(), PredictAge = numeric())
results_df <- data.frame(correlation = numeric(), RMSE = numeric(), RSquare = numeric(), MAE = numeric(), MSE = numeric())

n_samples <- 100  # Number of bootstrap samples
sample_size <- round(nrow(SelectCpGsRaw) * 0.66)  # Size of each bootstrap sample

for (i in 1:n_samples) {
  bootstrap_indices <- sample(nrow(SelectCpGsRaw), sample_size, replace = TRUE)
  bootstrap_sample <- SelectCpGsRaw[bootstrap_indices, ]
  
  SelectCpGs1 <- bootstrap_sample %>% select(-OriginalAge) # Remove Age before prediction
  PredictAge <- predict(TheModel, newdata = SelectCpGs1) # Use MODEL: TheModel
  PredictAge <- as.data.frame(PredictAge) # Convert to dataframe
  
  SelectCpGs1$OriginalAge <- bootstrap_sample$OriginalAge # Add Original Age again
  SelectCpGs1$PredictAge <- PredictAge$PredictAge # Add Predicted Age
  
  correlation <- cor(SelectCpGs1$OriginalAge, SelectCpGs1$PredictAge, method = "pearson") # Check correlation
  RMSE <- sqrt(mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2)) # Calculate RMSE
  RSquare <- summary(lm(SelectCpGs1$PredictAge ~ SelectCpGs1$OriginalAge))$r.squared # Calculate R-Square
  MAE <- mean(abs(SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)) # Calculate MAE
  MSE <- mean((SelectCpGs1$OriginalAge - SelectCpGs1$PredictAge)^2) # Calculate MSE
  
  results_df <- rbind(results_df, data.frame(correlation, RMSE, RSquare, MAE, MSE))
  
  # Store OriginalAge and PredictAge in Age_Compare
  Age_Compare <- rbind(Age_Compare, data.frame(OriginalAge = SelectCpGs1$OriginalAge, PredictAge = SelectCpGs1$PredictAge))
}

Age_Compare <- Age_Compare[order(Age_Compare$OriginalAge), ]
Age_Compare <- Age_Compare[!duplicated(Age_Compare), ]


write.csv(results_df, 'BS_Predct_CorrelationETC_ModelHealthyAll_Cerebellum.csv', row.names = F)
write.csv(Age_Compare, 'BS_Age_Compare_ModelHealthyAll_Cerebellum.csv', row.names = F)

