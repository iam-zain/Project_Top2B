setwd ("Z:\\Project_Top2B\\April_2023\\all")
df <- vroom("Merged_HealthyAndDiseased_Top2B.csv")
df <- df  %>% select(-c(1))
sum(is.na(df))
#Finding variance of each CpG
vars = apply(df[,-1], 2, var) # -1 means except 1st column, 2 means columnwise
plot(vars)
summary(vars)
boxplot(vars)
hist(vars)


############  Variance 0.01  #############
cpg_0.01 = names(vars)[(which(vars>0.01))] # Selecting CpG having values >0.01

#Selecting CpG which has variance more than 0.005 
df1 = df[,cpg_0.01]
df1$ChroAge = df$Age

model <- lm (ChroAge ~ ., data = df1)
summary(model)
Predict_Age = predict(model, datanew= df1[,-363])
plot(df1$ChroAge, Predict_Age)
cor(df1$ChroAge, Predict_Age) # 0.9512438

write.csv(df1, "Var01_Merged_CpGListData.csv", row.names = F)

Age_Compare = data.frame(df1$ChroAge, Predict_Age)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter())+theme_minimal_hgrid()+
scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age")+
ggtitle("Age Comparison Plot (Correlation value = 0.9512438) 428 CpG (Variance >0.01)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




####  Test Train  ####
setwd ("Z:\\Project_Top2B\\April_2023\\all")
df <- read.csv("Var01_Merged_CpGListData1.csv")
head(df)
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
  output[2:654, i] <- pred
  output[655:1307, i] <- data_test$Age
}
stopCluster(cl)
output
write.csv(output, "Var01_Age_enet_10FCV.csv", row.names = F)

dframe = read.csv("Var01_Age_enet_10FCV.csv", header = T)
cor(dframe$Age, dframe$PredAge, method = "pearson") # 0.9318577

RMSE      Rsquared    MAE
8.291609  0.8690201   6.336464















############  Variance 0.007  #############
cpg_0.01 = names(vars)[(which(vars>0.007))] # Selecting CpG having values >0.01

#Selecting CpG which has variance more than 0.005 
df1 = df[,cpg_0.01]
df1$ChroAge = df$Age

model <- lm (ChroAge ~ ., data = df1)
summary(model)
Predict_Age = predict(model, datanew= df1[,-329])
plot(df1$ChroAge, Predict_Age)
cor(df1$ChroAge, Predict_Age) #

write.csv(df1, "Var007_PostNatal_CpGListData_ChroAge_tl.csv")

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter())+theme_minimal_hgrid()+
scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age")+
ggtitle("Age Comparison Plot (Correlation value = 0.3) 3 CpG (Variance >0.00)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




############  Boruta  ############
result_boruta <- Boruta (Age ~ ., data = df, doTrace = 2, maxRuns = 999)
imp_Tent_CpG <- getNonRejectedFormula(result_boruta) #Shows confirmed,tentative
imp_Conf_CpG <- getConfirmedFormula(result_boruta) #Shows only confirmed important

imp_CpG_Data <- df %>% select(Age,)


write.csv(imp_CpG_Data, "CpG_176Boruta_onAll_tl.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_176Boruta_onAll_tl.csv") 
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.9267886

#write.csv(CpG_withData_1, "BorutaOnAll_PostNatal_CpGListData_ChroAge.csv")

Age_Compare = data.frame(CpG_withData_1$Age, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_BorutaOnAllPredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(CpG_withData_1$Age, Predict_Age)) +
  geom_point(position = position_jitter()),theme_minimal_hgrid(),
scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age"),
ggtitle("Age Comparison Plot (Correlation value = 0.9267886) 176 CpG (among 3193 CpG)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





##############  Boruta Python  #############
imp_CpG_Data <- df %>% select(Age,cg24222995
                              ,cg16362480
                              ,cg19295840
                              ,cg27038935
                              ,cg15260109
                              ,cg02446366
                              ,cg02467451
                              ,cg10461088
                              ,cg00751072
                              ,cg24446586
                              ,cg02477175
                              ,cg07036730
                              ,cg21638161
                              ,cg13210467
                              ,cg23999932
                              ,cg15911153
                              ,cg09516349
                              ,cg21918786
                              ,cg27658967
                              ,cg05913514
                              ,cg22015966
                              ,cg27569265
                              ,cg21326301
                              ,cg08410921
                              ,cg14595269
                              ,cg21151769
                              ,cg24853724
                              ,cg08798295
                              ,cg22917801
                              ,cg08767286
                              ,cg17270520
                              ,cg05612279
                              ,cg17051321
                              ,cg17029156
                              ,cg16952286
                              ,cg23184070
                              ,cg00313401
                              ,cg20591472
                              ,cg22830707
                              ,cg20706192
                              ,cg01196531
                              ,cg17431746
                              ,cg16701059
                              ,cg20894465
                              ,cg11297817
                              ,cg01029676
                              ,cg01021045
                              ,cg17820989
                              ,cg10963061
                              ,cg00302494
                              ,cg01601841
                              ,cg27601855
                              ,cg09815927
                              ,cg27635069
                              ,cg15212349
                              ,cg14026927
                              ,cg18691434
                              ,cg04745384
                              ,cg22078638
                              ,cg14014506
                              ,cg26110645
                              ,cg14781190
                              ,cg27624319
                              ,cg00222341
                              ,cg24761867
                              ,cg25054580
                              ,cg22048546
                              ,cg15356195
                              ,cg04757806
)


write.csv(imp_CpG_Data, "CpG_69BorutaPy_onAll_fl.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_69BorutaPy_onAll_fl.csv") #Duplicates removed
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.8747325

#write.csv(CpG_withData_1, "BorutaOnAll_PostNatal_CpGListData_ChroAge.csv")

Age_Compare = data.frame(CpG_withData_1$Age, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_BorutaOnAllPredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(CpG_withData_1$Age, Predict_Age)) +
  geom_point(position = position_jitter()),theme_minimal_hgrid(),
scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age"),
ggtitle("Age Comparison Plot (Correlation value = 0.8747325) 69 CpG (among 3193 CpG)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))


