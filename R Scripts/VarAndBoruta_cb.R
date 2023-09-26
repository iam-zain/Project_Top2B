setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum")
df <- read.csv("Merged_Top2B_Mean_cb.csv", header = T)
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
cor(df1$ChroAge, Predict_Age) # 0.9811848

write.csv(df1, "Var01_Merged_CpGListData_ChroAge_cb.csv", row.names = F)

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9811848) 273 CpG (Variance >0.01)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




############  Variance 0.005  #############
cpg_0.01 = names(vars)[(which(vars>0.009))] # Selecting CpG having values >0.01

#Selecting CpG which has variance more than 0.005 
df1 = df[,cpg_0.01]
df1$ChroAge = df$Age

model <- lm (ChroAge ~ ., data = df1)
summary(model)
Predict_Age = predict(model, datanew= df1[,-329])
plot(df1$ChroAge, Predict_Age)
cor(df1$ChroAge, Predict_Age) # 0.9949284

write.csv(df1, "Var009_PostNatal_CpGListData_ChroAge_cb.csv")

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9949284) 328 CpG (Variance >0.009)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




############  Boruta  ############
result_boruta <- Boruta (Age ~ ., data = df, doTrace = 2, maxRuns = 999)
imp_Tent_CpG <- getNonRejectedFormula(result_boruta) #Shows confirmed + tentative
imp_Conf_CpG <- getConfirmedFormula(result_boruta) #Shows only confirmed important

imp_CpG_Data <- df %>% select(Age,cg00257712 ,cg00301775 ,cg00302494 ,cg00370293 ,cg00473985 ,
                                cg00474746 ,cg00513735 ,cg00654814 ,cg00957684 ,cg01021045 ,
                                cg01054755 ,cg01100784 ,cg01329687 ,cg01505176 ,cg01575590 ,
                                cg01601841 ,cg01807886 ,cg01846486 ,cg01880902 ,cg02055615 ,
                                cg02112681 ,cg02238051 ,cg02366320 ,cg02504211 ,cg02624770 ,
                                cg02685016 ,cg03035661 ,cg03192775 ,cg03243135 ,cg03353765 ,
                                cg03433560 ,cg03442734 ,cg03631076 ,cg03933279 ,cg04112539 ,
                                cg04226648 ,cg04376747 ,cg04533189 ,cg04757806 ,cg04816311 ,
                                cg04977810 ,cg05057634 ,cg05140895 ,cg05189517 ,cg05406774 ,
                                cg05800368 ,cg05812814 ,cg05827943 ,cg05913684 ,cg05977669 ,
                                cg06283368 ,cg06823034 ,cg06978336 ,cg07016060 ,cg07123069 ,
                                cg07177789 ,cg07472191 ,cg07486199 ,cg07892422 ,cg08082788 ,
                                cg08187779 ,cg08187983 ,cg08625416 ,cg08696261 ,cg08798295 ,
                                cg08806263 ,cg08848753 ,cg08874598 ,cg09041756 ,cg09182138 ,
                                cg09535605 ,cg09580244 ,cg09772299 ,cg09816471 ,cg10193721 ,
                                cg10426084 ,cg10479082 ,cg10609068 ,cg10780647 ,cg11063988 ,
                                cg11083276 ,cg11409350 ,cg11423323 ,cg11439475 ,cg11474778 ,
                                cg11586124 ,cg11967546 ,cg12028052 ,cg12647142 ,cg13162615 ,
                                cg13210467 ,cg13522835 ,cg13557631 ,cg13645902 ,cg13697193 ,
                                cg13913149 ,cg13944018 ,cg13945644 ,cg14017435 ,cg14169740 ,
                                cg14408173 ,cg14595269 ,cg14709460 ,cg14745529 ,cg14781190 ,
                                cg14786713 ,cg15480305 ,cg15512832 ,cg15574972 ,cg15879179 ,
                                cg15905579 ,cg16440058 ,cg16601904 ,cg16701059 ,cg16819369 ,
                                cg17029156 ,cg17270520 ,cg17418956 ,cg18080670 ,cg18389807 ,
                                cg18427589 ,cg18565130 ,cg18786782 ,cg19234897 ,cg19472303 ,
                                cg19475258 ,cg19838043 ,cg19925025 ,cg19929126 ,cg19973604 ,
                                cg20037773 ,cg20208633 ,cg20557595 ,cg20591472 ,cg20631351 ,
                                cg20771670 ,cg20905902 ,cg20963020 ,cg21151769 ,cg21184174 ,
                                cg21226225 ,cg21512241 ,cg21638161 ,cg21732842 ,cg21783428 ,
                                cg22015966 ,cg22669260 ,cg22830707 ,cg22851200 ,cg22857356 ,
                                cg22967612 ,cg23194024 ,cg23287547 ,cg23394480 ,cg23456306 ,
                                cg23642130 ,cg23659289 ,cg24066980 ,cg24731702 ,cg24746106 ,
                                cg24761867 ,cg24853724 ,cg25023215 ,cg25121609 ,cg25211956 ,
                                cg26220673 ,cg26546105 ,cg27108600 ,cg27287438 ,cg27351581)


write.csv(imp_CpG_Data, "CpG_170Boruta_onAll_cb.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_170Boruta_onAll_cb.csv") 
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.9670818

#write.csv(CpG_withData_1, "BorutaOnAll_PostNatal_CpGListData_ChroAge.csv")

Age_Compare = data.frame(CpG_withData_1$Age, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_BorutaOnAllPredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(CpG_withData_1$Age, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9778869) 514 CpG (among 3193 CpG)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





##############  Boruta Python  #############
imp_CpG_Data <- df %>% select(Age,cg19472303,cg02855996,cg00400263,cg21732842,cg08874598,cg10479082,cg09772299,
                              cg22830707,cg20163796,cg10348543,cg22851200,cg13162615,cg15905579,cg17031165,cg04533189,
                              cg23659289,cg17029156,cg13080626,cg23642130,cg01575590,cg26220673,cg02238051,cg04816311,
                              cg00302494,cg07196207,cg14595269,cg24066980,cg05189517,cg01021045,cg11474778,cg16601904,
                              cg20591472,cg05977669,cg24853724,cg00513735,cg20905902,cg08758174,cg16701059,cg21512241,
                              cg00474746,cg00473985,cg03353765,cg01242498,cg13944018,cg01329687,cg11063988,cg18873763,
                              cg17329518,cg08187983)


write.csv(imp_CpG_Data, "CpG_49BorutaPy_onAll_cb.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_49BorutaPy_onAll_cb.csv") #Duplicates removed
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.9112927

#write.csv(CpG_withData_1, "BorutaOnAll_PostNatal_CpGListData_ChroAge.csv")

Age_Compare = data.frame(CpG_withData_1$Age, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_BorutaOnAllPredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(CpG_withData_1$Age, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9646195) 236 CpG (among 3193 CpG)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))


