setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\temporal_lobe\\Healthy")
df <- read.csv("Merged_Top2B_Mean_tl.csv", header = T)
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
cor(df1$ChroAge, Predict_Age) # 0.9096418

write.csv(df1, "Var01_Merged_CpGListData_ChroAge_tl.csv", row.names = F)

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()),theme_minimal_hgrid(),
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age"),
  ggtitle("Age Comparison Plot (Correlation value = 0.9567082) 105 CpG (Variance >0.01)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




############  Variance 0.007  #############
cpg_0.01 = names(vars)[(which(vars>0.007))] # Selecting CpG having values >0.01

#Selecting CpG which has variance more than 0.005 
df1 = df[,cpg_0.01]
df1$ChroAge = df$Age

model <- lm (ChroAge ~ ., data = df1)
summary(model)
Predict_Age = predict(model, datanew= df1[,-329])
plot(df1$ChroAge, Predict_Age)
cor(df1$ChroAge, Predict_Age) # 0.9636956

write.csv(df1, "Var007_PostNatal_CpGListData_ChroAge_tl.csv")

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()),theme_minimal_hgrid(),
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age"),
  ggtitle("Age Comparison Plot (Correlation value = 0.987435) 369 CpG (Variance >0.009)") +
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

imp_CpG_Data <- df %>% select(Age,cg00048759,cg00075589,cg00151607,cg00222341,cg00302494,
                                cg00313401,cg00370293,cg00409684,cg00446211,cg00474746,
                                cg00751072,cg00841693,cg00874073,cg01021045,cg01054755,
                                cg01142635,cg01329687,cg01505176,cg01601841,cg01721538,
                                cg01915076,cg02467451,cg02477175,cg02623684,cg02624770,
                                cg02838492,cg03168249,cg03199745,cg03543495,cg03607825,
                                cg03958004,cg04337928,cg04399091,cg04555312,cg04745384,
                                cg04757806,cg04781820,cg05009619,cg05223897,cg05309454,
                                cg05366561,cg05612279,cg05827943,cg05913514,cg05977669,
                                cg05988699,cg06081518,cg06232263,cg06391982,cg06651450,
                                cg06670742,cg06851224,cg07028390,cg07123069,cg07235053,
                                cg07408211,cg07714657,cg07892422,cg08332990,cg08410921,
                                cg08428129,cg08624472,cg08767286,cg08816988,cg09147131,
                                cg09373148,cg09511119,cg09815927,cg09816471,cg10461088,
                                cg10623219,cg10799055,cg10963061,cg11393847,cg12457238,
                                cg12884704,cg13033853,cg13134662,cg13210467,cg13557631,
                                cg13603859,cg13648088,cg13775054,cg14014506,cg14061069,
                                cg14145074,cg14185918,cg14245947,cg14265650,cg14507002,
                                cg14595269,cg14745529,cg14781190,cg15260109,cg15356195,
                                cg15572086,cg15630071,cg15848628,cg15911153,cg15916646,
                                cg16209860,cg16362480,cg16385941,cg16484811,cg16701059,
                                cg16855422,cg16952286,cg17029156,cg17051321,cg17073432,
                                cg17255063,cg17270520,cg17431746,cg17720153,cg17810691,
                                cg17820989,cg17830716,cg18023065,cg18184107,cg18427589,
                                cg18691434,cg19087463,cg19363916,cg19664311,cg19754013,
                                cg20383948,cg20431080,cg20533957,cg20557595,cg20571333,
                                cg20591472,cg20706192,cg20753954,cg20820438,cg20889818,
                                cg20894465,cg21151769,cg21326301,cg21638161,cg21963656,
                                cg22015966,cg22048546,cg22621695,cg22719913,cg22830707,
                                cg23184070,cg23194024,cg23987054,cg24002183,cg24137427,
                                cg24222995,cg24446586,cg24724832,cg24761867,cg24853724,
                                cg25038283,cg25120797,cg25877299,cg26110645,cg26231529,
                                cg26360484,cg26546105,cg26780321,cg26958558,cg26967100,
                                cg27013360,cg27038935,cg27079232,cg27108600,cg27269940,
                                cg27389549,cg27431596,cg27569265,cg27624319,cg27635069,
                                cg27658967)


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


