setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Healthy")
df <- read.csv("Merged_Top2B_Mean_fl.csv", header = T)
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
cor(df1$ChroAge, Predict_Age) # 0.9567082

write.csv(df1, "Var01_Merged_CpGListData_ChroAge_fl.csv", row.names = F)

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9567082) 105 CpG (Variance >0.01)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




############  Variance 0.005  #############
cpg_0.01 = names(vars)[(which(vars>0.005))] # Selecting CpG having values >0.01

#Selecting CpG which has variance more than 0.005 
df1 = df[,cpg_0.01]
df1$ChroAge = df$Age

model <- lm (ChroAge ~ ., data = df1)
summary(model)
Predict_Age = predict(model, datanew= df1[,-329])
plot(df1$ChroAge, Predict_Age)
cor(df1$ChroAge, Predict_Age) # 0.987435

write.csv(df1, "Var009_PostNatal_CpGListData_ChroAge_fl.csv")

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.987435) 369 CpG (Variance >0.009)") +
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

imp_CpG_Data <- df %>% select(Age,cg00243281,cg00301775,cg00302494,cg00303183,cg00357610,
,  cg00370293,cg00400263,cg00474746,cg00502509,cg00608548,
,  cg00874073,cg01039763,cg01054755,cg01062942,cg01142635,
,  cg01329687,cg01409887,cg01572696,cg01624768,cg01846486,
,  cg02018902,cg02051616,cg02070354,cg02134200,cg02154874,
,  cg02229097,cg02263144,cg02311725,cg02318926,cg02473103,
,  cg02513409,cg02624770,cg02727674,cg02735446,cg02951552,
,  cg03066050,cg03078269,cg03152785,cg03199745,cg03208198,
,  cg03281661,cg03399905,cg03428036,cg03450509,cg03498038,
,  cg03537942,cg03543495,cg03616164,cg03845863,cg03933279,
,  cg04187185,cg04275194,cg04326540,cg04376747,cg04431346,
,  cg04502985,cg04580029,cg04745384,cg04757806,cg04781820,
,  cg04926347,cg05223847,cg05310920,cg05324789,cg05331791,
,  cg05675373,cg05774672,cg05827943,cg05900234,cg05907933,
,  cg05921579,cg05977669,cg06081518,cg06092312,cg06375967,
,  cg06391982,cg06526620,cg06560309,cg06651450,cg07036730,
,  cg07123069,cg07179078,cg07224147,cg07235053,cg07465480,
,  cg07486199,cg07503294,cg07599144,cg07771539,cg07804122,
,  cg07892422,cg07907998,cg08055087,cg08181610,cg08187983,
,  cg08248955,cg08257257,cg08332990,cg08374472,cg08410921,
,  cg08478193,cg08528826,cg08621624,cg08624472,cg08662665,
,  cg08730330,cg08749736,cg08767286,cg08787837,cg08798295,
,  cg08846870,cg08863777,cg08989214,cg09047553,cg09214983,
,  cg09297288,cg09516349,cg09608716,cg09760836,cg09816471,
,  cg09862801,cg09934056,cg10193681,cg10240853,cg10283505,
,  cg10436257,cg10479082,cg10623219,cg10963061,cg10970409,
,  cg11165626,cg11330740,cg11338121,cg11400774,cg11438986,
,  cg11967546,cg12069248,cg12390907,cg12399224,cg12422930,
,  cg12457238,cg12747301,cg12945444,cg12997720,cg13031251,
,  cg13043150,cg13134662,cg13162615,cg13210467,cg13426307,
,  cg13575205,cg13603859,cg13619824,cg13645078,cg13673164,
,  cg13964184,cg14020762,cg14162552,cg14178748,cg14185918,
,  cg14408173,cg14507002,cg14696396,cg14788660,cg14822719,
,  cg15112783,cg15212349,cg15260109,cg15270654,cg15421236,
,  cg15623480,cg15630071,cg15879179,cg15911153,cg15916646,
,  cg15963913,cg16209860,cg16368763,cg16385941,cg16510099,
,  cg16688437,cg16701059,cg16705594,cg17022362,cg17051321,
,  cg17142371,cg17184952,cg17418956,cg17814047,cg17820989,
,  cg17847861,cg17866734,cg17950095,cg18023065,cg18083597,
,  cg18101314,cg18427589,cg18428626,cg18691434,cg18701590,
,  cg18747104,cg18749563,cg18786782,cg18809522,cg19295840,
,  cg19341977,cg19653594,cg19840693,cg19869746,cg20387387,
,  cg20533957,cg20591472,cg20706192,cg21097199,cg21106899,
,  cg21183502,cg21512241,cg21525032,cg21638161,cg21885973,
,  cg21947590,cg22133562,cg22690239,cg22830707,cg22903655,
,  cg23166955,cg23184070,cg23348764,cg23423852,cg23576260,
,  cg23649631,cg23653792,cg23666945,cg24002183,cg24137427,
,  cg24222995,cg24446586,cg24693287,cg24805239,cg24853724,
,  cg24925865,cg25123308,cg25707945,cg25723149,cg25767729,
,  cg25877009,cg26095266,cg26110645,cg26338030,cg26351776,
,  cg26360484,cg26546105,cg26569469,cg26578682,cg26606327,
,  cg26693553,cg26714517,cg26890189,cg26914630,cg26922444,
,  cg27004760,cg27052113,cg27071460,cg27156542,cg27355141,
,  cg27396583,cg27485687,cg27601855,cg27624319)


write.csv(imp_CpG_Data, "CpG_269Boruta_onAll_fl.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_269Boruta_onAll_fl.csv") 
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.9894103

#write.csv(CpG_withData_1, "BorutaOnAll_PostNatal_CpGListData_ChroAge.csv")

Age_Compare = data.frame(CpG_withData_1$Age, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_BorutaOnAllPredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(CpG_withData_1$Age, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9894103) 269 CpG (among 3193 CpG)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))





##############  Boruta Python  #############
imp_CpG_Data <- df %>% select(Age,cg13603859
,cg20533957
,cg16701059
,cg16705594
,cg07224147
,cg00502509
,cg20591472
,cg26578682
,cg00474746
,cg01142635
,cg08846870
,cg26546105
,cg25406735
,cg07179078
,cg02940454
,cg01062942
,cg26420739
,cg15112783
,cg22180201
,cg26714517
,cg02735446
,cg14376436
,cg01329687
,cg08730330
,cg13162615
,cg26890189
,cg13210467
,cg08749736
,cg05907933
,cg09214983
,cg00400263
,cg24925865
,cg17847861
,cg05968052
,cg15260109
,cg03845863
,cg08798295
,cg14408173
,cg00608548
,cg01039763
,cg20689228
,cg09862801
,cg05310920
,cg14696396
,cg26110645
,cg10623219
,cg09816471
,cg26095266
,cg05309454
,cg03281661
,cg21326301
,cg17142371
,cg25820858
,cg12069248
,cg17184952
,cg17418956
,cg07036730
,cg07771539
,cg03261162
,cg03543495
,cg07123069
,cg09934056
,cg03066050
,cg11967546
,cg26351776
,cg12449931
,cg21638161
,cg26338030
,cg03152785
,cg07804122
,cg13264208
,cg07156219
,cg21596313
,cg03616164
,cg03185820
,cg05675373
,cg17051321
,cg12028052
,cg24853724
,cg11438986
,cg09225139
,cg18023065
,cg27396583
,cg23666945
,cg27355141
,cg18691434
,cg07503294
,cg09373148
,cg06526620
,cg04431346
,cg19295840
,cg07465480
,cg02051616
,cg14162552
,cg04781820
,cg04376747
,cg10193681
,cg04021856
,cg24446586
,cg11165626
,cg06560309
,cg13645078
,cg04757806
,cg18786782
,cg27624319
,cg04649627
,cg12997720
,cg06391982
,cg18809855
,cg01828911
,cg06375967
,cg13619824
,cg24137427
,cg23877831
,cg04580029
,cg13031251
,cg15879179
,cg15911153
,cg08410921
,cg10240853
,cg24222995
,cg08624472
,cg21183502
,cg13951948
,cg27013360
,cg00302494
,cg15421236
,cg19870365
,cg19754013
,cg19653594
,cg14253932
,cg13134662
,cg09297288
,cg16311339
,cg10970409
,cg13964184
,cg06092312
,cg19869746
,cg18783395
,cg08055087
,cg02473103
,cg04187185
,cg11400774
,cg01377696
,cg23348764
,cg24805239
,cg00243281
)


write.csv(imp_CpG_Data, "CpG_147BorutaPy_onAll_fl.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_147BorutaPy_onAll_fl.csv") #Duplicates removed
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.982869

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


