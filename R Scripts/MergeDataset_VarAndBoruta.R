setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow")
df <- read.csv("Merged18_Top2B_Mean.csv", header = T)
sum(is.na(df))
library(zoo)
df = na.aggregate(df)
#Finding variance of each CpG
vars = apply(df[,-1], 2, var) # -1 meand except 1st column, 2 means columnwise
plot(vars)
summary(vars)
boxplot(vars)
hist(vars)

#write.csv(df, "Merged18_Top2B_noNA.csv", row.names = F)



############  Variance 0.01  #############
cpg_0.01 = names(vars)[(which(vars>0.01))] # Selecting CpG having values >0.01

#Selecting CpG which has variance more than 0.005 
df1 = df[,cpg_0.01]
df1$ChroAge = df$Age

model <- lm (ChroAge ~ ., data = df1)
summary(model)
Predict_Age = predict(model, datanew= df1[,-363])
plot(df1$ChroAge, Predict_Age)
cor(df1$ChroAge, Predict_Age) # 0.9578865

write.csv(df1, "TopVar_Merged_CpGListData_ChroAge.csv", row.names = F)

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9578865) 373 CpG (Variance >0.01)") +
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
Predict_Age = predict(model, datanew= df1[,-672])
plot(df1$ChroAge, Predict_Age)
cor(df1$ChroAge, Predict_Age) # 0.9787125

write.csv(df1, "TopVar_PostNatal_005CpGListData_ChroAge.csv")

Age_Compare = data.frame(df1$ChroAge, Predict_Age)
#write.csv(Age_Compare, "AgeCompare_PredVsChroAge_PostNatal.csv", row.names = F)

#Plot using ggplot
ggplot(Age_Compare, aes(df1$ChroAge, Predict_Age)) +
  geom_point(position = position_jitter()) + theme_minimal_hgrid() + 
  scale_color_discrete(name = 'ChroAge') +
  labs(x = "Chronological Age", y= "Predicted Age") + 
  ggtitle("Age Comparison Plot (Correlation value = 0.9787125) 671 CpG (Variance >0.005)") +
  geom_smooth(method = "lm", formula = y~x, se=F) +
  scale_x_continuous (breaks = seq(0, 120, by = 10)) +
  scale_y_continuous (breaks = seq(0, 120, by = 10)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =2)) +
  theme(panel.grid.major.x = element_line(color = "grey",linewidth = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",linewidth = 0.2,linetype = 3))




############  Boruta  ############
result_boruta <- Boruta (Age ~ ., data = df, doTrace = 2, maxRuns = 999)
imp_Tent_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_Conf_CpG <- getConfirmedFormula(result_boruta) #Shows only confirmed important

imp_CpG_Data <- df %>% select(Age,cg00075589,cg00155429,cg00177797,cg00243281,cg00301775, cg00400263,cg00409684,
                              cg00437258,cg00474746,cg00502509,
cg00751072,cg00800993,cg00841693,cg00875191,cg00926192,cg01021045,cg01029676,cg01033463,cg01039763,cg01054755,
cg01142635,cg01209909,cg01329687,cg01409887,cg01443390,cg01505176,cg01572696,cg01601841,cg01721538,cg01830674,
cg01842807,cg01846486,cg01888592,cg02000113,cg02036832,cg02051616,cg02070354,cg02134200,cg02229097,cg02229516,
cg02236651,cg02238051,cg02263144,cg02311725,cg02318926,cg02374107,cg02477175,cg02504211,cg02513409,cg02624770,
cg02628353,cg02727674,cg02735446,cg02758463,cg02818775,cg02838492,cg02940454,cg02949992,cg02970384,cg03040848,
cg03064167,cg03066050,cg03078269,cg03152785,cg03160740,cg03168249,cg03199745,cg03208198,cg03281661,cg03414321,
cg03498038,cg03501953,cg03515494,cg03537942,cg03543495,cg03547487,cg03574765,cg03616164,cg03631076,cg03882914,
cg03933279,cg03956820,cg03972398,cg04077069,cg04153571,cg04177251,cg04296885,cg04329125,cg04376747,cg04431346,
cg04468198,cg04484579,cg04502985,cg04533189,cg04580029,cg04582974,cg04745384,cg04757806,cg04781820,cg04823311,
cg04878072,cg05009619,cg05082095,cg05151228,cg05229803,cg05238553,cg05240948,cg05310920,cg05321361,cg05324789,
cg05416434,cg05473447,cg05542986,cg05612279,cg05675373,cg05734456,cg05751310,cg05764240,cg05772935,cg05827943,
cg05867499,cg05907933,cg05913514,cg05977669,cg05985303,cg06035754,cg06081518,cg06170425,cg06285648,cg06299037,
cg06367154,cg06375967,cg06391982,cg06526620,cg06560309,cg06651450,cg06655796,cg06699275,cg06711621,cg06745885,
cg06851224,cg06853339,cg07123069,cg07144296,cg07172676,cg07235053,cg07331616,cg07406296,cg07408211,cg07486199,
cg07503294,cg07532839,cg07552707,cg07597069,cg07599144,cg07608496,cg07613656,cg07714657,cg07760773,cg07771539,
cg07801181,cg07835682,cg07854831,cg07892422,cg07907998,cg07970734,cg08055087,cg08058544,cg08084860,cg08094175,
cg08187983,cg08248955,cg08257257,cg08293690,cg08302209,cg08332990,cg08352530,cg08374472,cg08410921,cg08444284,
cg08451517,cg08516247,cg08525001,cg08535411,cg08621624,cg08621843,cg08624472,cg08625416,cg08626445,cg08633085,
cg08662665,cg08696261,cg08730330,cg08749736,cg08767286,cg08787837,cg08798295,cg08816988,cg08846870,cg08863777,
cg09041756,cg09112514,cg09113850,cg09161549,cg09214983,cg09373148,cg09511119,cg09516349,cg09535605,cg09580244,
cg09608716,cg09760836,cg09772299,cg09788111,cg09802426,cg09815927,cg09816471,cg09865386,cg09934056,cg09944659,
cg10000424,cg10084644,cg10097681,cg10114372,cg10127483,cg10141938,cg10222474,cg10240853,cg10286163,cg10348920,
cg10398682,cg10436257,cg10479082,cg10489786,cg10623219,cg10657141,cg10767223,cg10954251,cg10966580,cg10970409,
cg11010242,cg11083276,cg11165626,cg11173941,cg11185473,cg11187427,cg11220619,cg11393847,cg11417675,cg11423323,
cg11438986,cg11586124,cg11885098,cg11967546,cg12028052,cg12210378,cg12399224,cg12422930,cg12457238,cg12613107,
cg12631737,cg12810084,cg13023584,cg13031251,cg13043150,cg13134662,cg13162615,cg13202751,cg13210467,cg13557631,
cg13575205,cg13582226,cg13645902,cg13655082,cg13673164,cg13697193,cg13752043,cg13775054,cg13944018,cg13962212,
cg13964184,cg14014506,cg14020762,cg14022322,cg14061069,cg14124894,cg14157042,cg14162552,cg14178748,cg14185918,
cg14408173,cg14490972,cg14507002,cg14595269,cg14628267,cg14696396,cg14745529,cg14786713,cg14788660,cg14822719,
cg14869372,cg14899547,cg14951864,cg14970273,cg15112783,cg15120942,cg15212349,cg15248979,cg15260109,cg15270654,
cg15356195,cg15421236,cg15480305,cg15572086,cg15574972,cg15630071,cg15714846,cg15799279,cg15825786,cg15879179,
cg15905579,cg15911153,cg15916646,cg15963913,cg16079645,cg16209860,cg16238815,cg16362480,cg16368763,cg16440058,
cg16499607,cg16510099,cg16517196,cg16601151,cg16619576,cg16645202,cg16688437,cg16701059,cg16705594,cg16722536,
cg16855422,cg16985113,cg17029156,cg17050632,cg17051321,cg17073432,cg17098040,cg17108007,cg17184952,cg17270520,
cg17283169,cg17370785,cg17371350,cg17418956,cg17431746,cg17465304,cg17545334,cg17658976,cg17810691,cg17814047,
cg17820989,cg17834180,cg17840536,cg17866734,cg17921248,cg17950095,cg18023065,cg18054632,cg18083597,cg18184107,
cg18360149,cg18389807,cg18427589,cg18428626,cg18534312,cg18691434,cg18747104,cg18786782,cg18796287,cg19052355,
cg19087463,cg19134665,cg19284277,cg19295840,cg19341977,cg19653594,cg19830147,cg19838043,cg19869746,cg19973604,
cg19977966,cg20387387,cg20485669,cg20500237,cg20533957,cg20557595,cg20591472,cg20624041,cg20706192,cg20753954,
cg20820438,cg20889818,cg20894465,cg20963020,cg20964064,cg20983112,cg21054968,cg21151769,cg21183502,cg21326139,
cg21338479,cg21475402,cg21512241,cg21525032,cg21608691,cg21638161,cg21936552,cg21947590,cg22015966,cg22048546,
cg22133562,cg22173131,cg22250531,cg22348673,cg22357379,cg22441683,cg22466550,cg22621695,cg22708233,cg22788223,
cg22798758,cg22830707,cg22851200,cg22947679,cg22967612,cg23153227,cg23162587,cg23184070,cg23188121,cg23194024,
cg23286629,cg23287547,cg23348764,cg23534983,cg23542284,cg23558473,cg23576260,cg23649631,cg23653792,cg23666945,
cg23969309,cg23984769,cg23987054,cg23999932,cg24002183,cg24137427,cg24222995,cg24286812,cg24392955,cg24407327,
cg24428042,cg24446586,cg24548108,cg24693287,cg24724832,cg24731702,cg24746106,cg24761867,cg24805239,cg24853724,
cg25023215,cg25123308,cg25303336,cg25767729,cg25820858,cg25877009,cg25901381,cg25922279,cg26110645,cg26231529,
cg26338030,cg26351776,cg26360484,cg26546105,cg26569469,cg26606327,cg26857315,cg26890189,cg26914630,cg26922444,
cg26958558,cg26967100,cg26988423,cg27007439,cg27038935,cg27052113,cg27071460,cg27108600,cg27134832,cg27156542,
cg27234864,cg27355141,cg27389549,cg27417677,cg27431596,cg27461213,cg27485687,cg27494429,cg27624319)


write.csv(imp_CpG_Data, "CpG_514Boruta_onAll.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_514Boruta_onAll.csv") #Duplicates removed
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.9778869

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
imp_CpG_Data <- df %>% select(Age,cg11967546,cg26958558,cg11393847,cg11423323,cg11438986,cg02134200,cg02051616,cg23348764
,cg12399224,cg12422930,cg23287547,cg23194024,cg01910249,cg23184070,cg12613107,cg27038935,cg01846486,cg27052113,cg11165626
,cg11083276,cg02263144,cg10966580,cg09772299,cg09802426,cg09815927,cg09816471,cg09934056,cg02624770,cg26890189,cg10000424
,cg23162587,cg10114372,cg23987054,cg10240853,cg10286163,cg23969309,cg10479082,cg10623219,cg02311725,cg10824705,cg24002183
,cg09580244,cg12846139,cg12927641,cg27108600,cg22830707,cg14185918,cg22827290,cg14408173,cg14507002,cg01329687,cg14591123
,cg14595269,cg14743352,cg14745529,cg14822719,cg22691505,cg15112783,cg15212349,cg01142635,cg22621695,cg14157042,cg01409887
,cg22857356,cg14061069,cg23152743,cg13023584,cg13031251,cg13134662,cg13162615,cg01721538,cg13210467,cg01603430,cg12861418
,cg01601841,cg13673164,cg13697193,cg22967612,cg13944018,cg13964184,cg13973180,cg01443390,cg14014506,cg13557631,cg15356195
,cg02727674,cg09516349,cg05675373,cg05719902,cg05772935,cg05827943,cg05867499,cg05900234,cg05907933,cg05977669,cg06375967
,cg06391982,cg06494592,cg03616164,cg06745885,cg06851224,cg06853339,cg03543495,cg25206134,cg05612279,cg26338030,cg05351087
,cg05321361,cg04376747,cg04580029,cg26110645,cg04745384,cg04153571,cg04757806,cg04823311,cg25901381,cg03515494,cg25820858
,cg05082095,cg25767729,cg25767504,cg05196366,cg05224190,cg05267394,cg03933279,cg05310920,cg05009619,cg02758463,cg03501953
,cg26546105,cg08410921,cg03066050,cg08624472,cg08625416,cg08633085,cg02970384,cg08696261,cg08730330,cg08767286,cg08798295
,cg24446586,cg08846870,cg09113850,cg09182138,cg24222995,cg09373148,cg24137427,cg24731702,cg03152785,cg08237551,cg24761867
,cg07406296,cg07408211,cg07503294,cg07509935,cg07532839,cg07613656,cg07714657,cg03281661,cg07123069,cg24853724,cg07771539
,cg03208198,cg03199745,cg24805239,cg07892422,cg07907998,cg08055087,cg08084860,cg07760773,cg15574972,cg04270274,cg00474746
,cg00400263,cg18786782,cg21512241,cg21475402,cg00370293,cg00357610,cg16701059,cg19134665,cg16688437,cg22015966,cg19295840
,cg19472303,cg22173131,cg16499607,cg00313401,cg00302494,cg27494429,cg27431596,cg18691434,cg18641151,cg18427589,cg00751072
,cg17418956,cg17270520,cg17184952,cg17108007,cg27355141,cg21936552,cg17820989,cg16238815,cg17073432,cg17840536,cg18023065
,cg18083597,cg22048216,cg16985113,cg18184107,cg21638161,cg00502509,cg17051321,cg19653594,cg16645202,cg15879179,cg19888017
,cg20820438,cg20889818,cg22441683,cg01021045,cg21151769,cg20533957,cg19869746,cg20557595,cg20891558,cg19830147,cg20485669
,cg15911153,cg20894465,cg15916646,cg21183502,cg20383948,cg01039763,cg27624319,cg20591472)


write.csv(imp_CpG_Data, "CpG_236BorutaPy_onAll.csv", row.names = F)

CpG_withData_1 = read.csv("CpG_236BorutaPy_onAll.csv") #Duplicates removed
## Modeling and plotting
sum(is.na(CpG_withData_1))
model <- lm (Age ~ ., data = CpG_withData_1)
summary(model)
Predict_Age = predict(model, datanew= CpG_withData_1[,-1])
plot(CpG_withData_1$Age, Predict_Age)
cor(CpG_withData_1$Age, Predict_Age) # 0.9646195

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


