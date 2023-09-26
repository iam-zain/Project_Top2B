setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\RMSE_etc\\Healthy")

############   RMSE   ############ 
Non_Mot = read.csv("Different_RMSE_Healthy_RandomSampling.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = Feature, y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("RMSE\n") + scale_y_continuous (breaks = seq(0, 50, by = 1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("RMSE Random Sampling: Individual Dataset: Prediction on Healthy Data")



############   R-Square   ############ 
Non_Mot = read.csv("Different_RSquare_Healthy_RandomSampling.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = Feature, y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("R-Square\n") + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("R-Square Random Sampling: Individual Dataset: Prediction on Healthy Data")



############   Correlation   ############ 
Non_Mot = read.csv("Different_Correlation_Healthy_RandomSampling.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = Feature, y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("Pearson Correlation\n") + scale_y_continuous (breaks = seq(-1, 1, by = 0.1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Correlation Random Sampling: Individual Dataset: Prediction on Healthy Data")



############   Mean Absolute Error   ############ 
Non_Mot = read.csv("Different_MAE_Healthy_RandomSampling.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = Feature, y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("Mean Absolute Error\n") + scale_y_continuous (breaks = seq(0, 50, by = 1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("MAE Random Sampling: Individual Dataset: Prediction on Healthy Data")



############   Mean Squared Error   ############ 
Non_Mot = read.csv("Different_MSE_Healthy_RandomSampling.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = Feature, y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("Mean Squared Error\n") + scale_y_continuous (breaks = seq(0, 580, by = 40)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("MSE Random Sampling: Individual Dataset: Prediction on Healthy Data")

