setwd ("Z:\\Project_Top2B\\April_2023")

############   RMSE   ############ 
Non_Mot = read.csv("Different_RMSE_BootStrap.csv",header = T)
Non_Mot = Non_Mot[,-c(2:4)]
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_Merged','AD', 'Schizophrenia')),
                         y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("RMSE\n") + scale_y_continuous (breaks = seq(0, 50, by = 1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("RMSE Random Sampling: All Merged model: Prediction on Diseased Data")



############   R Square   ############ 
Non_Mot = read.csv("Different_RSquare_BootStrap.csv",header = T)
Non_Mot = Non_Mot[,-c(2:4)]
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RSquare')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('All_Merged','AD', 'Schizophrenia')),
                         y = RSquare, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("R-Square\n") + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("R Square Random Sampling: All Merged model: Prediction on Diseased Data")



############   Pearson Correlation   ############ 
Non_Mot = read.csv("Different_Correlation_BootStrap.csv",header = T)
Non_Mot = Non_Mot[,-c(2:4)]
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Pearson')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('All_Merged','AD', 'Schizophrenia')),
                         y = Pearson, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("Pearson Correlation\n") + scale_y_continuous (breaks = seq(0, 1, by = 0.05)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("Pearson Correlation Random Sampling: All Merged model: Prediction on Diseased Data")



############   MAE   ############ 
Non_Mot = read.csv("Different_MAE_BootStrap.csv",header = T)
Non_Mot = Non_Mot[,-c(2:4)]
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'MAE')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('All_Merged','AD', 'Schizophrenia')),
                         y = MAE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("MAE\n") + scale_y_continuous (breaks = seq(0, 10, by = 0.5)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("MAE Random Sampling: All Merged model: Prediction on Diseased Data")



############   MSE   ############ 
Non_Mot = read.csv("Different_MSE_BootStrap.csv",header = T)
Non_Mot = Non_Mot[,-c(2:4)]
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'MSE')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('All_Merged','AD', 'Schizophrenia')),
                         y = MSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("MSE\n") + scale_y_continuous (breaks = seq(0, 1200, by = 10)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("MSE Random Sampling: All Merged model: Prediction on Diseased Data")

