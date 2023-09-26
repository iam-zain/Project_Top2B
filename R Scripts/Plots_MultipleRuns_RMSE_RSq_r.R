setwd ("Z:\\Project_Top2B\\April_2023")

############   RMSE   ############ 
Non_Mot = read.csv("Different_RMSE_MultipleRuns.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('Frontal','Cerebellum','Temporal','All_Merged')),
                         y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("RMSE\n") + scale_y_continuous (breaks = seq(0, 50, by = 1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("RMSE Multiple Runs: Differents models: Prediction on Diseasaed Data")



############   R Square   ############ 
Non_Mot = read.csv("Different_RSquare_MultipleRuns.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RSquare')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('Frontal', 'Cerebellum', 'Temporal', 'All_Merged')),
                         y = RSquare, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("RSquare\n") + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("R Square Multiple Runs: Different models: Prediction on Diseasaed Data")



############   Pearson Correlation   ############ 
Non_Mot = read.csv("Different_Correlation_MultipleRuns.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Pearson')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('Frontal', 'Cerebellum', 'Temporal', 'All_Merged')),
                         y = Pearson, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("Pearson Correlation\n") + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("Pearson Correlation Multiple Runs: Different models: Prediction on Diseasaed Data")



############   MAE   ############ 
Non_Mot = read.csv("Different_MAE_MultipleRuns.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'MAE')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('Frontal', 'Cerebellum', 'Temporal', 'All_Merged')),
                         y = MAE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("MAE\n") + scale_y_continuous (breaks = seq(0, 50, by = 2)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("MAE Multiple Runs: Different models: Prediction on Diseasaed Data")



############   MSE   ############ 
Non_Mot = read.csv("Different_MSE_MultipleRuns.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'MSE')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('Frontal', 'Cerebellum', 'Temporal', 'All_Merged')),
                         y = MSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("MSE\n") + scale_y_continuous (breaks = seq(0, 1200, by = 50)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle("MSE Multiple Runs: Different models: Prediction on Diseasaed Data")

