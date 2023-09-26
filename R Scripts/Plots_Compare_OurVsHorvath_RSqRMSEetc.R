setwd ("Z:\\Project_Top2B\\April_2023\\Horvath\\Compare_OurVsHorvath")

############   RMSE   ############ 
Non_Mot = read.csv("Different_RMSE_BootStrap.csv",header = T)
Non_Mot_Long <- reshape2::melt (Non_Mot, id.vars = c("Data_Type", "Model_Type"), variable.name = "Values")
Non_Mot_Long = Non_Mot_Long [,-c(3)]

ggplot(Non_Mot_Long, aes(x= Data_Type, y=value, color = Model_Type))+
  geom_boxplot()+  ylab("Methylation Value") + scale_y_continuous (breaks = seq(0, 12, by = 0.5)) +
  theme(legend.position=c(0.86, 0.88), legend.background = element_rect(fill="azure", size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.text.x = element_text(size=18, angle=0, vjust = 0.99, color="black")) +
  xlab("") + ylab("RMSE Value\n") +
  ggtitle ("Compare our model with Horvath1 model: RMSE")





############   R Square   ############ 
Non_Mot = read.csv("Different_RSquare_BootStrap.csv",header = T)
Non_Mot_Long <- reshape2::melt (Non_Mot, id.vars = c("Data_Type", "Model_Type"), variable.name = "Values")
Non_Mot_Long = Non_Mot_Long [,-c(3)]

ggplot(Non_Mot_Long, aes(x= Data_Type, y=value, color = Model_Type))+
  geom_boxplot()+  ylab("Methylation Value") + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) +
  theme(legend.position=c(0.86, 0.11), legend.background = element_rect(fill="azure", size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.text.x = element_text(size=18, angle=0, vjust = 0.99, color="black")) +
  xlab("") + ylab("RSquare Value\n") +
  ggtitle ("Compare our model with Horvath1 model: RSquare")



############   Pearson Correlation   ############ 
Non_Mot = read.csv("Different_Correlation_BootStrap.csv",header = T)
Non_Mot_Long <- reshape2::melt (Non_Mot, id.vars = c("Data_Type", "Model_Type"), variable.name = "Values")
Non_Mot_Long = Non_Mot_Long [,-c(3)]

ggplot(Non_Mot_Long, aes(x= Data_Type, y=value, color = Model_Type))+
  geom_boxplot()+  ylab("Methylation Value") + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) +
  theme(legend.position=c(0.86, 0.11), legend.background = element_rect(fill="azure", size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.text.x = element_text(size=18, angle=0, vjust = 0.99, color="black")) +
  xlab("") + ylab("Pearson Correlation Value\n") +
  ggtitle ("Compare our model with Horvath1 model: Correlation")



############   MAE   ############ 
Non_Mot = read.csv("Different_MAE_BootStrap.csv",header = T)
Non_Mot_Long <- reshape2::melt (Non_Mot, id.vars = c("Data_Type", "Model_Type"), variable.name = "Values")
Non_Mot_Long = Non_Mot_Long [,-c(3)]

ggplot(Non_Mot_Long, aes(x= Data_Type, y=value, color = Model_Type))+
  geom_boxplot()+  ylab("Methylation Value") + scale_y_continuous (breaks = seq(0, 10, by = 0.5)) +
  theme(legend.position=c(0.86, 0.11), legend.background = element_rect(fill="azure", size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.text.x = element_text(size=18, angle=0, vjust = 0.99, color="black")) +
  xlab("") + ylab("MAE Value\n") +
  ggtitle ("Compare our model with Horvath1 model: MAE")



############   MSE   ############ 
Non_Mot = read.csv("Different_MSE_BootStrap.csv",header = T)
Non_Mot_Long <- reshape2::melt (Non_Mot, id.vars = c("Data_Type", "Model_Type"), variable.name = "Values")
Non_Mot_Long = Non_Mot_Long [,-c(3)]

ggplot(Non_Mot_Long, aes(x= Data_Type, y=value, color = Model_Type))+
  geom_boxplot()+  ylab("Methylation Value") + scale_y_continuous (breaks = seq(0, 100, by = 10)) +
  theme(legend.position=c(0.16, 0.85), legend.background = element_rect(fill="azure", size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.text.x = element_text(size=18, angle=0, vjust = 0.99, color="black")) +
  xlab("") + ylab("MSE Value\n") +
  ggtitle ("Compare our model with Horvath1 model: MSE")

