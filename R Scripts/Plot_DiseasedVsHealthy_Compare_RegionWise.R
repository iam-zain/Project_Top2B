setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\Compare\\DiseasedVsHealthy")

############   Correlation   ############ 
df = read.csv("Compare_RegionWise_DiseasedVsHealthy_Correlation_Long.csv",header = T)
#Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
df_long <- reshape2::melt (df, id.vars = c("Region", "Type"), variable.name = "Correaltion")

ggplot(df_long, aes(x= Region, y=value, color = Type))+
  geom_boxplot() + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) + 
  theme(legend.position=c(0.15, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nRegion") + ylab("Pearson Correlation\n") +
  ggtitle ("Correlation: Diseased Vs Healthy: Region wise")


############   RMSE   ############ 
df = read.csv("Compare_RegionWise_DiseasedVsHealthy_RMSE_Long.csv",header = T)
#Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
df_long <- reshape2::melt (df, id.vars = c("Region", "Type"), variable.name = "RMSE")

ggplot(df_long, aes(x= Region, y=value, color = Type))+
  geom_boxplot() + scale_y_continuous (breaks = seq(0, 30, by = 1)) + 
  theme(legend.position=c(0.15, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nRegion") + ylab("RMSE\n") +
  ggtitle ("RMSE: Diseased Vs Healthy: Region wise")


############   R-Square   ############ 
df = read.csv("Compare_RegionWise_DiseasedVsHealthy_RSquare_Long.csv",header = T)
#Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
df_long <- reshape2::melt (df, id.vars = c("Region", "Type"), variable.name = "RSquare")

ggplot(df_long, aes(x= Region, y=value, color = Type))+
  geom_boxplot() + scale_y_continuous (breaks = seq(0, 1, by = 0.1)) + 
  theme(legend.position=c(0.15, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nRegion") + ylab("RSquare\n") +
  ggtitle ("R-Square: Diseased Vs Healthy: Region wise")



############   MAE   ############ 
df = read.csv("Compare_RegionWise_DiseasedVsHealthy_MAE_Long.csv",header = T)
#Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
df_long <- reshape2::melt (df, id.vars = c("Region", "Type"), variable.name = "MAE")

ggplot(df_long, aes(x= Region, y=value, color = Type))+
  geom_boxplot() + scale_y_continuous (breaks = seq(0, 10, by = 1)) + 
  theme(legend.position=c(0.15, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nRegion") + ylab("MAE\n") +
  ggtitle ("MAE: Diseased Vs Healthy: Region wise")




############   MSE   ############ 
df = read.csv("Compare_RegionWise_DiseasedVsHealthy_MSE_Long.csv",header = T)
#Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
df_long <- reshape2::melt (df, id.vars = c("Region", "Type"), variable.name = "MSE")

ggplot(df_long, aes(x= Region, y=value, color = Type))+
  geom_boxplot() + scale_y_continuous (breaks = seq(0, 150, by = 10)) + 
  theme(legend.position=c(0.15, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  xlab("\nRegion") + ylab("MSE\n") +
  ggtitle ("MSE: Diseased Vs Healthy: Region wise")
