setwd ("Z:\\Project_Top2B\\April_2023")

############   RMSE   ############ 
Non_Mot = read.csv("RMSE.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RMSE')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('Frontal','Cerebellum','Temporal','Healthy','Healthy_and_Diseased')),
                         y = RMSE, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, vjust = 0.99, hjust=1,color="black")) +
  ylab ("RMSE\n") + scale_y_continuous (breaks = seq(0, 50, by = 2)) +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("RMSE Comparison: Differents models")

############   R Square   ############ 
Non_Mot = read.csv("RSquare.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'RSquare')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level = c('Frontal', 'Cerebellum', 'Temporal', 'Healthy', 'Healthy_and_Diseased')),
                         y = RSquare, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("R Square\n") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold")) +
  theme(text = element_text(size = 18)) +
  theme(axis.text = element_text(color = "black")) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(axis.text.y = element_text(size = 14, vjust = 0.5)) +  # Adjusted vjust value
  ggtitle("R Square Comparison: Different models")



############   Pearson Correlation   ############ 
df = read.csv("Pearson_Correlation.csv",header = T)

# Reshape the dataframe to long format
df_long <- tidyr::pivot_longer(df, cols = 3:4, names_to = "Variable", values_to = "Value")

# Reorder
df_long$Type <- factor(df_long$Type, levels = c('Frontal', 'Cerebellum', 'Temporal', 'Healthy', 'Healthy_and_Diseased'))
df_long$Variable <- factor(df_long$Variable, levels = c("TestData", "DiseasedData"))

# Create the grouped bar graph
ggplot(df_long, aes(x = Type, y = Value, fill = Variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.99, hjust = 1, color = "black")) +
  labs(fill = "Type of Dataset", y = "Pearson Correlation Value\n") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold")) +
  theme(text = element_text(size = 18)) +
  theme(axis.text = element_text(color = "black")) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(axis.text.y = element_text(size = 14, vjust = 0.5)) +  # Adjusted vjust value
  ggtitle("Pearson Correlation Comparison: Different models")+
  theme(legend.position=c(0.81, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) 




############   RMSE Bar : Best Values of each model  ############ 
df = read.csv("RMSE_Bar.csv",header = T)

# Reorder
df$Type <- factor(df$Type, levels = c('Frontal', 'Cerebellum', 'Temporal', 'Healthy', 'Healthy_and_Diseased'))

# Create the grouped bar graph
ggplot(df, aes(x = Type, y = Value, fill = Type)) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size=18, angle = 50, vjust = 0.98, hjust=1, color="black")) +
  ylab ("RMSE\n") + scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(color="black",face="bold")) + 
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(face="bold", size=22)) +
  ggtitle ("RMSE: Different models")


############   R Square Bar : Best Values of each model  ############ 
df = read.csv("RSquare_Bar.csv",header = T)

# Reorder
df$Type <- factor(df$Type, levels = c('Frontal', 'Cerebellum', 'Temporal', 'Healthy', 'Healthy_and_Diseased'))

# Create the grouped bar graph
ggplot(df, aes(x = Type, y = Value, fill = Type)) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size=18, angle = 50, vjust = 0.98, hjust=1, color="black")) +
  ylab ("R Square\n") + scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(color="black",face="bold")) + 
  theme(text = element_text(size=18)) +
  theme(plot.title = element_text(face="bold", size=22)) +
  ggtitle ("R Square: Different models")
