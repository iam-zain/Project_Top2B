## Age difference Plot


# Cerebellum
setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\cerebellum\\Diseased")
df = read.csv("BS_Age_Compare_Cerebellum.csv",header = T)
df$Difference = (df$PredictAge - df$OriginalAge)
write.csv(df, 'Diff_Age_Compare_Cerebellum.csv', row.names = F)

ggplot(df, aes(x = seq_along(Difference), y = Difference)) +
  geom_bar(stat = "identity", aes(fill = Difference > 0)) +
  scale_fill_manual(values = c('green', 'red'), labels = c('Below', 'Above')) +
  labs(fill = "") +  # Defining legend title
  theme(axis.text.x = element_text(size = 18, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("Age difference\n") +
  scale_y_continuous(breaks = seq(-80, 80, by = 5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(text = element_text(size = 18)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  ggtitle("Cerebellum: Barplot of Age Difference: Predicted - Original") +
  theme(legend.position = "bottom")  # Defining legend position

  

# Frontal
setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\Frontal_lobe\\Diseased")
df = read.csv("BS_Age_Compare_Frontal.csv",header = T)
df$Difference = (df$PredictAge - df$OriginalAge)
write.csv(df, 'Diff_Age_Compare_Frontal.csv', row.names = F)

ggplot(df, aes(x = seq_along(Difference), y = Difference)) +
  geom_bar(stat = "identity", aes(fill = Difference > 0)) +
  scale_fill_manual(values = c('green', 'red'), labels = c('Below', 'Above')) +
  labs(fill = "") +  # Defining legend title
  theme(axis.text.x = element_text(size = 18, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("Age difference\n") +
  scale_y_continuous(breaks = seq(-80, 80, by = 5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(text = element_text(size = 18)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  ggtitle("Frontal: Barplot of Age Difference: Predicted - Original") +
  theme(legend.position = "bottom")  # Defining legend position



# Temporal
setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\Temporal_lobe\\Diseased")
df = read.csv("BS_Age_Compare_Temporal.csv",header = T)
df$Difference = (df$PredictAge - df$OriginalAge)
write.csv(df, 'Diff_Age_Compare_Temporal.csv', row.names = F)

ggplot(df, aes(x = seq_along(Difference), y = Difference)) +
  geom_bar(stat = "identity", aes(fill = Difference > 0)) +
  scale_fill_manual(values = c('green', 'red'), labels = c('Below', 'Above')) +
  labs(fill = "") +  # Defining legend title
  theme(axis.text.x = element_text(size = 18, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("Age difference\n") +
  scale_y_continuous(breaks = seq(-80, 80, by = 5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(text = element_text(size = 18)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  ggtitle("Temporal: Barplot of Age Difference: Predicted - Original") +
  theme(legend.position = "bottom")  # Defining legend position





# AD
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\AD")
df = read.csv("BS_Age_Compare_AD_All.csv",header = T)
df$Difference = (df$PredictAge - df$OriginalAge)
write.csv(df, 'Diff_Age_Compare_AD.csv', row.names = F)

ggplot(df, aes(x = seq_along(Difference), y = Difference)) +
  geom_bar(stat = "identity", aes(fill = Difference > 0)) +
  scale_fill_manual(values = c('green', 'red'), labels = c('Below', 'Above')) +
  labs(fill = "") +  # Defining legend title
  theme(axis.text.x = element_text(size = 18, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("Age difference\n") +
  scale_y_continuous(breaks = seq(-80, 80, by = 5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(text = element_text(size = 18)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  ggtitle("AD: Barplot of Age Difference: Predicted - Original") +
  theme(legend.position = "bottom")  # Defining legend position




# Schizophrenia
setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\schiz")
df = read.csv("BS_Age_Compare_SchizAll.csv",header = T)
df$Difference = (df$PredictAge - df$OriginalAge)
write.csv(df, 'Diff_Age_Compare_Schiz.csv', row.names = F)

ggplot(df, aes(x = seq_along(Difference), y = Difference)) +
  geom_bar(stat = "identity", aes(fill = Difference > 0)) +
  scale_fill_manual(values = c('green', 'red'), labels = c('Below', 'Above')) +
  labs(fill = "") +  # Defining legend title
  theme(axis.text.x = element_text(size = 18, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("Age difference\n") +
  scale_y_continuous(breaks = seq(-80, 80, by = 5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(text = element_text(size = 18)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  ggtitle("Schizophrenia: Barplot of Age Difference: Predicted - Original") +
  theme(legend.position = "bottom")  # Defining legend position





# Merged All
setwd ("Z:\\Project_Top2B\\April_2023\\Diseased_All")
df = read.csv("BS_Age_Compare_All.csv",header = T)
df$Difference = (df$PredictAge - df$OriginalAge)
write.csv(df, 'Diff_Age_Compare_All.csv', row.names = F)

ggplot(df, aes(x = seq_along(Difference), y = Difference)) +
  geom_bar(stat = "identity", aes(fill = Difference > 0)) +
  scale_fill_manual(values = c('green', 'red'), labels = c('Below', 'Above')) +
  labs(fill = "") +  # Defining legend title
  theme(axis.text.x = element_text(size = 18, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("Age difference\n") +
  scale_y_continuous(breaks = seq(-80, 80, by = 5)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text = element_text(face = "bold", color = "black")) +
  theme(text = element_text(size = 18)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  ggtitle("Merged All: Barplot of Age Difference: Predicted - Original") +
  theme(legend.position = "bottom")  # Defining legend position

