setwd("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Diseased")
df <- read.csv("BS_Age_Compare_Frontal.csv", header = TRUE)


#######  Barplot #######
df$Points = 1:438
df_long <- tidyr::gather(df, key = "Feature", value = "Value", -Points)

ggplot(df_long, aes(x = factor(Points), y = Value, fill = Feature)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Age (in Years)", fill = "Feature") +
  ggtitle("Age comparison: Original vs Predicted") +
  theme_minimal()





#######  Above or Below plot #######
df <- read.csv("BS_Age_Compare_Frontal.csv", header = TRUE)
# Create a new column indicating whether Feature2 is above or below Feature1
df$Comparison <- ifelse(df$PredictAge > df$OriginalAge, 1, 0)

# Define colors based on Comparison values
colors <- c("green", "red")

# Plot the data
plot(df$OriginalAge, df$PredictAge, col = colors[df$Comparison + 1], pch = 16, xlab = "Original Age", ylab = "Predicted Age")

# Add a legend
legend("topleft", legend = c("Below", "Above"), col = colors, pch = 16)






#######  Count Plot  #######
df <- read.csv("BS_Age_Compare_Frontal.csv", header = TRUE)

# Create a new column indicating whether Predicted Age is above or below original age
df$Comparison <- ifelse(df$PredictAge > df$OriginalAge, "Above", "Below")

# Count the occurrences of "Above" and "Below"
counts <- table(df$Comparison)

# Convert counts to a data frame
counts_df <- data.frame(Comparison = names(counts), Freq = as.numeric(counts))

# Create a bar plot using ggplot2
ggplot(data = counts_df, aes(x = factor(Comparison), y = Freq, fill = Comparison)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("red", "green")) +
  labs(x = "Comparison of Predicted Age with Original Age", y = "Count") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(color = "black", size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.position = "none") +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 4)







#######  Conditional Bar Plot: Only higher value will be plotted  #######
df <- read.csv("BS_Age_Compare_Frontal.csv", header = TRUE)

# Get the column index with the highest value for each row
max_columns <- apply(df, 1, which.max)

# Create a new dataframe for plotting
plot_df <- data.frame(Row = 1:nrow(df), Value = df[cbind(1:nrow(df), max_columns)],
  Column = factor(max_columns, labels = c("OriginalAge", "PredictAge")),
  Color = factor(max_columns, labels = c("Original Age", "Predict Age")))

# Plot the bar graph using ggplot2
ggplot(plot_df, aes(x = Row, y = Value, fill = Color)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "green")) +
  labs(x = "", y = "Age (in Years)", title = "Higher Value Bar Graph") +
  theme_minimal() +
  theme(legend.title = element_blank())
