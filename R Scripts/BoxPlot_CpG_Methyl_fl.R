setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Diseased")
df = read.csv("CpG_147BorutaPy_MergedBoth_fl.csv",header = T)

#### fl ####
df_long <- reshape2::melt (df, id.vars = c("Type"), variable.name = "CpG")
df_long$Type <- factor(df_long$Type, levels = c("Healthy", "Diseased"))
df_long$CpG <- reorder(df_long$CpG, df_long$value, median)

ggplot(df_long, aes(x = CpG, y = value, color = Type)) +
  geom_boxplot() +
  labs(x = "\nCpG", y = "Methylation Value\n", title = "Comparing methylation in healthy vs diseased: Frontal lobe") +
  scale_color_manual(values = c("blue", "red"), name = "Type") +
  theme_minimal() +
  theme(
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "azure", color = "black"),
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(face = "bold", size = 18))


# Assuming your main dataset is named "df" and the dependent variable column is named "Type"

df_top50 <- df %>%
  tidyr::gather(CpG, value, -Type) %>%
  group_by(CpG) %>%
  summarize(diff = abs(mean(value[Type == "Healthy"]) - mean(value[Type == "Diseased"]))) %>%
  arrange(desc(diff)) %>%
  top_n(50)

df_top50$CpG <- factor(df_top50$CpG, levels = df_top50$CpG[order(df_top50$diff)])
CpgNames <- df_top50$CpG
select_Top50 = df[,CpgNames] # Taking those columns out from new data


df_long <- reshape2::melt (select_Top50, id.vars = c("Type"), variable.name = "CpG")
df_long$Type <- factor(df_long$Type, levels = c("Healthy", "Diseased"))
df_long$CpG <- reorder(df_long$CpG, df_long$value, median)

ggplot(df_long, aes(x = CpG, y = value, color = Type)) +
  geom_boxplot() +
  labs(x = "\nCpG", y = "Methylation Value\n", title = "Comparing methylation in healthy vs diseased: Frontal (Top 50 CpG)") +
  scale_color_manual(values = c("blue", "red"), name = "Type") +
  theme_minimal() +
  theme(
    axis.text = element_text(face = "bold", size = 14, color = 'black'),
    axis.title = element_text(face = "bold", size = 16, color = 'black'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = 'black'),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "azure", color = "black"),
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(face = "bold", size = 18))
