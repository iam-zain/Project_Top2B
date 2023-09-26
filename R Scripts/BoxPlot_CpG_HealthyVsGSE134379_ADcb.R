setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow")
df = read.csv("236_HealthyVsAD134379.csv",header = T)

#### AD cb ####
df_long <- reshape2::melt (df, id.vars = c("Type"), variable.name = "CpG")
df_long$Type <- factor(df_long$Type, levels = c("Healthy", "AD"))
df_long$CpG <- reorder(df_long$CpG, df_long$value, median)

ggplot(df_long, aes(x = CpG, y = value, color = Type)) +
  geom_boxplot() +
  labs(x = "\nCpG", y = "Methylation Value\n", title = "Comparing methylation in healthy vs AD GSE134379") +
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




##### ! Some Issue with selecting CpGs here in this script ! #####
# Calculate the absolute difference between Healthy and AD values for each CpG
df_diff <- df_long %>%
  group_by(CpG) %>%
  reframe(Difference = abs(diff(value))) %>%
  arrange(desc(Difference), CpG) %>%
  slice_head(n = 500) %>%
  pull(CpG)

# Filter the data for the top 50 CpGs
df_top50 <- df_long %>% filter(CpG %in% df_diff)

# Assuming your main dataset is named "df" and the dependent variable column is named "Type"

df_top50 <- df %>%
  tidyr::gather(CpG, value, -Type) %>%
  group_by(CpG) %>%
  summarize(diff = abs(mean(value[Type == "Healthy"]) - mean(value[Type == "AD"]))) %>%
  arrange(desc(diff)) %>%
  top_n(30)

df_top50$CpG <- factor(df_top50$CpG, levels = df_top50$CpG[order(df_top50$diff)])
CpgNames <- df_top50$CpG
select_Top50 = df[,CpgNames] # Taking those columns out from new data


df_long <- reshape2::melt (select_Top50, id.vars = c("Type"), variable.name = "CpG")
df_long$Type <- factor(df_long$Type, levels = c("Healthy", "AD"))
df_long$CpG <- reorder(df_long$CpG, df_long$value, median)

ggplot(df_long, aes(x = CpG, y = value, color = Type)) +
  geom_boxplot() +
  labs(x = "\nCpG", y = "Methylation Value\n", title = "Comparing methylation in healthy vs AD GSE134379 (Top 30 CpG)") +
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
