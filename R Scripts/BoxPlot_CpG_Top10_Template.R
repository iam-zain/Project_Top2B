setwd ("Z:\\Project_Top2B\\April_2023\\regionwise\\frontal_lobe\\Diseased")
df = read.csv("CpG_147BorutaPy_MergedBoth_fl.csv",header = T)

options(scipen = 999)
df_diff <- df %>%
  tidyr::gather(CpG, value, -Type) %>%
  group_by(CpG) %>%
  summarize(diff = (mean(value[Type == "Healthy"]) - mean(value[Type == "Diseased"]))) %>%
  arrange(desc(diff)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

write.csv(df_diff, 'Methylation_UpDownList_Frontal.csv', row.names = F)

###### Up regulated  ######
df_up15 <- df_diff %>% top_n(15, wt = -diff)
select_up15 = df[,df_up15$CpG] # Taking those columns out from new data
select_up15$Type = df$Type
df_long_up <- reshape2::melt (select_up15, id.vars = c("Type"), variable.name = "CpG")
df_long_up$Type <- factor(df_long_up$Type, levels = c("Healthy", "Diseased"))
df_long_up$CpG <- reorder(df_long_up$CpG, df_long_up$value, median)

ggplot(df_long_up, aes(x = CpG, y = value, color = Type)) +
  geom_boxplot() +
  labs(x = "\nCpG", y = "Methylation Value\n", title = "Methylation in healthy vs diseased: Frontal (upregulated 15 CpG)") +
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


#####  Down regulated  #####
df_down15 <- df_diff %>% top_n(15, wt = diff)

df_down15$CpG <- factor(df_down15$CpG, levels = df_down15$CpG[order(df_down15$diff)])
CpgNames_down <- df_down15$CpG
select_down15 = df[,df_down15$CpG] # Taking those columns out from new data
select_down15$Type = df$Type
df_long_down <- reshape2::melt (select_down15, id.vars = c("Type"), variable.name = "CpG")
df_long_down$Type <- factor(df_long_down$Type, levels = c("Healthy", "Diseased"))
df_long_down$CpG <- reorder(df_long_down$CpG, df_long_down$value, median)

ggplot(df_long_down, aes(x = CpG, y = value, color = Type)) +
  geom_boxplot() +
  labs(x = "\nCpG", y = "Methylation Value\n", title = "Methylation in healthy vs diseased: Frontal (downregulated 15 CpG)") +
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

