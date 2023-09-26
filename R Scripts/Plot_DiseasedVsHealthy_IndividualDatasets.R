setwd ("Z:\\Project_Top2B\\April_2023\\Individual_Files\\RMSE_etc")

############   RMSE   ############ 
df = read.csv("DiseasedVsHealthy_RMSE_RandomSampling.csv",header = T)

# Get unique initials from column names
initials <- unique(str_match(colnames(df), "_(.*?)_")[, 2]) # extracting texts that lies between two underscores (_)

# Initialize an empty list to store melted data frames
melted_data_list <- list()

# Melt each pair of consecutive columns and store in the list
for (initial in initials) {
  cols <- colnames(df)[grepl(initial, colnames(df))]
  melted <- melt(df[, cols], 
                 variable.name = "Type", value.name = "Values")
  melted$Name <- initial
  melted_data_list[[initial]] <- melted
}
# Combine all melted data frames into a single data frame
melted_data <- do.call(rbind, melted_data_list)
melted_data$Type <- gsub(".*_", "", melted_data$Type)

# renaming Name column, it will now contain diseased name also
# Define your conditions and corresponding prefixes
conditions <- c('GSE66351', 'GSE76105', 'GSE80970', 'GSE105109', 'GSE109627', 'GSE125895', 'GSE134379cb', 'GSE134379mtg',
                'GSE53162','GSE59457','GSE67748','GSE67749','GSE40360','GSE61380','GSE61431','GSE74193','GSE89702')
prefixes <- c('AD','AD','AD','AD','AD','AD','AD','AD','Auti','HIIV','HIV','HIV','MS','Schiz','Schiz','Schiz','Schiz')

# Function to add prefix based on condition
add_prefix <- function(name) {
  condition_index <- which(conditions == name)
  if (length(condition_index) == 1) {
    return(paste0(prefixes[condition_index], "_", name))
  } else {
    return(name)
  }
}

# Modify 'Name' column based on conditions and prefixes
melted_data$Name <- sapply(melted_data$Name, add_prefix)

ggplot(melted_data, aes(x = Name, y = Values, color = Type)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  theme(
    legend.position = c(0.15, 0.85),
    axis.text = element_text(face = "bold", color = "black"),
    text = element_text(size = 16, color = "black"),
    legend.background = element_rect(fill = "azure", linewidth = 0.1, linetype = "solid", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 18, angle = 65, vjust = 0.99, hjust = 1, color = "black"),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold")) + 
  xlab("Dataset") + ylab("RMSE\n") +
  ggtitle("RMSE: Diseased Vs Healthy: Individual Datasets")




############   R-Square   ############ 
df = read.csv("DiseasedVsHealthy_RSquare_RandomSampling.csv",header = T)

# Get unique initials from column names
initials <- unique(str_match(colnames(df), "_(.*?)_")[, 2]) # extracting texts that lies between two underscores (_)

# Initialize an empty list to store melted data frames
melted_data_list <- list()

# Melt each pair of consecutive columns and store in the list
for (initial in initials) {
  cols <- colnames(df)[grepl(initial, colnames(df))]
  melted <- melt(df[, cols], 
                 variable.name = "Type", value.name = "Values")
  melted$Name <- initial
  melted_data_list[[initial]] <- melted
}
# Combine all melted data frames into a single data frame
melted_data <- do.call(rbind, melted_data_list)
melted_data$Type <- gsub(".*_", "", melted_data$Type)

# renaming Name column, it will now contain diseased name also
# Define your conditions and corresponding prefixes
conditions <- c('GSE66351', 'GSE76105', 'GSE80970', 'GSE105109', 'GSE109627', 'GSE125895', 'GSE134379cb', 'GSE134379mtg',
                'GSE53162','GSE59457','GSE67748','GSE67749','GSE40360','GSE61380','GSE61431','GSE74193','GSE89702')
prefixes <- c('AD','AD','AD','AD','AD','AD','AD','AD','Auti','HIIV','HIV','HIV','MS','Schiz','Schiz','Schiz','Schiz')

# Function to add prefix based on condition
add_prefix <- function(name) {
  condition_index <- which(conditions == name)
  if (length(condition_index) == 1) {
    return(paste0(prefixes[condition_index], "_", name))
  } else {
    return(name)
  }
}

# Modify 'Name' column based on conditions and prefixes
melted_data$Name <- sapply(melted_data$Name, add_prefix)

ggplot(melted_data, aes(x = Name, y = Values, color = Type)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme(
    legend.position = c(0.90, 0.15),
    axis.text = element_text(face = "bold", color = "black"),
    text = element_text(size = 16, color = "black"),
    legend.background = element_rect(fill = "azure", linewidth = 0.1, linetype = "solid", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 18, angle = 65, vjust = 0.99, hjust = 1, color = "black"),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold")) + 
  xlab("Dataset") + ylab("R-Square\n") +
  ggtitle("R-Square: Diseased Vs Healthy: Individual Datasets")




############   Correlation   ############ 
df = read.csv("DiseasedVsHealthy_Correlation_RandomSampling.csv",header = T)

# Get unique initials from column names
initials <- unique(str_match(colnames(df), "_(.*?)_")[, 2]) # extracting texts that lies between two underscores (_)

# Initialize an empty list to store melted data frames
melted_data_list <- list()

# Melt each pair of consecutive columns and store in the list
for (initial in initials) {
  cols <- colnames(df)[grepl(initial, colnames(df))]
  melted <- melt(df[, cols], 
                 variable.name = "Type", value.name = "Values")
  melted$Name <- initial
  melted_data_list[[initial]] <- melted
}
# Combine all melted data frames into a single data frame
melted_data <- do.call(rbind, melted_data_list)
melted_data$Type <- gsub(".*_", "", melted_data$Type)

# renaming Name column, it will now contain diseased name also
# Define your conditions and corresponding prefixes
conditions <- c('GSE66351', 'GSE76105', 'GSE80970', 'GSE105109', 'GSE109627', 'GSE125895', 'GSE134379cb', 'GSE134379mtg',
                'GSE53162','GSE59457','GSE67748','GSE67749','GSE40360','GSE61380','GSE61431','GSE74193','GSE89702')
prefixes <- c('AD','AD','AD','AD','AD','AD','AD','AD','Auti','HIIV','HIV','HIV','MS','Schiz','Schiz','Schiz','Schiz')

# Function to add prefix based on condition
add_prefix <- function(name) {
  condition_index <- which(conditions == name)
  if (length(condition_index) == 1) {
    return(paste0(prefixes[condition_index], "_", name))
  } else {
    return(name)
  }
}

# Modify 'Name' column based on conditions and prefixes
melted_data$Name <- sapply(melted_data$Name, add_prefix)

ggplot(melted_data, aes(x = Name, y = Values, color = Type)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1)) +
  theme(
    legend.position = c(0.90, 0.15),
    axis.text = element_text(face = "bold", color = "black"),
    text = element_text(size = 16, color = "black"),
    legend.background = element_rect(fill = "azure", linewidth = 0.1, linetype = "solid", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 18, angle = 65, vjust = 0.99, hjust = 1, color = "black"),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold")) + 
  xlab("Dataset") + ylab("Correlation\n") +
  ggtitle("Correlation: Diseased Vs Healthy: Individual Datasets")



############   MAE   ############ 
df = read.csv("DiseasedVsHealthy_MAE_RandomSampling.csv",header = T)

# Get unique initials from column names
initials <- unique(str_match(colnames(df), "_(.*?)_")[, 2]) # extracting texts that lies between two underscores (_)

# Initialize an empty list to store melted data frames
melted_data_list <- list()

# Melt each pair of consecutive columns and store in the list
for (initial in initials) {
  cols <- colnames(df)[grepl(initial, colnames(df))]
  melted <- melt(df[, cols], 
                 variable.name = "Type", value.name = "Values")
  melted$Name <- initial
  melted_data_list[[initial]] <- melted
}
# Combine all melted data frames into a single data frame
melted_data <- do.call(rbind, melted_data_list)
melted_data$Type <- gsub(".*_", "", melted_data$Type)

# renaming Name column, it will now contain diseased name also
# Define your conditions and corresponding prefixes
conditions <- c('GSE66351', 'GSE76105', 'GSE80970', 'GSE105109', 'GSE109627', 'GSE125895', 'GSE134379cb', 'GSE134379mtg',
                'GSE53162','GSE59457','GSE67748','GSE67749','GSE40360','GSE61380','GSE61431','GSE74193','GSE89702')
prefixes <- c('AD','AD','AD','AD','AD','AD','AD','AD','Auti','HIIV','HIV','HIV','MS','Schiz','Schiz','Schiz','Schiz')

# Function to add prefix based on condition
add_prefix <- function(name) {
  condition_index <- which(conditions == name)
  if (length(condition_index) == 1) {
    return(paste0(prefixes[condition_index], "_", name))
  } else {
    return(name)
  }
}

# Modify 'Name' column based on conditions and prefixes
melted_data$Name <- sapply(melted_data$Name, add_prefix)

ggplot(melted_data, aes(x = Name, y = Values, color = Type)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  theme(
    legend.position = c(0.90, 0.85),
    axis.text = element_text(face = "bold", color = "black"),
    text = element_text(size = 16, color = "black"),
    legend.background = element_rect(fill = "azure", linewidth = 0.1, linetype = "solid", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 18, angle = 65, vjust = 0.99, hjust = 1, color = "black"),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold")) + 
  xlab("Dataset") + ylab("MAE\n") +
  ggtitle("MAE: Diseased Vs Healthy: Individual Datasets")





############   MSE   ############ 
df = read.csv("DiseasedVsHealthy_MSE_RandomSampling.csv",header = T)

# Get unique initials from column names
initials <- unique(str_match(colnames(df), "_(.*?)_")[, 2]) # extracting texts that lies between two underscores (_)

# Initialize an empty list to store melted data frames
melted_data_list <- list()

# Melt each pair of consecutive columns and store in the list
for (initial in initials) {
  cols <- colnames(df)[grepl(initial, colnames(df))]
  melted <- melt(df[, cols], 
                 variable.name = "Type", value.name = "Values")
  melted$Name <- initial
  melted_data_list[[initial]] <- melted
}
# Combine all melted data frames into a single data frame
melted_data <- do.call(rbind, melted_data_list)
melted_data$Type <- gsub(".*_", "", melted_data$Type)

# renaming Name column, it will now contain diseased name also
# Define your conditions and corresponding prefixes
conditions <- c('GSE66351', 'GSE76105', 'GSE80970', 'GSE105109', 'GSE109627', 'GSE125895', 'GSE134379cb', 'GSE134379mtg',
                'GSE53162','GSE59457','GSE67748','GSE67749','GSE40360','GSE61380','GSE61431','GSE74193','GSE89702')
prefixes <- c('AD','AD','AD','AD','AD','AD','AD','AD','Auti','HIIV','HIV','HIV','MS','Schiz','Schiz','Schiz','Schiz')

# Function to add prefix based on condition
add_prefix <- function(name) {
  condition_index <- which(conditions == name)
  if (length(condition_index) == 1) {
    return(paste0(prefixes[condition_index], "_", name))
  } else {
    return(name)
  }
}

# Modify 'Name' column based on conditions and prefixes
melted_data$Name <- sapply(melted_data$Name, add_prefix)

ggplot(melted_data, aes(x = Name, y = Values, color = Type)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 800, by = 40)) +
  theme(
    legend.position = c(0.90, 0.85),
    axis.text = element_text(face = "bold", color = "black"),
    text = element_text(size = 16, color = "black"),
    legend.background = element_rect(fill = "azure", linewidth = 0.1, linetype = "solid", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 18, angle = 65, vjust = 0.99, hjust = 1, color = "black"),
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(face = "bold")) + 
  xlab("Dataset") + ylab("MSE\n") +
  ggtitle("MSE: Diseased Vs Healthy: Individual Datasets")
