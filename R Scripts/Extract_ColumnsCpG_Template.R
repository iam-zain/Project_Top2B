setwd ("Z:\\Project_Top2B\\April_2023\\Files_Top2B\\CpG_inRow")
df <- read.csv("CpG_236BorutaPy_onAll.csv", header = T)

setwd ("Z:\\Project_Top2B\\April_2023\\diseasewise\\schiz")
NewData <- vroom("Merged_Schiz_Top2B_AgeMean.csv")
head(NewData[1:4,1:9])
#NewData <- NewData  %>% select(-c(1))
sum(is.na(NewData))

CpgNames = colnames(df[,2:237]) # Selecting Column names from our model
SelectCpGsRaw = NewData[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$OriginalAge <- NewData$Age # Adding original age in this dataset

write.csv(SelectCpGsRaw, 'Merged_Top2B_236CpG_schiz.csv', row.names = F)

