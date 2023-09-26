
setwd ("Z:\\Project_Top2B\\April_2023\\genes")
## Details available on the link below ##
## https://yulab-smu.top/biomedical-knowledge-mining-book/enrichplot.html 

library(DOSE)
library(enrichR)
library(ggupset)
library(biomaRt)
library(gProfileR)
library(ReactomePA)
library(enrichplot)
library(ggnewscale)
library(org.Hs.eg.db)
library(clusterProfiler)




#####  Temporal  ####
df <- read.delim("Gene_Temporal.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.2)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.6,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot







#####  Cerbellum  ####
df <- read.delim("Gene_Cerebellum.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.7)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.6,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot




#####  Frontal  ####
df <- read.delim("Gene_Frontal.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.7)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.9,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot






#####  Healthy (all)  ####
df <- read.delim("Gene_Healthy.txt", header = F) #Gene List in text file
df <- as.character(df$V1)

GeneInfo <- enrichGO(df, keyType = "SYMBOL", pvalueCutoff = 0.05,ont = "all", OrgDb = "org.Hs.eg.db")
GeneInfo@result[6] #pValue
GeneID <- AnnotationDbi::select(org.Hs.eg.db,keytype = "SYMBOL",keys=df,columns=c("SYMBOL","ENTREZID"))

#Disease Pathway
myEnrich <- enrichDGN(GeneID$ENTREZID, pvalueCutoff = 0.7)
barplot(myEnrich, showCategory = 20)
termsim <- pairwise_termsim(myEnrich) #Similarity Matrix
myEnrichName <- setReadable(myEnrich, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim) #View plot
upsetplot(myEnrich) #View Upset Plot

#KEGGPathway
myEnrichKegg = enrichPathway (GeneID$ENTREZID,organism = "human",pvalueCutoff = 0.9,pAdjustMethod = "BH",minGSSize = 5)
barplot(myEnrichKegg)
termsim1 <- pairwise_termsim(myEnrichKegg)
myEnrichName <- setReadable(myEnrichKegg, 'org.Hs.eg.db', 'ENTREZID')
cnetplot(myEnrichName, categorySize="pvalue", circular = TRUE, colorEdge = TRUE,node_label="all", showCategory = 10)
emapplot(termsim1) #View plot
upsetplot(myEnrichKegg)  #View Upset Plot
