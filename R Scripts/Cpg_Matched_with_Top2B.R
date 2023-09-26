setwd ("Z:\\Project_Top2B")

top2b <- read.csv("GSM4205700_TOP2B_peaks_MCF7_R1.bed", sep = "\t")
tail(top2b)
names(top2b) = c("chr", "start", "end", "X")
dif = top2b$end-top2b$start
summary(dif)

cpg = read.csv("CpG450k_onlyHMM.csv")
head(cpg)
dim(cpg)
diff = cpg$END-cpg$Start
summary(diff)
cpg$CHR = paste0("chr",cpg$CHR)

  matches = data.frame(c())
  for (i in 1:nrow(top2b)) {
    for (k in 1:nrow(cpg)) {
      if(
        (top2b$chr[i] == cpg$CHR[k]) &&
        ((top2b$start[i]>=cpg$Start[k] && top2b$start[i] <= cpg$END[k]) ||
        (top2b$end[i]>=cpg$Start[k] && top2b$end[i] <= cpg$END[k]))
      ){
        #print(top2b[i,])
        #print(cpg[k,])
        matches = rbind(matches, cpg[k,])
      
      }
    }
  }
write.csv (matches, "Cpg_Matched_with_Top2B.csv")
