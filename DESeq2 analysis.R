library(airway)
library(DESeq2)
library(tidyverse)

counts_data <-  read.csv("counts_data.csv")
colData <- read.csv("sample_info.csv")

all(colnames(counts_data) %in% rownames(colData))

all(colnames(counts_data) == rownames(colData))



dds <- DESeqDataSetFromMatrix(countData = counts_data,
                       colData = colData,
                       design = ~dexamethasone)


dds


keep <- rowSums(counts(dds)) >= 10

dds <- dds[keep,]

dds

dds$dexamethasone <- relevel(dds$dexamethasone, ref= "untreated")


dds <- DESeq(dds)
res <- results(dds)


summary(res)

res0.1 <- results(dds, alpha = 0.01)

summary(res0.1)


EnhancedVolcano(res0.1,
                lab = rownames(res0.1),
                x = 'log2FoldChange',
                y = 'pvalue')




