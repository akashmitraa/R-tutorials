
library(airway)
library(DESeq2)
library(tidyverse)


data(airway)

#preparation of the design file
sample_info = as.data.frame(colData(airway))


sample_info %>% 
  select(2,3) %>% 
  setNames(c("cellLine", "dexamethasone")) %>% 
  mutate(dexamethasone = fct_recode(dexamethasone, "treated" = "trt", "untreated" = "untrt")) %>% 
  write.table(file = "sample_info.csv", sep = ",", col.names = T, row.names = T, quote = F)


#preparation of the counts data set
countsData <- assay(airway) %>%
  write.table(file = "counts_data.csv", sep = ',', col.names = T, row.names = T, quote = F)


read.csv('counts_data.csv')
read.csv('sample_info.csv')

