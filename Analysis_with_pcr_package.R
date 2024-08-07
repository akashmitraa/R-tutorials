library(pcr)
library(tidyverse)
library(readxl)
library(RColorBrewer)


pcr_data <- read_excel("Desktop/test.xlsx")

pcr <- pcr_data %>% 
  select(2:3)

grp <- c(rep(c('control', 't1', 't2', 't3'), c(2,2,2,2)))


qpcr <- pcr %>% 
  pcr_analyze(group_var = grp,
              reference_gene = 'gapdh',
              reference_group = 'control',
              method = 'delta_delta_ct')



qpcr %>%
  ggplot(aes(group, relative_expression)) +
  geom_bar(stat = "identity", color = "black", fill = brewer.pal(4, "Pastel1"))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width=0.2)+
  theme_classic()+
  labs(x = "Treatment",
       y = "Fold change (mRNA expression)")+
  theme(plot.margin = margin(1,1,1,1, "cm"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -2),
        axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,2.5), expand = c(0,0))

