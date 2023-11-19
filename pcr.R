#setting working directory
setwd("~/My Drive/Academics/PhD/Experiment-results/Lipoxin_study")

#insatlling required packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("plotrix")
install.packages("ggsignif")

#importing libraries
library(tidyverse)
library(plotrix)
library(ggsignif)
library(readxl)


#importing dataset from excel
atp6 = read_excel("PCR/lipoxin-study-14102023-PCR-2.xls", 
                   sheet = "atp6")
View(atp6)

#data cleaning
atp = atp6 %>% 
  select(Treatment,5,9,13) %>% 
  setNames(c("treatment", "fc1", "fc2", "fc3")) %>% 
  pivot_longer(2:4, names_to = "reps", values_to = "fold_change") %>% 
  select(1,3) %>% 
  mutate(treatment = fct_relevel(treatment, "Control", "Glc", "LXA4", "G+L")) %>% 
  mutate(treatment = fct_recode(treatment, "40mM\nGlc" = "Glc",
                                "100ng/ml\nLXA4" = "LXA4",
                                "Glc+LXA4" = "G+L")) %>% 
  group_by(treatment)


#Statistical analysis (normality check)
atp6 %>% 
  select(Treatment,5,9,13) %>% 
  setNames(c("treatment", "fc1", "fc2", "fc3")) %>% 
  t() %>% 
  as.data.frame() %>% 
  setNames(.[1,]) %>% 
  slice(-1) %>% 
  mutate_all(as.numeric) %>% 
  summarise(glc = shapiro.test(Glc)$p.value,
            lxa4 = shapiro.test(LXA4)$p.value,
            `g+l` = shapiro.test(`G+L`)$p.value)

#Statistical analysis (ANOVA and tukey's post hoc)
atp %>% 
  aov(fold_change~treatment, data = .) %>% 
  summary()

atp %>% 
  aov(fold_change~treatment, data = .) %>% 
  TukeyHSD()


#plotting the graph
atp %>% 
  summarize(mean_fc = mean(fold_change),
            sem_fc = std.error(fold_change),
            min_fc = min(mean_fc - sem_fc),
            max_fc = max(mean_fc + sem_fc)) %>% 
  ggplot(aes(treatment, mean_fc))+
  geom_bar(stat = "identity", color = "black", fill = "skyblue", alpha = 0.5)+
  geom_errorbar(aes(ymin = min_fc, ymax = max_fc), width = 0.2)+
  labs(x = "Treatment", y = "Fold change (mRNA expression)", title = "Atp6")+
  theme_classic()+
  scale_y_continuous(expand = c(0,0), limits = c(0,2.5))+
  geom_jitter(aes(treatment, fold_change), atp, width = 0.2, alpha = 0.7, size = 3)+
  geom_signif(comparisons = list (c("Control", "40mM\nGlc"),
                                  c("40mM\nGlc", "Glc+LXA4"),
                                  c("Control", "100ng/ml\nLXA4")),
              map_signif_level = TRUE,
              y_position = c(1.2, 1.6, 2),
              annotations = c("p = 0.0010852\n**", "p = 0.1582812\nNS", "p = 0.3122380\nNS"), textsize = 4 )


