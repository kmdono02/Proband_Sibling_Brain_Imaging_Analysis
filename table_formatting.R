library(tidyverse)
library(kableExtra)

# Correlation Tables: SQC
## Primary brain measures
brain_vars <- c("EACSF","TCV","TSA","CCbody","CCGenu","CCSplenium",
                "ROI01","ROI02","ROI08","ROI41","ROI42","ROI44","ROI47","ROI48","ROI59","ROI60","ROI77")

cor_mat_ASD <- read.csv("corr_mat_ASD_SQC_Primary.csv") %>% 
  mutate(RiskGroup="HR-ASD") %>%
  select(-X)
cor_mat_Neg <- read.csv("corr_mat_Neg_SCQ_Primary.csv") %>% 
  mutate(RiskGroup="HR-Neg") %>%
  select(-X)

cor_mat_full <- data.frame(rbind(cor_mat_ASD, cor_mat_Neg)) %>%
  filter(is.na(p_value_FDR)==0)

write.csv(cor_mat_full, file="cor_mat_SCQ_primary.csv")

## Secondary brain measures
cor_mat_ASD <- read.csv("corr_mat_ASD_SQC_secondary.csv") %>% 
  mutate(RiskGroup="HR-ASD") %>%
  select(-X)
cor_mat_Neg <- read.csv("corr_mat_Neg_SQC_secondary.csv")%>% 
  mutate(RiskGroup="HR-Neg") %>%
  select(-X)

cor_mat_full <- data.frame(rbind(cor_mat_ASD, cor_mat_Neg)) %>%
  filter(is.na(p_value_FDR)==0)

write.csv(cor_mat_full, file="cor_mat_SCQ_secondary.csv")