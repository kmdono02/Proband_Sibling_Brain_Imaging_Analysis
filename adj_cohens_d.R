# Generating LS means and Adj Cohen's D for Proband SCQ - Sibling Brain Dev Paper
#library
library(lsmeans)

lsmeans_pdiff <- function(formula, group_var, data, filters=NULL, digits=2){
  if(is.null(filters)==0){
    data_subset <- data %>% filter_(filters)
  } else {
    data_subset <- data
  }
  lm_fit <- lm(formula, data=data_subset)
  ls.m1 <- lsmeans(lm_fit, group_var)
  out_object <- list("lm_object"=lm_fit,
                     "lsmeans_object"=ls.m1,
                     "percent_diff"=
                       round(abs(diff(summary(ls.m1)$lsmean))/(sum(summary(ls.m1)$lsmean)/2)*100, digits))
  return(out_object)
}

adj_cohens_d <- function(formula, group_var, data, filters=NULL, cutoff=0.005, digits=2){
  if(is.null(filters)==0){
    data_subset <- data %>% filter_(filters)
  } else {
    data_subset <- data
  }
  
  N <- dim(data_subset)[1]
  n1 <- ftable(eval(parse(text=paste0("data_subset$",group_var))))[1]
  n2 <- N - n1
  
  lm_coefs <- summary(lm(formula, data_subset))$coef
  index <- which(str_detect(row.names(lm_coefs), group_var))
  group_estimate <- 
    summary(lm(formula, data_subset))$coef[index,"Estimate"] 
  pooled_se_lm <-
    sqrt((var(model.frame(formula, data_subset))[1,1]*(N-1)-((group_estimate)^2)*(n1*n2/N))/(N-2))
    
  return(list("lm_fit" = lm(formula, data_subset),
         "adj_cohens_d" = 
           ifelse(abs(group_estimate/pooled_se_lm)<cutoff&group_estimate>0, paste0("<", cutoff),
                  ifelse(abs(group_estimate/pooled_se_lm)<cutoff&group_estimate>0, paste0(">-", cutoff),
                         round(group_estimate/pooled_se_lm, digits))),
         "adj_cohens_d_noround" = group_estimate/pooled_se_lm))
}

# #### generating LS means for  TCV ####
# # TCV - V06
# model1 <- lm(TCV_V06 ~ Pro_SCQgrp + Proband_Sex + Cand_Sex + Pro_Age_SCQ_m + AgeMRI_V06, data=asd.pos)
# ls.m1 = lsmeans(model1, ~Pro_SCQgrp)
# ls.m1
# #output from LS means copied for use in calculating percent differences
# #Pro_SCQgrp lsmean    SE df lower.CL upper.CL
# #High       679855 16619 38   646211   713499
# #Low        649678 22257 38   604620   694734
# pdif <- ((abs(679855-649678))/((679855+649678)/2)*100)
# pdif #4.53% diff
# 
# # TCV - V12
# model1 <- lm(TCV_V12 ~ Pro_SCQgrp + Proband_Sex + Cand_Sex + Pro_Age_SCQ_m + AgeMRI_V12, data=asd.pos)
# ls.m1 = lsmeans(model1, ~Pro_SCQgrp)
# ls.m1
# #output from LS means copied for use in calculating percent differences
# #Pro_SCQgrp lsmean    SE df lower.CL upper.CL
# #High       827940 18604 33   790091   865790
# #Low        780037 27456 33   724178   835897
# pdif <- ((abs(827940-780037))/((827940+780037)/2)*100)
# pdif #5.96% diff
# 
# # TCV - V24
# model1 <- lm(TCV_V24 ~ Pro_SCQgrp + Proband_Sex + Cand_Sex + Pro_Age_SCQ_m + AgeMRI_V24, data=asd.pos)
# ls.m1 = lsmeans(model1, ~Pro_SCQgrp)
# ls.m1
# #output from LS means copied for use in calculating percent differences
# #Pro_SCQgrp lsmean    SE df lower.CL upper.CL
# #High       961019 28421 41   903622  1018416
# #Low        911642 32729 41   845544   977739
# pdif <- ((abs(961019-911642))/((961019+911642)/2)*100)
# pdif #5.27% diff
# 
# #### generating LS means for  TSA ####
# # TSA - V06
# model1 <- lm(TSA_V06 ~ Pro_SCQgrp + Proband_Sex + Cand_Sex + Pro_Age_SCQ_m + AgeMRI_V06, data=asd.pos)
# ls.m1 = lsmeans(model1, ~Pro_SCQgrp)
# ls.m1
# #output from LS means copied for use in calculating percent differences
# #Pro_SCQgrp lsmean   SE df lower.CL upper.CL
# #High        59867 2736 27    54254    65480
# #Low         57427 2622 27    52047    62807
# pdif <- ((abs(59867-57427))/((59867+57427)/2)*100)
# pdif #4.16% diff
# 
# # TSA - V12
# model1 <- lm(TSA_V12 ~ Pro_SCQgrp + Proband_Sex + Cand_Sex + Pro_Age_SCQ_m + AgeMRI_V12, data=asd.pos)
# ls.m1 = lsmeans(model1, ~Pro_SCQgrp)
# ls.m1
# #output from LS means copied for use in calculating percent differences
# #Pro_SCQgrp lsmean   SE df lower.CL upper.CL
# #High        67585 1331 33    64877    70294
# #Low         63012 1671 33    59612    66412
# pdif <- ((abs(67585-63012))/((67585+63012)/2)*100)
# pdif #7.00% diff
# 
# # TSA - V24
# model1 <- lm(TSA_V24 ~ Pro_SCQgrp + Proband_Sex + Cand_Sex + Pro_Age_SCQ_m + AgeMRI_V24, data=asd.pos)
# ls.m1 = lsmeans(model1, ~Pro_SCQgrp)
# ls.m1
# #output from LS means copied for use in calculating percent differences
# #Pro_SCQgrp lsmean   SE df lower.CL upper.CL
# #High        79180 1931 40    75278    83081
# #Low         74697 2218 40    70214    79180
# pdif <- ((abs(79180-74697))/((79180+74697)/2)*100)
# pdif #5.83% diff
