#Heart Rate and Performance
library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)

org_dat = read.csv("archery_data.csv")
head(org_dat)

#mean of heart rate
heart = c('heart_before1', 'heart_before2', 'heart_before3', 'heart_before4',
          'heart_before5', 'heart_before6', 'heart_before7', 'heart_before8',
          'heart_before9','heart_before10','heart_before11','heart_before12')
org_dat %>%
  mutate(mean_heart = rowMeans(cur_data()[,heart], na.rm=T)) -> mean_dat

heart_mod <- 'score ~ mean_heart
              mean_heart ~ ind + A
              ind ~~ age +female + rankinground_rank + worldranking
              A ~~ set_number + arrows + shoot_order + countdown'

fit_heart = sem(model = heart_mod, estimator = "ML", missing = "fiml",
                data = mean_dat, meanstructure = T)
summary(fit_heart, fit.measures = T, standardized = T)

