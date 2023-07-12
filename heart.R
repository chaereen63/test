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

##회귀식 하나씩##
#model1
model1 <- 'score ~ mean_heart'
fit_1 = sem(model = model1, estimator = "ML", missing = "fiml",
                data = mean_dat, meanstructure = T)
summary(fit_1, fit.measures = T, standardized = T)
#model2
model2 <- 'score ~ mean_heart + age + female + worldranking + rankinground_rank 
          + stage + set_number + shoot_order + countdown + arrows'
fit2 <- sem(model = model2, estimator = "ML", missing = "fiml",
            data = mean_dat, meanstructure = T)
summary(fit2, fit.measures = T, standardized = T)
#model3
model3 <- 'score ~ mean_heart'
fit3 <- sem(model = model3, estimator = "ML", missing = "fiml",
            data = mean_dat, meanstructure = T, index = mean_dat$match_name_id)   #id를 match_name_id 기준으로 분석하려면 어떻게 해야하지..?
summary(fit3, fit.measures = T, standardized = T)

##
heart_fa <- 'ind =~ age + female + rankinground_rank + worldranking
              Arrow =~ set_number + arrows + shoot_order + countdown'
fit_fa = cfa(heart_fa, data=mean_dat)
summary(fit_fa, fit.measures = T, standardized = T)
fit_fa %>% modificationindices() %>% arrange(mi)

#측정변수로만
heart_simp <- 'score ~ mean_heart + female + rankinground_rank + set_number + shoot_order
              mean_heart ~ set_number + arrows + shoot_order + countdown + worldranking + rankinground_rank'

fit_heart0 = sem(model = heart_simp, estimator = "ML", missing = "pairwise",
                data = mean_dat, meanstructure = F)
#varTable(fit_heart0)
summary(fit_heart0, fit.measures = T, standardized = T)
fit_heart0 %>% modificationindices() %>% arrange(mi)
semPaths(fit_heart0, what = 'est', style = 'lisrel')

####latent####
heart_mod <- 'ind =~ age + female + rankinground_rank + worldranking
              Arrow =~ set_number + arrows + shoot_order + countdown
              score ~ mean_heart
              mean_heart ~ ind + Arrow'

fit_heart = sem(model = heart_mod, estimator = "ML", missing = "fiml",
                data = mean_dat, meanstructure = T)
varTable(fit_heart)
summary(fit_heart, fit.measures = T, standardized = T)

