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
write.csv(mean_dat,file="data2.csv")
summary(lm(score ~ mean_heart,data=mean_dat))
summary(lm(score ~ mean_heart + age + female + worldranking + rankinground_rank 
           + stage + set_number + shoot_order + countdown + arrows,data=mean_dat))
aa <- lm(score ~ mean_heart + factor(match_name_id),data=mean_dat)

summary(aa)
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
model3 <- 'level: 1
            score ~ mean_heart
            level: 2
            score
            '
fit3 <- sem(model = model1, estimator = "ML", missing = "fiml",
            cluster = "match_name_id", data = mean_dat, meanstructure = T)
summary(fit3, fit.measures = T, standardized = T)
#model4
model4 <- 'score ~ mean_heart  + set_number + shoot_order + countdown + arrows'
fit4 <- sem(model = model4, estimator = "ML", missing = "fiml",
            data = mean_dat, meanstructure = T)
summary(fit4, fit.measures = T, standardized = T)
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

