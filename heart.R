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
<<<<<<< HEAD
summary(lm(score ~ mean_heart,data = mean_dat))
summary(lm(score ~ mean_heart + age + female + worldranking + rankinground_rank 
           + stage + set_number + shoot_order + countdown + arrows,data = mean_dat))
=======
summary(lm(score ~ mean_heart,data=mean_dat))
summary(lm(score ~ mean_heart + age + female + worldranking + rankinground_rank 
           + stage + set_number + shoot_order + countdown + arrows,data=mean_dat))
>>>>>>> f89c84dc6fe5667838a9351fa510500a96fb6cc9
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
<<<<<<< HEAD
            score ~~ score
            '
fit3 <- sem(model = model3, data = mean_dat, cluster = "match_name_id")
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d
=======
            
fit3 <- sem(model = model1, estimator = "ML", missing = "fiml",
            cluster = "match_name_id", data = mean_dat, meanstructure = T)
<<<<<<< HEAD

>>>>>>> 6edeec1dc51e5cbd34db7be45859b1329d115d6e
summary(fit3, fit.measures = T, standardized = T)
=======
>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d

summary(fit3, fit.measures = T, standardized = T)
#model4
model4 <- 'level: 1
            score ~ mean_heart +  set_number + shoot_order + countdown + arrows
            level: 2
            score ~ mean_heart + age + female + worldranking + rankinground_rank
          '
fit4 <- sem(model = model4, data = mean_dat, estimator = "ML", missing = "fiml", cluster = "match_name_id")
summary(fit4, fit.measures = T, standardized = T)
#model5
model5 <- 'level: 1
            score ~ mean_heart
            mean_heart ~ set_number + shoot_order + countdown + arrows
            level: 2
            score ~ age + female + worldranking + rankinground_rank
          '
fit5 <- sem(model = model5, data = mean_dat, estimator = "ML", missing = "fiml", cluster = "match_name_id")
summary(fit5, fit.measures = T, standardized = T)
<<<<<<< HEAD
<<<<<<< HEAD
summary(fit5.5, fit.measures = T, standardized = T)
=======

fit5_1 <- sem(model = model5, data = mean_dat, cluster = "match_name_id")
summary(fit5_1, fit.measures = T, standardized = T)
>>>>>>> 6edeec1dc51e5cbd34db7be45859b1329d115d6e
=======

fit5_1 <- sem(model = model5, data = mean_dat, cluster = "match_name_id")
summary(fit5_1, fit.measures = T, standardized = T)
>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d
#model6
model6 <- 'level: 1
            score ~ mean_heart + set_number + shoot_order + countdown + arrows
            level: 2
            score ~ stage + age + female + worldranking + rankinground_rank
          '
fit6 <- sem(model = model6, data = mean_dat, estimator = "ML", missing = "fiml", cluster = "match_name_id")
summary(fit6, fit.measures = T, standardized = T)
<<<<<<< HEAD
<<<<<<< HEAD
=======
=======
>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d
##
heart_fa <- 'ind =~ age + female + rankinground_rank + worldranking
              Arrow =~ set_number + arrows + shoot_order + countdown'
fit_fa = cfa(heart_fa, data=mean_dat)
summary(fit_fa, fit.measures = T, standardized = T)
fit_fa %>% modificationindices() %>% arrange(mi)




<<<<<<< HEAD
>>>>>>> 6edeec1dc51e5cbd34db7be45859b1329d115d6e
=======
>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d
