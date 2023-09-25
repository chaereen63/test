library(tidyverse);library(psych);library(tidyr);library(sem); library(lavaan)
wisdom <- read_csv("HainesEtAl_data.csv")
head(wisdom)
as.data.frame(wisdom)
wisdom %>% select(SEMA_ID, Time, CTRLcln, REAP, REAP_1, ZBFI_N, ZDASS_D, ZDASS_A,
                  ZDASS_S, ZRSE, ZSOCANX, CTRL_M) -> main_wis
lmmod_D <- '
          level: 1
            REAP ~ 1 + p1*CTRLcln + p2*Time + p3*REAP_1
          level: 2
            REAP ~ 1 + ZDASS_D + CTRL_M
            CTRLcln ~ 1 + ZDASS_D + CTRL_M
            Time ~ 1 + ZDASS_D + CTRL_M
            REAP_1 ~ 1 + ZDASS_D + CTRL_M'
lm_D <- sem(model = lmmod_D, data = main_wis, cluster = "SEMA_ID", verbose = T, 
            optim.method = "em", em.iter.max = 10000, em.fx.tol = 1e-08, em.dx.tol = 1e-04) #수렴이 되지 않을 경우 em 알고리즘을 이용/ 10000이상으로 설정
summary(lm_D)


#lmer package로 해보기
#install.packages("lmerTest")
library(nlme);library(lme4);library(lmerTest)
intercept.only.model <- lmer(REAP ~ 1 + ( 1 | SEMA_ID), data=main_wis) #fitting
summary(intercept.only.model)
ranef(intercept.only.model) #each regression for ID
(ICCs = 285.5/(285.5 + 341.0)) #ICCs
ranova(intercept.only.model) #significance test for the intercept variance, ccompared with null model
    # FALSE `geom_smooth()` using formula 'y ~ x' #plot..?
random.intercept.model <- lmer(REAP ~ CTRLcln + Time + REAP_1 + (1 | SEMA_ID), data = main_wis, REML = T)
summary(random.intercept.model)
ranef(random.intercept.model) #each regression for ID
(explained_lv1 <- (341.0 - 323.1)/341.0) #explained level1 variance
(explained_T <- ((285.5 + 341.0)-(173.7+323.1))/(285.5 + 341.0)) #explained total variance
context.model <- lmer(REAP ~ CTRLcln + Time + REAP_1 + CTRL_M + ZDASS_D +(1 | SEMA_ID), data = main_wis, REML = T)
summary(context.model)
intercept.as.outcome.model <- lmer(REAP ~ CTRLcln + Time + REAP_1 + CTRL_M + ZDASS_D +(1 | SEMA_ID), data = main_wis, REML = T)