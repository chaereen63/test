#install.packages("lmerTest")
library(tidyverse);library(psych);library(tidyr);library(sem); library(lavaan)
library(nlme);library(lme4);library(lmerTest)
wisdom <- read_csv("HainesEtAl_data.csv")
head(wisdom)
as.data.frame(wisdom)
wisdom %>% select(SEMA_ID, Time, CTRLcln, REAP, REAP_1, ZBFI_N, ZDASS_D, ZDASS_A,
                  ZDASS_S, ZRSE, ZSOCANX, CTRL_M) -> main_wis
#multilevel SEM
lmmod_D <- '
          level: 1
            REAP ~ 1 + p1*CTRLcln + p2*Time + p3*REAP_1
          level: 2
            REAP ~ 1 + ZDASS_D + CTRL_M
            CTRLcln ~ 1 + ZDASS_D + CTRL_M
            Time ~ 1 + ZDASS_D + CTRL_M
            REAP_1 ~ 1 + ZDASS_D + CTRL_M'
#lm_D <- sem(model = lmmod_D, data = main_wis, cluster = "SEMA_ID", verbose = T, 
#            optim.method = "em", em.iter.max = 10000, em.fx.tol = 1e-08, em.dx.tol = 1e-04) #수렴이 되지 않을 경우 em 알고리즘을 이용/ 10000이상으로 설정
#summary(lm_D)

#lmer package로 해보기
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
random.coefficients.model <- lmer(REAP ~ CTRLcln + Time + REAP_1 + (CTRLcln + Time + REAP_1 | SEMA_ID), data = main_wis, REML = T)
summary(random.coefficients.model)
ranova(random.coefficients.model)
context.model <- lmer(REAP ~ CTRLcln + Time + REAP_1 + (CTRL_M + ZDASS_D | SEMA_ID), data = main_wis, REML = T)
summary(context.model)

main_wis %>% mutate(ID = as.factor(SEMA_ID)) -> main_wis

  ###lme function###
intercept.only <- lme(REAP ~ 1,
                      random = ~1 | SEMA_ID,
                      main_wis, method = "ML", na.action = na.omit)
summary(intercept.only)
VarCorr(intercept.only)#lme는 random 수준에서 표준화값만 나타남
random.intercept <- lme(REAP ~ CTRLcln + Time + REAP_1,
                        random = ~1 | SEMA_ID,
                        main_wis, method = "ML", na.action = na.omit)
summary(random.intercept)
VarCorr(random.intercept)
random.coefficients <- lme(REAP ~  CTRLcln + Time + REAP_1,
                           random = ~ CTRLcln + Time + REAP_1 | SEMA_ID,
                           main_wis, method = "ML", na.action = na.omit)
summary(random.coefficients)
VarCorr(random.coefficients)
ranef(random.coefficients)["CTRLcln"] %>% round(2) %>% tail(20) #residuals of the random slope
anova(random.coefficients, random.intercept)
#slope-as-outcome model
slope.outcome <- lme(REAP ~  CTRLcln*ZDASS_D + Time*ZDASS_D + REAP_1*ZDASS_D + CTRLcln*CTRL_M + Time*CTRL_M + REAP_1*CTRL_M,
                           random = ~ CTRLcln + Time + REAP_1 | SEMA_ID,
                           main_wis, method = "ML", na.action = na.omit)
summary(slope.outcome)
VarCorr(slope.outcome)
intercept.outcome <- lme(REAP ~ CTRLcln + Time + REAP_1 + ZDASS_D + CTRL_M,
                     random = ~ CTRLcln + Time + REAP_1 | SEMA_ID,
                     main_wis, method = "ML", na.action = na.omit)
summary(intercept.outcome)
VarCorr(intercept.outcome)
intercept.slope.outcome <- lme(REAP ~  CTRLcln + Time + REAP_1 + ZDASS_D + CTRL_M + CTRLcln*ZDASS_D + Time*ZDASS_D + REAP_1*ZDASS_D + CTRLcln*CTRL_M + Time*CTRL_M + REAP_1*CTRL_M,
                     random = ~ CTRLcln + Time + REAP_1 | SEMA_ID,
                     main_wis, method = "ML", na.action = na.omit)
summary(intercept.slope.outcome)
anova(intercept.slope.outcome, intercept.outcome)
anova(intercept.slope.outcome, slope.outcome)

intercept.slope.outcome_A<- lme(REAP ~  CTRLcln + Time + REAP_1 + ZDASS_A + CTRL_M + CTRLcln*ZDASS_A + Time*ZDASS_A + REAP_1*ZDASS_A + CTRLcln*CTRL_M + Time*CTRL_M + REAP_1*CTRL_M,
                               random = ~ CTRLcln + Time + REAP_1 | SEMA_ID,
                               main_wis, method = "ML", na.action = na.omit)
summary(intercept.slope.outcome_A)
