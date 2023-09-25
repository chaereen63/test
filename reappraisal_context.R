library(tidyverse);library(psych);library(tidyr);library(sem); library(lavaan)
wisdom <- read_csv("HainesEtAl_data.csv")
head(wisdom)
as.data.frame(wisdom)
wisdom %>% select(SEMA_ID, Time, CTRLcln, REAP, REAP_1, ZDASS_D, ZDASS_A,
                  ZDASS_S, ZRSE, ZSOCANX, CTRL_M) -> main_wis
lmmod_D <- '
          level: 1
            REAP ~ a + p1*CTRLcln + p2*Time + p3*REAP_1
          level: 2
            a ~ b00 + b1*ZDASS_D + b2*CTRL_M
            p1 ~ b10 + c1*ZDASS_D + c2*CTRL_M
            p2 ~ b20 + d1*ZDASS_D + d2*CTRL_M
            p3 ~ b30 + e1*ZDASS_D + e2*CTRL_M'
lm_D <- sem(model = lmmod_D, data = main_wis, cluster = "SEMA_ID")
summary(lm_D)
