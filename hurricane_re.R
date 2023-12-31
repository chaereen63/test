##let's create time variables##
library(tidyverse)
library(lavaan)
library(psych)

hurri <- read_csv("scales_datafile_rev.csv")
head(hurri)

# 변수별 결측치
hurri %>% 
  select(hrelsat5, hrelsat6, hrelsat7, wrelsat5, wrelsat6, wrelsat7) %>% data.frame() -> na_time
# colSums(is.na(na_time)) 
NN<- rowSums(is.na(na_time))
# tf <- rowSums(is.na(na_time))==6
# sum(tf==TRUE) -> na
na #허리케인 이후 보고가 0인 부부의 수
#각 행에 대한 결측치 수를 계산하고 이것이 6일 경우의 행의 개수를 산출해야 함. 해결!

# 위의 64쌍의 부부를 구분하는 더미변수 생성(개수만 센거라 위 데이터로 가능할지 모르겠다)
hurri %>%
  mutate(NN = replace(NN, rowSums(is.na(na_time)) == 6, 1)) %>%
  mutate(NN = replace(NN, rowSums(is.na(na_time)) != 6, 0))-> hurri_na #success~*^0^* replace 함수로 특정 열의 값 대체
hurri_na %>% filter(NN==1) %>% count()
# NN 변수로 두 부부집단의 초기 관계만족도 차이 검정(t-test)
t.test(hrelsat1 ~ NN, hurri_na, var.equal=TRUE)
t.test(wrelsat1 ~ NN, hurri_na, var.equal=TRUE)
# 사건 직전 social support, perceived stress 차이 검정
t.test(hps3 ~ NN, hurri_na, var.equal=TRUE)
t.test(wps3 ~ NN, hurri_na, var.equal=TRUE)
t.test(hsupp3 ~ NN, hurri_na, var.equal=TRUE)
t.test(wsupp3 ~ NN, hurri_na, var.equal=TRUE)
<<<<<<< HEAD
#기술통계표 만들기
hurri_na %>% 
  select(hrelsat1, hrelsat2, hrelsat3, hrelsat5, 
                     hrelsat6, hrelsat7, hexposure, hps3, hsupp3) %>% mean() -> table
summary(hurri_na)
=======

#기술통계
hurri_na %>% select(hrelsat1, hrelsat2, hrelsat3, hrelsat5, hrelsat6, hrelsat7, hexposure, hps3, hsupp3,
                    wrelsat1, wrelsat2, wrelsat3, wrelsat5, wrelsat6, wrelsat7, wexposure, wps3, wsupp3) %>%
  summary() -> summar
>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d

## main effect analysis
mainhr <- read_csv("scales_datafile_v3long_rev.csv")
# 시간변수 만들기: pre-hurricane slope, post-hurricane slope, jump to use in piecewise regression
mainhr %>% mutate(preHw = ifelse(time<0, Wife*time, 0), preHh = ifelse(time<0, Husb*time, 0), 
                  postHw = ifelse(time>0, Wife*time, 0), postHh = ifelse(time>0, Husb*time, 0),
                  WJump = ifelse(time>0,Wife*1,0), HJump = ifelse(time>0,Husb*1,0))-> mainHr #귀찮아서 Wife, Husb 곱하는 절차를 합침.

<<<<<<< HEAD
=======

>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d
#dual-intercept piecewise regression_mixed
summary(lm(relsat ~ 1*Husb + preHh + HJump + postHh, data=mainHr)) #남편
summary(lm(relsat ~ 1*Wife + preHw + WJump + postHw, data=mainHr)) #아내
#남편, 아내 동시에
summary(lm(relsat ~ 1*Husb + preHh + HJump + postHh +
<<<<<<< HEAD
                    1*Wife + preHw + WJump + postHw, data=mainHr))
=======
             1*Wife + preHw + WJump + postHw, data=mainHr))
>>>>>>> ef2e5561442dea882ebac7cc1195e37b938dc63d
model1 <- 'relsat ~ 1*Husb + preHh + HJump + postHh
    relsat ~ 1*Wife + preHw + WJump + postHw
    '
summary(sem(model = model1, estimator = "ML", missing = "fiml", data = mainHr, meanstructure=T)) #sem으로 해도 비슷!
