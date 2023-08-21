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
tf <- rowSums(is.na(na_time))==6
sum(tf==TRUE) -> na
na #허리케인 이후 보고가 0인 부부의 수
#각 행에 대한 결측치 수를 계산하고 이것이 6일 경우의 행의 개수를 산출해야 함. 해결!

# 위의 64쌍의 부부를 구분하는 더미변수 생성(개수만 센거라 위 데이터로 가능할지 모르겠다)
hurri %>%
  mutate(NN = replace(NN, rowSums(is.na(na_time)) == 6, 1)) %>%
  mutate(NN = replace(NN, rowSums(is.na(na_time)) != 6, 0))-> hurri_na #success~*^0^*

