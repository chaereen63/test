##let's create time variables##
library(tidyverse)
library(lavaan)
library(psych)

hurri <- read_csv("scales_datafile_rev.csv")
head(hurri)

# 변수별 결측치
hurri %>% 
  select(hrelsat5, hrelsat6, hrelsat7, wrelsat5, wrelsat6, wrelsat7) %>% data.frame() -> na_time
colSums(is.na(na_time)) 
rowSums(is.na(na_time))
tf <- rowSums(is.na(na_time))==6
sum(tf==TRUE) -> na
na #허리케인 이후 보고가 0인 부부의 수
#각 행에 대한 결측치 수를 계산하고 이것이 6일 경우의 행의 개수를 산출해야 함. 해결!



#
