library(psych);library(ggplot2)
library(tidyverse);library(tidyr)
data1 <- read.table('risktaking.txt', header = T)

plot(data1$EgoStrength,data1$RiskTaking)
#edit(data1)
  #Q1.a
scatter.smooth(data1$EgoStrength,data1$RiskTaking,xlab="x",ylab="y",span=2/3,degree=1,
               main="span=2/3,degree=1")
scatter.smooth(data1$EgoStrength,data1$RiskTaking,xlab="x",ylab="y", span=2/3,degree=2,
               main="span=2/3,degree=2")
scatter.smooth(data1$EgoStrength,data1$RiskTaking,xlab="x",ylab="y",span=1/3,degree=1,
               main="span=1/3,degree=1")
scatter.smooth(data1$EgoStrength,data1$RiskTaking,xlab="x",ylab="y", span=1/3,degree=2,
               main="span=1/3,degree=2")
  #Q1.b
#log
 data1 %>% mutate(lnEgo = log(data2$EgoStrength), sqrtEgo = sqrt(EgoStrength)) -> trans_data1
  #Q1.c
logM <- lm(RiskTaking ~ lnEgo, data = trans_data1) #transformed variable1
summary(logM)
SqM <- lm(RiskTaking ~ sqrtEgo, data = trans_data1)#2
summary(SqM)
  #Q1.d
# 로그 함수 정의
log_function <- function(x) {
  return(4.874*log(x) + 1.382)
} #lm으로 구한 기울기, 절편(회귀식_log model)
sqrt_function <- function(x) {
  return(4.993*sqrt(x)-2.424)
} #lm으로 구한 기울기, 절편(회귀식_sqrt model)

# ggplot2를 사용하여 산점도와 로그 함수를 그리기
plot.for.d <- ggplot(trans_data1, aes(x = EgoStrength, y = RiskTaking)) +
              geom_point() +  # 산점도
              stat_function(fun = log_function, color = "purple") +  # 로그 함수를 추가
              stat_function(fun = sqrt_function, color = "violet")+
              labs(title = "Scatterplot with log&sqrt Function", x = "EgoStrength", y = "RiskTaking") +
              geom_text(aes(x = 3, y = 11, label = "y_hat=4.874*log(x) + 1.382, R^=.939"), color = "purple", size=3) +  # 로그 함수의 이름 추가
              geom_text(aes(x = 8, y = 8, label = "y_hat=4.993*sqrt(x)-2.424, R^=.937"), color = "violet", size=3)  # 제곱근 함수의 이름 추가
plot.for.d
  #Q1.e.
#create quadratic nd cubic terms of the EgoStrength variable and centered EgoStrength
trans_data1 %>% mutate(c_ES = scale(EgoStrength, scale = F)) %>% #centering
  mutate(quadES = (EgoStrength)^2, cubicES = (EgoStrength)^3,    #quad and cubic
         cen_quadES = (c_ES)^2, cen_cubicES = (c_ES)^3) %>% 
  as.data.frame() -> transdata2
#transdata2
#correlation_RiskTaking, EgoStrength, Quadratic, cubic
transdata2 %>% 
  select("RiskTaking", "EgoStrength", "quadES","cubicES") %>% cor() -> cor1
cor1
#correlation_centered EgoStrength, its quad, cubic
transdata2 %>% 
  select("RiskTaking","c_ES", "cen_quadES","cen_cubicES") %>% cor() -> cor2
cor2
  #Q1.f
cubic.regression <- lm(RiskTaking ~ EgoStrength + quadES + cubicES, data=transdata2)
summary(cubic.regression) #original
cen.cubic.regression <- lm(RiskTaking ~ c_ES + cen_quadES + cen_cubicES, data=transdata2)
summary(cen.cubic.regression)
  #Q1.g
#hierarchical analysis_non centering
model1 <- lm(RiskTaking ~ EgoStrength, data=transdata2)
model2 <- lm(RiskTaking ~ EgoStrength + quadES, data=transdata2)
model3 <- lm(RiskTaking ~ EgoStrength + quadES + cubicES, data=transdata2)
anova(model1, model2) #accept model2
anova(model2, model3) #deny model3
  #Q1.h
plot(transdata2$quadES,transdata2$RiskTaking)
summary(model2);summary(logM);summary(SqM)
#the model which followed hierarchical analysis is better, 
#because it tells not only the nonlinear trend of data, 
#but also added term model explained outcome more.
#in addition, when compared with adjusted R-squared of models, model2(g.model) is bigger then others

  #Q2
Q2function <- function(D1, D2, D3) {
  return(5 + 6*D1 + 3*D2 - 4*D3)
}
(Q2function(1,0,0) -> group1)
(Q2function(0,1,0) -> group2)
(Q2function(0,0,1) -> group3)
(Q2function(0,0,0) -> group4)

  #Q3: see Excel file
  
  #Q4~Q6: omit missing
data4 <- read.table("data1.txt", header = T, na.strings = ".")
#edit(data4)
dataQ <- na.omit(data4[c("SEX","PHYSS","DATEDUR","SERIOUS")])
sum(is.na(dataQ)==T);summary(dataQ) #check missing
dataQ %>% mutate(SEX = ifelse(dataQ$SEX==2,0,1)) -> dataQ #female=1, male=0
dataT <- data.frame(SEX=as.factor(dataQ$SEX),
                    DATEDUR=as.factor(dataQ$DATEDUR),
                    SERIOUS=as.factor(dataQ$SERIOUS),
                    PHYSS=as.numeric(dataQ$PHYSS))
summary(dataT)
#Q4
bi.model <- lm(PHYSS ~ SEX, data = dataT)
summary(bi.model) #intercept is 1.200*** and slope is -.056(n.s.)
                  #F-test p-value is .2386
tt <- t.test(PHYSS ~ SEX, data = dataT, var.equal = T)
summary(bi.model);tt 
#if assume equal variance between two groups,
#p-value of t-test and F-test is equal to .2386
#intercept of the linear model is equal to mean of group 0(=male); 1.200267
tt$estimate[2]-tt$estimate[1];bi.model$coefficients[2]
#slope of the linear model is equal to difference between two groups (female-male); -.056316

  #Q5
categoric.model <- lm(PHYSS~DATEDUR, data = dataT)
summary(categoric.model)
#do this using dummy variables (comparison group = 1)
dataQ %>% mutate(d1=ifelse(DATEDUR==2,1,0), d2=ifelse(DATEDUR==3,1,0),
                 d3=ifelse(DATEDUR==4,1,0), d4=ifelse(DATEDUR==5,1,0)) -> dum.dataQ
dummy.model <- lm(PHYSS ~ d1 + d2 + d3 + d4, data = dum.dataQ)
summary(dummy.model) #same with categoric.model
#intercept is the mean of group1, each slopes is the differences of groups2,3,4,5 compared with group1
#F-test tell there are no difference of PHYSS of datedur compared with date duration is 1-3 month.
#아노바의 옴니버스 테스트 집단1=집단2=집단3 <=> 모형의 설명력은 0이다

  #Q6
interaction.model <- lm(PHYSS~SERIOUS + SEX + SERIOUS*SEX, data = dataT)
summary(interaction.model)
  #Q6.a
#do this using dummy variables (comparison group = 1)
dataQ %>% mutate(d1=ifelse(SERIOUS==2,1,0), d2=ifelse(SERIOUS==3,1,0),
                 d3=ifelse(SERIOUS==4,1,0), d4=ifelse(SERIOUS==5,1,0)) -> dum.data6
dummy.model6 <- lm(PHYSS ~ SEX +  d1 + d2 + d3 + d4 + (d1 + d2 + d3 + d4)*SEX, data = dum.data6)
summary(dummy.model6) #same with interaction.model
  #Q6.b
dummy.modelmain <- lm(PHYSS ~ SEX +  d1 + d2 + d3 + d4, data = dum.data6)
summary(dummy.modelmain)
dummy.modelmain2 <- lm(PHYSS ~ SEX, data = dum.data6)
summary(dummy.modelmain2)
anova(dummy.model6, dummy.modelmain) #effect of interaction sig.
anova(dummy.modelmain2, dummy.modelmain) #main effect of seriousness) n.s.
anova(dummy.modelmain, dummy.model) #main effect of sex n.s.
# Load the emmeans package
#install.packages("emmeans")
library(emmeans)
# Perform tests of simple effects for SEX within each level of d1, d2, d3, d4

simple_effect <- lsmeans(interaction.model, pairwise ~ SEX | SERIOUS, p.adjust.methods= "bonferroni")
summary(simple_effect)

.05 / 5 #Bonferroni

#유의확률 .05/5
# Print results
print(simple_effects_d1)
print(simple_effects_d2) 
print(simple_effects_d3) 
print(simple_effects_d4)
#anova
as <- aov(PHYSS ~ SERIOUS*SEX, data = dataT)
anova(as)
TukeyHSD(as)
