library(fmsb)
require(foreign)
require(lattice)
require(effects)
require(gplots)
require(MASS)
require(lme4)
require(lmerTest)
require(MuMIn)
require(car)

triad <- read.spss("Study 4 - LTO and Indulgence International.sav", to.data.frame=T)

#Estimating effect size of frewshwater resources from HLMs

summary(tr.glm <- lmer(LTO ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ WaterResources1962Log+ (1|REGION2), data = triad))

#Running model without wateer to estimate effect size of frewshwater resources (change in group-level variance)

summary(tr.glm <- lmer(LTO ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ (1|REGION2), data = triad))

(179.3-164.6)/179.3

#R squared 0.0819855

sqrt((179.3-164.6)/179.3)
#Correlation: .29
#d calculated from t values and DF in models, using this calculator: https://lbecker.uccs.edu/


summary(tr.glm <- lmer(INDULGE ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ WaterResources1962Log+ (1|REGION2), data = triad))

summary(tr.glm <- lmer(INDULGE ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ (1|REGION2), data = triad))

#R squared: .0924
(251.1-227.89)/251.1

#Correlation: .30
sqrt((251.1-227.89)/251.1)




#Estimating effect sizes from simple regressions: Comparing model with and without freshwater

summary(tr.glm <- lm(LTO ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ WaterResources1962Log, data = triad))

summary(tr.glm <- lm(LTO ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020, data = triad))

#R squared

(0.5258-0.4569)
#0.0689

#Correlation

sqrt(0.5258-0.4569)
#.26

#Simple effect size: Comparing model with and without freshwater

summary(tr.glm <- lm(INDULGE ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ WaterResources1962Log, data = triad))

summary(tr.glm <- lm(INDULGE ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020, data = triad))

#R squared

(0.6014-0.5198)
#.0816

#Correlation

sqrt(0.6014-0.5198)
#.29




#Table 5

summary(tr.glm <- lmer(LTO ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Checking for multicollinearity using variance inflation factors at the suggestion of a reviewer. VIF > 10 is considered problematic (Vittinghoff et al., 2006)

vif(tr.glm)

summary(tr.glm <- lmer(INDULGE ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+ WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

vif(tr.glm)

#FTR (separate from other analyses because it is unavialable for some countries)

summary(tr.glm <- lmer(LTO ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog+Equator + Muslim + CropYield + LifeExpentancyWorldBank2020 +FTR + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Checking for multicollinearity using variance inflation factors at the suggestion of a reviewer. VIF > 10 is considered problematic (Vittinghoff et al., 2006)

vif(tr.glm)

summary(tr.glm <- lmer(INDULGE ~ AVETEMP  +RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Equator+Muslim + CropYield + LifeExpentancyWorldBank2020+FTR+ WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

###End Table 5





###Table S2

#War

summary(tr.glm <- lmer(LTO ~ HistoricalWar +AVETEMP + Equator+RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~ HistoricalWar + AVETEMP+ Equator+RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Disease

summary(tr.glm <- lmer(LTO ~ CommunicableDiseaseDALYs2000 + AVETEMP+Equator+ RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~ CommunicableDiseaseDALYs2000 + AVETEMP+Equator+ RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Robustness: Excluding East Asia

triad <- read.spss("~/Documents/Psychology/Research/Iran Dry versus Rainy Regions/Dry vs Rainy Open Data/Study 4 - LTO and Indulgence International.sav", to.data.frame=T)

triad<-subset(triad, triad$REGION2<10)

summary(tr.glm <- lmer(LTO ~ AVETEMP +Equator+  RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~ AVETEMP + Equator+ RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+(1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

triad <- read.spss("~/Documents/Psychology/Research/Iran Dry versus Rainy Regions/Dry vs Rainy Open Data/Study 4 - LTO and Indulgence International.sav", to.data.frame=T)

#Robustness: original Hofstede study

summary(tr.glm <- lmer(LTOOriginal ~AVETEMP + Equator+ RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog +WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGEOriginal ~AVETEMP+ Equator+RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

###End Table S2

###Table S3

#Water per land area (not considering population)

summary(tr.glm <- lmer(LTO ~ AVETEMP+Equator+ RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterPerLandArea2017 + (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~ AVETEMP+ Equator+RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterPerLandArea2017 + (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Precipitation

summary(tr.glm <- lmer(LTO ~ AVETEMP+ Equator+RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Precipitation + (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~ AVETEMP+ Equator+RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + Precipitation + (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

###End Table S2



###Table S5

#Historical GDP

summary(tr.glm <- lmer(LTO ~AVETEMP +Equator+ RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GDPPerCapita1950Log +WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~AVETEMP+Equator+ RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GDPPerCapita1950Log +WaterResources1962Log+  (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Later GNI data

summary(tr.glm <- lmer(LTO ~AVETEMP +Equator+ RPOPDEN +NATDISAS+ LESSCORR + PercentOfArgiculturalLand2015Decimal + GNIPerCapitaLog +WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~AVETEMP+Equator+ RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIPerCapitaLog +WaterResources1962Log+  (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Climatic Variability

summary(tr.glm <- lmer(LTO ~AVETEMP +Equator+ RPOPDEN +NATDISAS+ LESSCORR +PercentOfArgiculturalLand2015Decimal+ GNIperCapita2000CurrentIntDollarLog+ ClimaticVariability + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(INDULGE ~AVETEMP+Equator+ RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog +ClimaticVariability +WaterResources1962Log+  (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

###End Table S5


###Table S8

#Divergent validity: Is water predicting LTO "just" reflecting a need for certainty? Water resources does not significant predict uncertainty avoidance. If anything, the Hofstede dimension is marginally negatively related to water resources. 

summary(tr.glm <- lmer(UncertaintyAvoidanceHofstedeUpdated ~Muslim + CropYield+ AVETEMP  +Equator+RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(UncertaintyAvoidancePracticesHouse ~Muslim + CropYield+ AVETEMP  +Equator+RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + +WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(UncertaintyAvoidanceValuesHouse ~Muslim + CropYield+ AVETEMP  +Equator+RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog +WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

#Water resources predicting LTO controlling for measures of uncertainty avoidance

summary(tr.glm <- lmer(LTO ~Muslim + CropYield+ AVETEMP  +Equator+RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + UncertaintyAvoidanceHofstedeUpdated+WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(LTO ~Muslim + CropYield+ AVETEMP  +Equator+RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + UncertaintyAvoidancePracticesHouse+WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

summary(tr.glm <- lmer(LTO ~Muslim + CropYield+ AVETEMP  +Equator+RPOPDEN +NATDISAS+ LESSCORR+ PercentOfArgiculturalLand2015Decimal + GNIperCapita2000CurrentIntDollarLog + UncertaintyAvoidanceValuesHouse+WaterResources1962Log+ (1|REGION2), data = triad))

r.squaredGLMM(tr.glm)

###End Table S8

