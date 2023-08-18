```{r loading packages and data, message=FALSE, warning=FALSE}

library(lavaan)
library(semPower)
library(semTools)
library(RcmdrMisc)

df <- read.csv("https://osf.io/2xdvg/download", header = TRUE)

sw <- 1 # starting wave
nw <- 9 # final wave


# Descriptive Statistics

numSummary(df[, c(paste0("twi", sw:nw), paste0("tws", sw:nw), paste0("twd", sw:nw),
                  "age_est", "Educat_years", "Gender", "Living_alone", "Smoking",
                  "Apoe4", "n_test", "health_sa")], 
           statistics = c("mean", "sd", "skewness", 
                          "kurtosis", "quantiles"), 
           quantiles = c(.00, 1))

# Preliminary Model

model <- paste0(
  c(
    
    # intercept_Gc
    paste0("i_Gc =~ ", paste0(rep(1, I(nw - sw + 1)), "*", 
                              paste0("Gc", sw:nw), collapse = " + ")),
    # intercept_Gf
    paste0("i_Gf =~ ", paste0(rep(1, I(nw - sw + 1)), "*",
                              paste0("Gf", sw:nw), collapse = " + ")),
    
    # slope_Gc
    paste0("s_Gc =~ ", paste0(c("0", rep("NA", I(nw - sw - 1)), "1"), "*",
                              paste0("Gc", sw:nw), collapse = " + ")),
    # intercept_Gf
    paste0("s_Gf =~ ", paste0(c("0", rep("NA", I(nw - sw - 1)), "1"), "*",
                              paste0("Gf", sw:nw), collapse = " + ")),
    
    
    paste0(c(
      "i_Gc ~~ i_Gf", "s_Gc ~~ s_Gf",
      "i_Gc ~ 1", "i_Gf ~ 1", "s_Gc ~ 1", "s_Gf ~ 1"
    ),
    collapse = "\n"),
    
    
    # residual variances
    
    paste0(paste0("Gc", sw:nw), " ~~ u_Gc*", paste0("Gc", sw:nw), collapse = "\n"), 
    paste0(paste0("Gf", sw:nw), " ~~ u_Gf*", paste0("Gf", sw:nw), collapse = "\n"),
    
    "\n"),
  
  collapse = "\n")



fit <- growth(model, data = df,
              missing = "fiml", estimator = "mlr")

fit_aux <- growth.auxiliary(model, data = df,
                            aux = "health_sa",
                            missing = "fiml", estimator = "mlr")



## Base Model: Results


summary(fit, fit.measures = T, standardized = T, rsquare = F)


## Base Model with Auxiliary Variable: Results

summary(fit_aux, fit.measures = T, standardized = T, rsquare = F)


## Main Model: Results


regression_string <- "Education + Age + Gender + Living_alone + Smoking + Apoe4 + n_test"

model_edu_mimic <- paste0(
  
  c(
    
    # intercepts and slopes
    
    paste0("i_Gc =~ ", paste0(rep(1, I(nw - sw + 1)), "*",
                              paste0("Gc", sw:nw), collapse = " + ")),
    paste0("i_Gf =~ ", paste0(rep(1, I(nw - sw + 1)), "*",
                              paste0("Gf", sw:nw), collapse = " + ")),
    
    paste0("s_Gc =~ ", paste0(c("0", rep("NA", I(nw - sw - 1)), "1"), "*",
                              paste0("Gc", sw:nw), collapse = " + ")),
    paste0("s_Gf =~ ", paste0(c("0", rep("NA", I(nw - sw - 1)), "1"), "*",
                              paste0("Gf", sw:nw), collapse = " + ")),
    
    
    paste0(c(
      "i_Gc ~~ i_Gf", "s_Gc ~~ s_Gf",
      "i_Gc ~ 1", "i_Gf ~ 1", "s_Gc ~ 1", "s_Gf ~ 1"
    ),
    collapse = "\n"),
    
    # residual variances
    
    paste0(paste0("Gc", sw:nw), " ~~ u_Gc*", paste0("Gc", sw:nw), collapse = "\n"), 
    paste0(paste0("Gf", sw:nw), " ~~ u_Gf*", paste0("Gf", sw:nw), collapse = "\n"),
    
    
    # regressions
    
    paste0("i_Gc ~ ", regression_string),
    paste0("i_Gf ~ ", regression_string),
    paste0("s_Gc ~ ", regression_string),
    paste0("s_Gf ~ ", regression_string),
    
    "\n"),
  
  collapse = "\n")


fit_edu_mimic <- growth(model_edu_mimic, data = df,
                        missing = "fiml", estimator = "mlr")

fit_par_edu <- parameterEstimates(fit_edu_mimic, standardized = T)

summary(fit_edu_mimic, fit.measures = T, standardized = T, rsquare = F)


# Raudenbush's Coefficients


i_Gc_rel_mimic <-
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "i_Gc" & fit_par_edu[, "rhs"] == "i_Gc"), "est"]) / 
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "i_Gc" & fit_par_edu[, "rhs"] == "i_Gc"), "est"] +
     ((fit_par_edu[which(fit_par_edu[, "lhs"] == "Gc1" & fit_par_edu[, "rhs"] == "Gc1"), "est"]) / nw))

i_Gf_rel_mimic <-
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "i_Gf" & fit_par_edu[, "rhs"] == "i_Gf"), "est"]) / 
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "i_Gf" & fit_par_edu[, "rhs"] == "i_Gf"), "est"] +
     ((fit_par_edu[which(fit_par_edu[, "lhs"] == "Gf1" & fit_par_edu[, "rhs"] == "Gf1"), "est"]) / nw))

s_Gc_rel_mimic <-
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "s_Gc" & fit_par_edu[, "rhs"] == "s_Gc"), "est"]) / 
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "s_Gc" & fit_par_edu[, "rhs"] == "s_Gc"), "est"] +
     ((fit_par_edu[which(fit_par_edu[, "lhs"] == "Gc1" & fit_par_edu[, "rhs"] == "Gc1"), "est"]) / nw))

s_Gf_rel_mimic <-
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "s_Gf" & fit_par_edu[, "rhs"] == "s_Gf"), "est"]) / 
  (fit_par_edu[which(fit_par_edu[, "lhs"] == "s_Gf" & fit_par_edu[, "rhs"] == "s_Gf"), "est"] +
     ((fit_par_edu[which(fit_par_edu[, "lhs"] == "Gf1" & fit_par_edu[, "rhs"] == "Gf1"), "est"]) / nw))

i_Gc_rel_mimic
i_Gf_rel_mimic
s_Gc_rel_mimic
s_Gf_rel_mimic

# Power Analysis

alpha <- .005

semPower(type = "post-hoc", effect = .05, effect.measure = "RMSEA",
         alpha = alpha, N = nrow(df), df = inspect(fit_edu_mimic, "fit.measures")["df"])


# Correlation and Residual Matrices

lavInspect(fit_edu_mimic, "cor.all")
lavInspect(fit_edu_mimic, "resid")
