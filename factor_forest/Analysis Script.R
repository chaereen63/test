## Actual analysis script

# load packages

library(mlr)
library(psych)
library(ineq)
library(BBmisc)
library(ddpcr)

# source scripts

source("xgb-functions.R")
source("CD_approach.R")
source("factor.forest.R")

# load model

xgb <- readRDS(file = "tunedxgb.rds")

# usage of the trained model


# load data and select colums containing the variables for which EFA should be conducted
# set the argument new data = YOUR EFA DATA


factor.forest(newdata = YOUR_EFA_DATA, mod = xgb)

# default method for missing data when calculating correlation matrices is pairwise deletion 
# you can set the use argument known from the cor function
# use = "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"

