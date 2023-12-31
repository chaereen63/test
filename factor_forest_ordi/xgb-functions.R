# This script contains the xgboost implementation that was used to create the prediction model.
# All functions are designed to be used within the mlr framework.

# The early-stopping-xgboost wrapper was written by J. Thomas (2018) and 
# designed for the autoxgboost (see https://github.com/ja-thomas/autoxgboost for the original work)

# makeLearner

makeRLearner.classif.xgboost.earlystop = function() {
  makeRLearnerClassif(
    cl = "classif.xgboost.earlystop",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = NULL, tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = 1, lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "max.nrounds", default = 10^6L, lower = 1L, upper = 10^7L),
      makeUntypedLearnerParam(id = "early.stopping.data"),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeNumericLearnerParam(id = "scale_pos_weight", lower = 0),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeLogicalLearnerParam(id = "one_drop", default = FALSE, requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "tree_method", default = "exact", values = c("exact", "hist"), requires = quote(booster != "gblinear")),
      makeDiscreteLearnerParam(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), requires = quote(tree_method == "hist")),
      makeIntegerLearnerParam(id = "max_leaves", default = 0L, lower = 0L, requires = quote(grow_policy == "lossguide")),
      makeIntegerLearnerParam(id = "max_bin", default = 256L, lower = 2L, requires = quote(tree_method == "hist"))
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob", "weights", "missings"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost.earlystop",
    note = ""
  )
}

# TrainLearner

trainLearner.classif.xgboost.earlystop = function(.learner, .task, .subset, .weights = NULL,
                                                  objective = NULL, eval_metric = NULL,
                                                  early_stopping_rounds = 1, max.nrounds = 10^6, 
                                                  early.stopping.data, scale_pos_weight, nthread, ...) {
  
  td = getTaskDesc(.task)
  nc = length(td$class.levels)
  parlist = list(...)
  
  if (is.null(eval_metric))
    eval_metric = ifelse(nc == 2L, "error", "merror")
  parlist$eval_metric = eval_metric
  
  watchlist = list(eval = createDMatrixFromTask(early.stopping.data))
  
  if (is.null(.weights)) {
    data = createDMatrixFromTask(subsetTask(.task, .subset))
  } else {
    data = createDMatrixFromTask(subsetTask(.task, .subset), weights = .weights)
  }
  
  if (is.null(objective))
    objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")
  
  if (.learner$predict.type == "prob" && objective == "multi:softmax")
    stop("objective = 'multi:softmax' does not work with predict.type = 'prob'")
  
  if (objective %in% c("multi:softprob", "multi:softmax"))
    parlist$num_class = nc
  
  if (!missing(nthread)) {
    mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
                             objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L, nthread = nthread)
  } else {
    mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
                             objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L)
  }
  
  return(mod)
}

# prediction function for xgboost.earlystop

predictLearner.classif.xgboost.earlystop = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  m = .model$learner.model
  cls = td$class.levels
  nc = length(cls)
  obj = .learner$par.vals$objective
  
  if (is.null(obj))
    .learner$par.vals$objective = ifelse(nc == 2L, "binary:logistic", "multi:softprob")
  
  p = predict(m, newdata = data.matrix(convertDataFrameCols(.newdata, ints.as.num = TRUE)), ...)
  
  if (nc == 2L) { #binaryclass
    if (.learner$par.vals$objective == "multi:softprob") {
      y = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
      colnames(y) = cls
    } else {
      y = matrix(0, ncol = 2, nrow = nrow(.newdata))
      colnames(y) = cls
      y[, 1L] = 1 - p
      y[, 2L] = p
    }
    if (.learner$predict.type == "prob") {
      return(y)
    } else {
      p = colnames(y)[max.col(y)]
      names(p) = NULL
      p = factor(p, levels = colnames(y))
      return(p)
    }
  } else { #multiclass
    if (.learner$par.vals$objective  == "multi:softmax") {
      return(factor(p, levels = cls)) #special handling for multi:softmax which directly predicts class levels
    } else {
      p = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
      colnames(p) = cls
      if (.learner$predict.type == "prob") {
        return(p)
      } else {
        ind = max.col(p)
        cns = colnames(p)
        return(factor(cns[ind], levels = cns))
      }
    }
  }
}

# helper function to create "test set" for early stopping rule

createDMatrixFromTask = function(task, weights = NULL) {
  
  data = getTaskData(task, target.extra = TRUE)
  data$data = convertDataFrameCols(data$data, ints.as.num = TRUE)
  if (getTaskType(task) == "classif")  {
    cl = getTaskClassLevels(task)
    data$target =  match(as.character(data$target), cl) - 1
  }
  
  if (!is.null(weights))
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target, weight = weights)
  else if (!is.null(task$weights))
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target, weight = task$weights)
  else
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target)
}
