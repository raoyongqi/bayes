library(MachineShop)
if (requireNamespace('xgboost', quietly = TRUE)) {
  library("xgboost")
  
  data(agaricus.train, package = "xgboost")
  
  Folds <- list(
    Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
    , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
    , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3))
  )
  
  scoringFunction <- function(max_depth, min_child_weight, subsample) {
    
    dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)
    
    Pars <- list(
      booster = "gbtree"
      , eta = 0.01
      , max_depth = max_depth
      , min_child_weight = min_child_weight
      , subsample = subsample
      , objective = "binary:logistic"
      , eval_metric = "auc"
    )
    
    xgbcv <- xgb.cv(
      params = Pars
      , data = dtrain
      , nround = 100
      , folds = Folds
      , prediction = TRUE
      , showsd = TRUE
      , early_stopping_rounds = 5
      , maximize = TRUE
      , verbose = 0
    )
    
    return(
      list(
        Score = max(xgbcv$evaluation_log$test_auc_mean)
        , nrounds = xgbcv$best_iteration
      )
    )
  }
  
  bounds <- list(
    max_depth = c(2L, 10L)
    , min_child_weight = c(1, 100)
    , subsample = c(0.25, 1)
  )
  
  ScoreResult <- bayesOpt(
    FUN = scoringFunction
    , bounds = bounds
    , initPoints = 3
    , iters.n = 2
    , iters.k = 1
    , acq = "ei"
    , gsPoints = 10
    , parallel = FALSE
    , verbose = 1
  )
}

