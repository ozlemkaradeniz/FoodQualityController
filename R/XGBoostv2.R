run.XGBoost.v2 <- function(method,dataAll){
        require(xgboost)
        require(DiagrammeR)
        require(rBayesianOptimization)
        cat('run.XGBoost \n')

        set.seed(90)
        trainIndex <- createDataPartition(dataAll$TVC, p = 0.7,
                                          list = FALSE, times = 1)

        train = dataAll[trainIndex, ]
        test = dataAll[-trainIndex, ]

        #define predictor and response variables in training set
        train_x = data.matrix(train[, -1])
        train_y = train[,1]

        #define predictor and response variables in testing set
        test_x = data.matrix(test[, -1])
        test_y = test[, 1]

        #define final training and testing sets
        xgb_train = xgb.DMatrix(data = train_x, label = train_y)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y)


        cv_folds <- KFold(train_y, nfolds = 5,
                          stratified = TRUE, seed = 0)
        xgb_cv_bayes <- function(max_depth, min_child_weight, subsample) {
                cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
                                           max_depth = max_depth,
                                           min_child_weight = min_child_weight,
                                           subsample = subsample, colsample_bytree = 0.3,
                                           lambda = 1, alpha = 0,
                                           objective = "reg:squarederror",
                                           eval_metric = "rmse"),
                             data = xgb_train, nround = 100,
                             folds = cv_folds, prediction = TRUE, showsd = TRUE,
                             early_stopping_rounds = 5, maximize = TRUE, verbose = 0,
                             print_every_n=TRUE)
                list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration],
                     Pred = cv$pred)
        }

        OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                        bounds = list(max_depth = c(2L, 6L),
                                                      min_child_weight = c(1L, 10L),
                                                      subsample = c(0.5, 0.8)),
                                        init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                        acq = "ucb", kappa = 2.576, eps = 0.0,
                                        verbose = TRUE)

        #defining a watchlist
        watchlist = list(train=xgb_train, test=xgb_test)

        bestValue<- OPT_Res$Best_Value
        bestNrounds<-min(which(OPT_Res$History$Value == bestValue))

        #define final model
        model_xgboost = xgboost(data = xgb_train, max_depth = OPT_Res$Best_Par["max_depth"],
                                min_child_weight = OPT_Res$Best_Par["min_child_weight"],
                                subsample = OPT_Res$Best_Par["subsample"],
                                nrounds = bestNrounds, verbose = 1)

        summary(model_xgboost)

        #use model to make predictions on test data
        pred_y = predict(model_xgboost, xgb_test)

        result <- evalMetrics(test_y, pred_y)

        result

        return (result)
}
