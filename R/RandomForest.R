#' Main function to calculate performance of Random forest model
#' @description this function calculates performance of Random forest model
#' through iterations and returns performance metrics.In each iteration different
#' partitioning is done on dataset to create training and validation datasets,
#' tuning is done on training dataset to find optimum mtry-value
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of Iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance results
#' RMSEList: a list which contains RMSE of each iteration
#' cumulativeRMSEList: a list which contains cumulative RMSE mean in
#' each iteration
#' RMSE: mean RMSE of all iterations
#' RSquareList: a list which contains RSquare of each iteration
#' cumulativeRSquareList : a list which contains cumulative RSquare mean in
#' each iteration
#' RSquare: mean RSquare of all iterations
#' bestHyperParamsList: a list containing best mtry value and default number of trees
#' @import randomForest caret foreach
#'
#' @examples
#' \dontrun{randomForest.run(regressionParameterList)}

randomForest.run <- function(regressionParameterList){
        cat('randomForest.run \n')

        dataSet <- regressionParameterList$dataSet

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # pretreatment method is overriden to no_pretreatment in case it has been set by user
        # there is no need for data pretreatment  before randomForest
        regressionParameterList$pretreatment = "no_pretreatment"

        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                trainSet_y<-trainSet[,names(trainSet)=="TVC"]
                trainSet_x<-trainSet[,names(trainSet)!="TVC"]
                testSet <- dataSet[-trainIndexList[,i],]
                testSet_y<-testSet[,names(testSet)=="TVC"]
                testSet_x<-testSet[,names(testSet)!="TVC"]

                # Starting with the default value of mtry, search for the optimal value (with respect to Out-of-Bag error estimate) of mtry for randomForest.
                tuningResult <- tuneRF(trainSet_x, trainSet_y, , ntreeTry=5000, stepFactor=1.1, improve=0.0000001,
                                       trace=TRUE, plot=TRUE, doBest=TRUE)

                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list("mtry"=tuningResult$mtry,"ntree"=tuningResult$ntree)

                # RandomForest model is created with the best hyperparameters for the current iteration
                modelFit <- randomForest(x = trainSet_x, y = trainSet_y, xtest = testSet_x, ytest = testSet_y,
                                         ntree = bestHyperParams$ntree, mtry = bestHyperParams$mtry)

                # Using testSet svm model predicts TVC values
                predictedValues <- modelFit$test$predicted

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare, "bestHyperParams" = bestHyperParams)


        }

        return(createPerformanceStatistics(performanceResults, regressionParameterList ))
}

randomforest.run.alternative <- function(dataAll){
        require(mlr)
        cat('run.rfr.1 \n')

        set.seed(90)
        trainIndex <- createDataPartition(dataAll$TVC, p = defProportion,
                                          list = FALSE, times = 1)
        trainSet <- dataAll[trainIndex,]
        testSet <- dataAll[-trainIndex,]

        #Create task
        rtask <- makeRegrTask(id = "TVC", data = trainSet, target = "TVC")

        #create RF learner
        learner <- makeLearner("regr.randomForest")
        #Tune the model

        mtry_lower <- sqrt(ncol(trainSet) -1)
        mtry_upper <- ncol(trainSet) -1

        forestParamSpace <- makeParamSet(
                makeIntegerParam("ntree", lower = 1000, upper = 1000), # Define hyperparameter space
                makeIntegerParam("mtry", lower = mtry_lower, upper = 12),
                makeIntegerParam("nodesize", lower = 1, upper = 5),
                makeIntegerParam("maxnodes", lower = 5, upper = 20))

        randSearch <- makeTuneControlRandom(maxit = 100)       # Define a random search method with 100 iterations
        #gridSearch <- makeTuneControlGrid()

        cvForTuning <- makeResampleDesc("CV", iters = 5)       # Define a 5-fold cross-validation strategy

        #Tune the hyperparameters
        tunedForestPars <- tuneParams(learner, task = rtask,
                                      resampling = cvForTuning,
                                      par.set = forestParamSpace,
                                      control = randSearch)
        tunedForestPars    # Print tuning results

        # train a final model to make a learner with the tuned hyperparameters
        tunedForest <- setHyperPars(learner, par.vals = tunedForestPars$x)
        tunedForestModel <- mlr::train(tunedForest, rtask)

        predicted <- predict(tunedForestModel, newdata = testSet )

        modelRMSE<-sqrt(mean((predicted$data[,"response"] - predicted$data[,"truth"])^2))
        modelRSquare <- RSquare(predicted$data[,"truth"], predicted$data[,"response"])

        return(list("RMSE" = modelRMSE, "RSquare" = modelRSquare))

}
