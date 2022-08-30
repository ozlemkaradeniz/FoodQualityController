#' Main function to calculate performance of svm model
#' @description This function calculates performance of svm model through iterations
#' and returns performance metrics.In each iteration different partitioning is done
#' on dataset to create training and validation datasets, cross-validation tuning is
#' done on training dataset to find optimum cost,gamma, epsilon values
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of Iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' kernel: svm kernel methods as radial or polynomial
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
#' bestHyperParamsList: a list containing best cost, gamma and epsilon values
#' @import caret foreach e1071
#'
#' @examples
#' \dontrun{svm.run(regressionParameterList)}

svm.run <- function(regressionParameterList){
        cat('svm.run \n')

        preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        dataSet <- regressionParameterList$dataSet

        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # do things in parallel
       # svmModelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                # This generic function tunes hyperparameters of statistical methods using a grid search over supplied parameter ranges.
                # tune function uses tune.control object created with fix sampling
                tuningResult <- e1071::tune(svm, trainSet, trainSet$TVC,
                                            ranges = list(cost = defCostRange, gamma = defGammaRange, epsilon = defEpsilonRange),
                                            tunecontrol = tune.control(sampling = "fix")
                )

                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list("cost" = tuningResult$best.parameters["cost"][1,1],
                                        "gamma" = tuningResult$best.parameters["gamma"][1,1],
                                        "epsilon" = tuningResult$best.parameters["epsilon"][1,1])


                # svm model is created with the best hyperparameters for the current iteration
                modelFit <- svm(trainSet, trainSet$TVC, type="eps-regression",
                                kernel=regressionParameterList$kernel, cost=bestHyperParams$cost, gamma =bestHyperParams$gamma,
                                epsilon =bestHyperParams$epsilon)

                # Using testSet svm model predicts TVC values
                predictedValues <- predict(modelFit , testSet)

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                # svm model with the performance metrics for the current iteration is appended to the svm model list
                # svm model list contains all svm models for all iterations
                performanceResults[[i]] <- list( "RMSE" = RMSE, "RSquare" = RSquare, "bestHyperParams" = bestHyperParams)
        }

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}

