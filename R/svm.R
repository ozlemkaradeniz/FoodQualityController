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
#' @export
#'
#' @examples
#' \dontrun{svm.run(regressionParameterList)}

svm.run <- function(regressionParameterList){
        cat('svm.run \n')

        dataSet<-regressionParameterList$dataSet
        set.seed(1821)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        bestHyperParamsList<-c()
        svmModelList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RMSEList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RSquareList <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # do things in parallel
        svmModelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                tuningResult <- e1071::tune(svm, trainSet, trainSet$TVC,
                                               ranges = list(cost = defCostRange, gamma = defGammaRange, epsilon = defEpsilonRange),
                                               tunecontrol = tune.control(sampling = "fix")
                )

                bestHyperParams <- list("cost" = tuningResult$best.parameters["cost"][1,1],
                                        "gamma" = tuningResult$best.parameters["gamma"][1,1],
                                        "epsilon" = tuningResult$best.parameters["epsilon"][1,1])
                bestHyperParamsList <- c(bestHyperParamsList, bestHyperParams)

                #svm model
                modelFit <- svm(trainSet, trainSet$TVC, type="eps-regression",
                                kernel=regressionParameterList$kernel, cost=bestHyperParams$cost, gamma =bestHyperParams$gamma,
                                epsilon =bestHyperParams$epsilon)

                predictedValues <- predict(modelFit , testSet)

                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                svmModelList[[i]] <- list("model" = modelFit, "RMSE" = RMSE, "RSquare" = RSquare)
        }

        RMSEList <- unlist(lapply(svmModelList, function(x) x$RMSE))
        meanRMSE <- round(mean(RMSEList), 4)
        cumulativeMeanRMSEList <- cumsum(RMSEList) / seq_along(RMSEList)
        names(cumulativeMeanRMSEList) <- seq_along(RMSEList)

        RSquareList <- unlist(lapply(svmModelList, function(x) x$RSquare))
        meanRSquare <- round(mean(RSquareList), 4)
        cumulativeMeanRSquareList <- cumsum(RSquareList) / seq_along(RSquareList)
        names(cumulativeMeanRSquareList) <- seq_along(RMSEList)

        cat('svm mean RMSE: ', meanRMSE, '\n')
        cat('svm mean RSquare: ', meanRSquare, '\n')

        result <- list("RMSEList"= RMSEList, "cumulativeMeanRMSEList" = cumulativeMeanRMSEList, "RMSE" = meanRMSE,
                       "RSquareList" = RSquareList, "cumulativeMeanRSquareList" = cumulativeMeanRSquareList, "RSquare" = meanRSquare,
                       "bestHyperParamsList" = bestHyperParamsList, method = regressionParameterList$method, platform = regressionParameterList$platform)

        return(result)
}
