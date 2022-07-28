#' Main function to calculate performance of k-nearest neighbors model
#' @description After pretreatment on dataset this function calculates performance
#' of k-nearest neighbors model through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets, cross-validation tuning is done on training
#' dataset to find optimum k-value
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
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
#' bestHyperParamsList: a list containing best k-value for each iteration
#' @import caret
#' @export
#'
#' @examples
#' \dontrun{knn.run(regressionParameterList)}

knn.run <- function(regressionParameterList){
        cat('knn.run \n')

        dataSet <- regressionParameterList$dataSet
        preProcValues <- preProcess(regressionParameterList$dataSet, method = regressionParameterList$pretreatment )
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)

        set.seed(1821)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        bestHyperParamsList<-c()
        knnModelList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RMSEList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RSquareList <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # do things in parallel
        knnModelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                trControl <- trainControl(method = "cv", number = 5)

                modelFit <- caret::train(TVC ~ . , method='knn', data=trainSet,
                                         tuneGrid=expand.grid(k=1:maxK), trControl=trControl)

                bestHyperParams <- list("k"=modelFit$bestTune[1,1])
                bestHyperParamsList <- c(bestHyperParamsList, bestHyperParams)

                predictedValues <- predict(modelFit, testSet)
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                knnModelList[[i]] <- list("model" = modelFit, "RMSE" = RMSE, "RSquare" = RSquare)
        }

        RMSEList <- unlist(lapply(knnModelList, function(x) x$RMSE))
        meanRMSE <- round(mean(RMSEList), 4)
        cumulativeMeanRMSEList <- cumsum(RMSEList) / seq_along(RMSEList)
        names(cumulativeMeanRMSEList) <- seq_along(RMSEList)

        RSquareList <- unlist(lapply(knnModelList, function(x) x$RSquare))
        meanRSquare <- round(mean(RSquareList), 4)
        cumulativeMeanRSquareList <- cumsum(RSquareList) / seq_along(RSquareList)
        names(cumulativeMeanRSquareList) <- seq_along(RMSEList)

        cat('k-NN mean RMSE: ', meanRMSE, '\n')
        cat('k-NN mean RSquare: ', meanRSquare, '\n')
        result <- list("RMSEList"= RMSEList, "cumulativeMeanRMSEList" = cumulativeMeanRMSEList, "RMSE" = meanRMSE,
                       "RSquareList" = RSquareList, "cumulativeMeanRSquareList" = cumulativeMeanRSquareList, "RSquare" = meanRSquare,
                       "bestHyperParamsList" = bestHyperParamsList, method = regressionParameterList$method, platform = regressionParameterList$platform)
}
