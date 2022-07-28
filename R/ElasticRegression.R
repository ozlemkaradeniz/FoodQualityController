#' Main function to calculate performance of elastic regression model
#' @description After pretreatment on dataset this function calculates performance
#' of elastic regression model through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets, cross-validation tuning is done on training
#' dataset to find optimum lambda value
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
#' bestHyperParamsList: a list containing best alpha and lambda values for each iteration
#' @import caret foreach
#' @export
#'
#' @examples
#' \dontrun{elasticRegression.run(regressionParameterList)}

elasticRegression.run <- function(regressionParameterList){
        cat('elasticRegression.run \n')

        dataSet<-regressionParameterList$dataSet
        preProcValues <- preProcess(dataSet, method = regressionParameterList$pretreatment)
        dataSet <- predict(preProcValues, dataSet)

        set.seed(1821)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        bestHyperParamsList<-c()
        modelList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RMSEList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RSquareList <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # do things in parallel
        modelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                dummies <- dummyVars(TVC ~ ., data = dataSet)
                trainDummies = predict(dummies, newdata = trainSet)
                testDummies = predict(dummies, newdata = testSet)

                trainMx = as.matrix(trainDummies)
                trainTarget = trainSet$TVC

                testMx = as.matrix(testDummies)
                testTarget = testSet$TVC

                # Set training control
                trainCont <- trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 1,
                                          search = "random",
                                          verboseIter = TRUE)

                # Train the model
                modelFit <- caret::train(TVC ~ .,
                                         data = trainSet,
                                         method = "glmnet",
                                         tuneLength = 10,
                                         trControl = trainCont)

                bestHyperParams <- list(alpha=modelFit$bestTune$alpha,lambda=modelFit$bestTune$lambda)
                bestHyperParamsList <- c(bestHyperParamsList, bestHyperParams)

                predictedValues <- predict(modelFit,  newx = testMx)

                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                modelList[[i]] <- list("model" = modelFit, "RMSE" = RMSE, "RSquare" = RSquare)

        }

        RMSEList <- unlist(lapply(modelList, function(x) x$RMSE))
        meanRMSE <- round(mean(RMSEList), 4)
        cumulativeMeanRMSEList <- cumsum(RMSEList) / seq_along(RMSEList)
        names(cumulativeMeanRMSEList) <- seq_along(RMSEList)

        RSquareList <- unlist(lapply(modelList, function(x) x$RSquare))
        meanRSquare <- round(mean(RSquareList), 4)
        cumulativeMeanRSquareList <- cumsum(RSquareList) / seq_along(RSquareList)
        names(cumulativeMeanRSquareList) <- seq_along(RMSEList)

        cat(paste0(regressionParameterList$method,' mean RMSE: ', meanRMSE, '\n'))
        cat(paste0(regressionParameterList$method,' mean RSquare: ', meanRSquare, '\n'))

        result <- list("RMSEList"= RMSEList, "cumulativeMeanRMSEList" = cumulativeMeanRMSEList, "RMSE" = meanRMSE,
                       "RSquareList" = RSquareList, "cumulativeMeanRSquareList" = cumulativeMeanRSquareList, "RSquare" = meanRSquare,
                       "bestHyperParamsList" = bestHyperParamsList, method = regressionParameterList$method, platform = regressionParameterList$platform)
        return(result)
}
