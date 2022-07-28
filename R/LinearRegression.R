#' Main function to calculate performance of linear regression model
#' @description After pretreatment on dataset this function calculates performance
#' of linear regression model through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets.
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' method: method name as SLR or OLSR
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
#' @import caret foreach
#' @export
#'
#' @examples
#' \dontrun{linearRegression.run(regressionParameterList)}

linearRegression.run <- function(regressionParameterList){
        cat('linearRegression.run \n')

        dataSet<-regressionParameterList$dataSet
        preProcValues <- preProcess(dataSet, method = regressionParameterList$pretreatment)
        dataSet <- predict(preProcValues, dataSet)

        set.seed(1821)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        modelList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RMSEList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RSquareList <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # do things in parallel
        modelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                modelFit <- lm(TVC~ . , data=trainSet)

                # If Stepwise Linear Regression selected
                if (regressionParameterList$method == "SLR"){
                        modelFit <- step(modelFit) # perform step-wise feature selection

                }

                predictedValues <- predict(modelFit, testSet)

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
                       method = regressionParameterList$method, platform = regressionParameterList$platform)
        return(result)
}
