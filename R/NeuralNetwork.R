#' Main function to calculate performance of neural network model
#' @description After pretreatment on dataset this function calculates performance
#' of neural network model
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance metrics
#' RMSE: mean RMSE of all iterations
#' RSquare: mean RSquare of all iterations
#' @import caret neuralnet
#'
#' @examples
#' \dontrun{neuralNetwork.run(regressionParameterList)}

neuralNetwork.run <- function(regressionParameterList){
        cat(paste0('neuralNetwork.run for platform 222 = ', regressionParameterList$platform, " \n"))

        preProcValues <- preProcess(regressionParameterList$dataSet, method = regressionParameterList$pretreatment )
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        dataSet <- regressionParameterList$dataSet

        set.seed(90)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                          list = FALSE, times = regressionParameterList$numberOfIterations)

        nnModelList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RMSEList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RSquareList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        regressionParameterList$numberOfIterations<-10

        # do things in parallel
        #nnModelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                modelFit <- neuralnet(TVC ~ . ,
                               data = as.matrix(trainSet))

                predictedValues <- predict(modelFit, as.matrix(testSet))

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                nnModelList[[i]] <- list("model" = modelFit, "RMSE" = RMSE, "RSquare" = RSquare)

        }

        # RMSEList contains list of RMSE for each iteration
        RMSEList <- unlist(lapply(nnModelList, function(x) x$RMSE))
        meanRMSE <- round(mean(RMSEList), 4)
        cumulativeMeanRMSEList <- cumsum(RMSEList) / seq_along(RMSEList)
        names(cumulativeMeanRMSEList) <- seq_along(RMSEList)

        # RSquareList contains list of RSquare for each iteration
        RSquareList <- unlist(lapply(nnModelList, function(x) x$RSquare))
        meanRSquare <- round(mean(RSquareList), 4)
        cumulativeMeanRSquareList <- cumsum(RSquareList) / seq_along(RSquareList)
        names(cumulativeMeanRSquareList) <- seq_along(RSquareList)

        cat('Neural Network mean RMSE: ', meanRMSE, '\n')
        cat('Neural Network  mean RSquare: ', meanRSquare, '\n')

        # Result object is returned to run.regression function in regression.R, which contains whole performance information for the machine learning model
        result <- list("RMSEList"= RMSEList, "cumulativeMeanRMSEList" = cumulativeMeanRMSEList, "RMSE" = meanRMSE,
                       "RSquareList" = RSquareList, "cumulativeMeanRSquareList" = cumulativeMeanRSquareList, "RSquare" = meanRSquare,
                        method = regressionParameterList$method, platform = regressionParameterList$platform)


        return(result)

}
