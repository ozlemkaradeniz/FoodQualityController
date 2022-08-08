#' Main function to calculate  performance of Partial least squares regression
#' and PCA regression
#' @description This function calculates performance of Partial least squares
#' regression and PCA regression through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets.
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  a list which contains
#' number_of_iterations: number of iterations to calculate performance
#' pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
#' is supported)
#' percentageForTrainingSet: percentage of samples in training dataset
#' method: method name as PLSR or PCA
#' dataSet: dataFrame which is read from data file and subjected to the model.
#' @return a list containing performance results
#' RMSEList: a list which contains RMSE of each iteration
#' cumulativeRMSEList: a list which contains cumulative RMSE mean in
#' each iteration#'
#' RMSE: mean RMSE of all iterations
#' RSquareList: a list which contains RSquare of each iteration
#' cumulativeRSquareList : a list which contains cumulative RSquare mean in
#' each iteration
#' RSquare: mean RSquare of all iterations
#' @import caret pls foreach
#'
#' @examples
#' \dontrun{plsr.pca.run(regressionParameterList)}

plsr.pca.run<- function(regressionParameterList){
        cat('plsr.pca.run \n')

        dataSet<-regressionParameterList$dataSet
        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        modelList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RMSEList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RSquareList <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        modelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                # Create model using PCR or PLSR
                if (regressionParameterList$method=="PCR"){
                        modelFit <- pcr(TVC~., data=trainSet)
                }else if (regressionParameterList$method=="PLSR"){
                        modelFit <- plsr(TVC ~ . , data=trainSet, scale=TRUE, validation="CV")
                }

                # Using testSet plsr or pcr model predicts TVC values
                predictedValues <- predict(modelFit, testSet, ncomp=2)

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                # plsr or pcr model model with the performance metrics for the current iteration is appended to the  model list
                # the model list contains all models for all iterations
                modelList[[i]] <- list("model" = modelFit, "RMSE" = RMSE, "RSquare" = RSquare)
        }

        # RMSEList contains list of RMSE for each iteration
        RMSEList <- unlist(lapply(modelList, function(x) x$RMSE))
        meanRMSE <- round(mean(RMSEList), 4)
        cumulativeMeanRMSEList <- cumsum(RMSEList) / seq_along(RMSEList)
        names(cumulativeMeanRMSEList) <- seq_along(RMSEList)

        # RSquareList contains list of RSquare for each iteration
        RSquareList <- unlist(lapply(modelList, function(x) x$RSquare))
        meanRSquare <- round(mean(RSquareList), 4)
        cumulativeMeanRSquareList <- cumsum(RSquareList) / seq_along(RSquareList)
        names(cumulativeMeanRSquareList) <- seq_along(RMSEList)

        cat(paste0(regressionParameterList$method,' mean RMSE: ', meanRMSE, '\n'))
        cat(paste0(regressionParameterList$method,' mean RSquare: ', meanRSquare, '\n'))

        # Result object is returned to run.regression function in regression.R, which contains whole performance information for the machine learning model
        result <- list("RMSEList"= RMSEList, "cumulativeMeanRMSEList" = cumulativeMeanRMSEList, "RMSE" = meanRMSE,
                       "RSquareList" = RSquareList, "cumulativeMeanRSquareList" = cumulativeMeanRSquareList, "RSquare" = meanRSquare,
                       method = regressionParameterList$method, platform = regressionParameterList$platform)
        return(result)
}
