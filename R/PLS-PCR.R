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
#' method: method name as PLS or PCA
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
#' \dontrun{pls.pcr.run(regressionParameterList)}

pls.pcr.run<- function(regressionParameterList){
        cat('pls.pcr.run \n')

        preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        dataSet <- regressionParameterList$dataSet
        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        #modelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                # Create model using PCR or PLS
                if (regressionParameterList$method=="PCR"){
                        modelFit <- pcr(TVC~., data=trainSet)
                }else if (regressionParameterList$method=="PLS"){
                        modelFit <- plsr(TVC ~ . , data=trainSet, scale=TRUE, validation="CV")
                }

                # Using testSet pls or pcr model predicts TVC values
                predictedValues <- predict(modelFit, testSet, ncomp=2)

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                # pls or pcr model model with the performance metrics for the current iteration is appended to the  model list
                # the model list contains all models for all iterations
                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare)
        }

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
