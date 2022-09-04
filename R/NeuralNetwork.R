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

        preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        dataSet <- regressionParameterList$dataSet

        set.seed(90)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                          list = FALSE, times = regressionParameterList$numberOfIterations)

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)


        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                modelFit <- neuralnet(TVC ~ . ,
                               data = as.matrix(trainSet),
                               hidden=10, threshold=0.04, act.fct="tanh", linear.output=TRUE, stepmax=1e7)

                predictedValues <- predict(modelFit, as.matrix(testSet))

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare)

        }

        return(createPerformanceStatistics(performanceResults, regressionParameterList))

}
