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
#' @export
#'
#' @examples
#' \dontrun{neuralNetwork.run(regressionParameterList)}

neuralNetwork.run <- function(regressionParameterList){
        cat(paste0('neuralNetwork.run for platform = ', regressionParameterList$platform, " \n"))

        dataSet<-regressionParameterList$dataSet

        preProcValues <- preProcess(dataSet, method = "range")
        dataSet <- predict(preProcValues, dataSet)

        set.seed(90)
        trainIndex <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                           list = FALSE, times = regressionParameterList$numberOfIterations)
        trainSet <- dataSet[trainIndex,]
        testSet <- dataSet[-trainIndex,]

        n <- neuralnet(TVC ~ . ,
                       data = trainSet,
                       hidden = 5,
                       err.fct = "sse",
                       linear.output = FALSE,
                       lifesign = 'full',
                       rep = 2,
                       algorithm = "rprop+",
                       stepmax = 100000)

        #plot(n, rep = 1)

        predicted <- predict(n, rep = 1, testSet)

        modelRMSE <- RMSE(testSet$TVC, predicted)
        modelRSquare <- RSQUARE(testSet$TVC, predicted)

        cat('Meural Network RMSE: ', modelRMSE, '\n')

        result <- list("RMSE" = modelRMSE, "RSquare" = modelRSquare, method = regressionParameterList$method, platform = regressionParameterList$platform)
        return(result)

}
