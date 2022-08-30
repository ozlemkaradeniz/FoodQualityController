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
#' method: method name as SR or OLS
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
#'
#' @examples
#' \dontrun{linearRegression.run(regressionParameterList)}

linearRegression.run <- function(regressionParameterList){
        cat('linearRegression.run \n')

        # In regression, it is often recommended to scale the features to make it easier to interpret the intercept term.
        # Scaling type is supplied by the user
        preProcValues <- preProcess(regressionParameterList$dataSet, method = gePretreatmentVector(regressionParameterList$pretreatment))
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        dataSet <- regressionParameterList$dataSet

        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # do things in parallel
        #modelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                modelFit <- lm(TVC~ . , data=trainSet)

                # If Stepwise Regression selected
                if (regressionParameterList$method == "SR"){

                        direction <- regressionParameterList$direction
                        cat("stepwise regression direction :", direction, "\n")
                        modelFit <- step(modelFit, direction = c(direction) ) # perform stepwise feature selection
                }

                # Using testSet the model predicts TVC values
                predictedValues <- predict(modelFit, testSet)

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare)
        }

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}


