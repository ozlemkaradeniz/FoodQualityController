#' Main function to calculate performance of regularized regression model
#' (lasso or ridge regressions)
#' @description After pretreatment on dataset this function calculates performance
#' of regularized regression models (lasso or ridge) through iterations and returns
#' performance metrics.In each iteration different partitioning is done on dataset
#' to create training and validation datasets, cross-validation tuning is done on training
#' dataset to find optimum alpha and lambda values
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
#' @import caret glmnet foreach
#'
#' @examples
#' \dontrun{regularizedRegression.run(regressionParameterList)}
#'
regularizedRegression.run <- function(regressionParameterList){

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


        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]
                dummies <- dummyVars(TVC ~ ., data = dataSet)

                trainDummies = predict(dummies, newdata = trainSet)
                testDummies = predict(dummies, newdata = testSet)

                trainMx = as.matrix(trainDummies)
                trainTarget = trainSet$TVC

                testMx = as.matrix(testDummies)
                testTarget = testSet$TVC

                alpha = -1
                if(regressionParameterList$method == "RR" )
                        alpha = 0
                if(regressionParameterList$method == "LR" )
                        alpha = 1

                set.seed(123)

                cvFit <- cv.glmnet(trainMx, trainTarget, alpha = alpha)

                # Fit the final model on the training data
                modelFit <- glmnet(trainMx, trainTarget, alpha = alpha, lambda = cvFit$lambda.min)

                bestHyperParams <- list(lambda=cvFit$lambda.min)

                # Using testSet the model predicts TVC values
                predictedValues <- predict(modelFit,  newx = testMx)

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare, "bestHyperParams" = bestHyperParams)

        }

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
