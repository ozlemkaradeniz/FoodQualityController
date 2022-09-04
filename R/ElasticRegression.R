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
#'
#' @examples
#' \dontrun{elasticRegression.run(regressionParameterList)}

elasticRegression.run <- function(regressionParameterList){
        cat('elasticRegression.run \n')

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

                # Set training control
                # 10 fold cross validation is defined
                trainCont <- trainControl(method = "repeatedcv",
                                          number = 10,
                                          repeats = 1,
                                          search = "random",
                                          verboseIter = TRUE)


                # Train the model
                modelFit <- caret::train(TVC ~ . , data=trainSet,
                                         method = "glmnet",
                                         tuneLength = 10,
                                         trControl = trainCont)

                modelFit <- glmnet(trainMx, trainTarget, alpha = modelFit$bestTune$alpha, lambda = modelFit$bestTune$lambda)

                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list(alpha=modelFit$bestTune$alpha,lambda=modelFit$bestTune$lambda)

                # Using testSet the model predicts TVC values
                predictedValues <- predict(modelFit,  newx = testMx)

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare, "bestHyperParams" = bestHyperParams)

        }

        return (createPerformanceStatistics(performanceResults, regressionParameterList))

}

