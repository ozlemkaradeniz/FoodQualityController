#' Main function to calculate performance of k-nearest neighbors model
#' @description After pretreatment on dataset this function calculates performance
#' of k-nearest neighbors model through iterations and returns performance metrics.
#' In each iteration different partitioning is done on dataset to create
#' training and validation datasets, cross-validation tuning is done on training
#' dataset to find optimum k-value
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
#' bestHyperParamsList: a list containing best k-value for each iteration
#' @import caret
#'
#' @examples
#' \dontrun{knn.run(regressionParameterList)}

knn.run <- function(regressionParameterList){
        cat('knn.run \n')

        # knn as a distance based algorithm is affected by the scale of the variables.
        # Scaling type is supplied by the user
        preProcValues <- preProcess(regressionParameterList$dataSet, method = regressionParameterList$pretreatment )
        regressionParameterList$dataSet <- predict(preProcValues, regressionParameterList$dataSet)
        dataSet <- regressionParameterList$dataSet

        set.seed(1821)
        # Partition data into training and test set
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        bestHyperParamsList<-c()
        knnModelList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RMSEList <- vector(mode="list", length = regressionParameterList$numberOfIterations)
        RSquareList <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        # do things in parallel
        #knnModelList <- foreach(i=seq(1:regressionParameterList$numberOfIterations), .inorder=FALSE) %dopar% {
        for(i in 1:regressionParameterList$numberOfIterations) {
                # training set and test set are created
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                # Before training resampling method is set as 5 fold cross validation
                trControl <- trainControl(method = "cv", number = 5)

                # model is trained with trainSet using 5 fold cross validation
                # as tuneGrid parameter possible k values is supplied,  train function finds the optimum k-value.
                modelFit <- caret::train(TVC ~ . , method='knn', data=trainSet,
                                         tuneGrid=expand.grid(k=1:maxK), trControl=trControl)

                # list of bestHyperParams is created with best hyperparameters
                bestHyperParams <- list("k"=modelFit$bestTune[1,1])
                # Tuning is called for each iteration seperately as the dataset differs in each iteration
                # List of hyperparameters for each iteration is created
                bestHyperParamsList <- c(bestHyperParamsList, bestHyperParams)

                # Using testSet knn model predicts TVC values
                predictedValues <- predict(modelFit, testSet)

                # Performance metrics (RMSE and RSquare) are calculated by comparing the predicted and actual values
                RMSE<- RMSE(testSet$TVC, predictedValues)
                RSquare <- RSQUARE(testSet$TVC, predictedValues)

                knnModelList[[i]] <- list("model" = modelFit, "RMSE" = RMSE, "RSquare" = RSquare)
        }

        # RMSEList contains list of RMSE for each iteration
        RMSEList <- unlist(lapply(knnModelList, function(x) x$RMSE))
        meanRMSE <- round(mean(RMSEList), 4)
        cumulativeMeanRMSEList <- cumsum(RMSEList) / seq_along(RMSEList)
        names(cumulativeMeanRMSEList) <- seq_along(RMSEList)

        # RSquareList contains list of RSquare for each iteration
        RSquareList <- unlist(lapply(knnModelList, function(x) x$RSquare))
        meanRSquare <- round(mean(RSquareList), 4)
        cumulativeMeanRSquareList <- cumsum(RSquareList) / seq_along(RSquareList)
        names(cumulativeMeanRSquareList) <- seq_along(RSquareList)

        cat('k-NN mean RMSE: ', meanRMSE, '\n')
        cat('k-NN mean RSquare: ', meanRSquare, '\n')

        # Result object is returned to run.regression function in regression.R, which contains whole performance information for the machine learning model
        result <- list("RMSEList"= RMSEList, "cumulativeMeanRMSEList" = cumulativeMeanRMSEList, "RMSE" = meanRMSE,
                       "RSquareList" = RSquareList, "cumulativeMeanRSquareList" = cumulativeMeanRSquareList, "RSquare" = meanRSquare,
                       "bestHyperParamsList" = bestHyperParamsList, method = regressionParameterList$method, platform = regressionParameterList$platform)

        return(result)
}
