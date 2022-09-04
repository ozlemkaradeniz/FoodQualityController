#' Cost-complexity post pruning of regression treed
#' @description This method post-prunes the regression tree by cost-complexiy
#' to overcome issue of over-fitting to training set
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param modelTree regression tree model created on training set before pruning
#' @param testSet validation dataset
#' @return a list which contains
#' RMSEList:  a list of RMSE values calculated for each cost-complexity
#' bestPrunedTree: the best pruned tree model found according to RMSE metric
#' RMSE: RMSE value of the best pruned tree
#' RSquare: RSquare value of the best pruned tree
#' bestCp: cost comlexity of the best pruned tree
#' @import rpart
#'
#' @examples
#' \dontrun{pruneTree(modelTree, testSet )}

pruneTree <-function(modelTree, testSet) {
        RMSEListForCpList<-c()
        bestRMSE <- 1000000
        bestRSquare<-0
        cpSequence <- seq(from = 0.01, to = 0.1, by = 0.01)
        bestPrunedTree<-NULL
        bestCp <- 0
        for (cp in cpSequence) {
                prunedTree <- prune(modelTree, cp = cp)
                predicted <- predict(prunedTree, testSet, type="vector")
                modelRMSE <- RMSE(testSet$TVC, predicted)
                modelRSquare <- RSQUARE(testSet$TVC, predicted)
                RMSEListForCpList<-c(RMSEListForCpList, modelRMSE)

                if(modelRMSE<bestRMSE){
                        bestRMSE <- modelRMSE
                        bestPrunedTree <- prunedTree
                        bestRSquare<-modelRSquare
                        bestCp <- cp
                }
        }
        names(RMSEListForCpList) <-  cpSequence
        return(list("RMSEList"= RMSEListForCpList, "RMSE" = bestRMSE, "bestPrunedTree" = bestPrunedTree, "RSquare" = bestRSquare ,
                    "bestCp = ", bestCp ))
}

#' Main function to calculate  performance of Regression tree using bagging
#' @description This function calculates performance of Regression tree through
#' iterations and returns performance metrics.In each iteration different
#' partitioning is done on dataset to create training and validation datasets.
#' The model tree is post-prunes by cost-complexiy to overcome issue of
#' over-fitting to training set in each iteration.
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
#' @import rpart foreach
#'
#' @examples
#' \dontrun{regressionTree.run(regressionParameterList)}
#'
regressionTree.run <- function(regressionParameterList){

        dataSet <- regressionParameterList$dataSet

        # pretreatment method is overriden to no_pretreatment in case it has been set by user
        # there is no need for data pretreatment  before regression tree
        regressionParameterList$pretreatment = "no_pretreatment"

        set.seed(1821)
        trainIndexList <- createDataPartition(dataSet$TVC, p = regressionParameterList$percentageForTrainingSet,
                                              list = FALSE, times = regressionParameterList$numberOfIterations)

        performanceResults <- vector(mode="list", length = regressionParameterList$numberOfIterations)

        for(i in 1:regressionParameterList$numberOfIterations) {
                trainSet <- dataSet[trainIndexList[,i],]
                testSet <- dataSet[-trainIndexList[,i],]

                ##Decision trees
                modelTree <- rpart(trainSet$TVC ~ ., data=trainSet)

                # cost-complexity post-pruning
                pruneResult <- pruneTree(modelTree,testSet)

                RMSE <- pruneResult$RMSE
                RSquare <- pruneResult$RSquare

                performanceResults[[i]] <- list("RMSE" = RMSE, "RSquare" = RSquare)
        }

        return(createPerformanceStatistics(performanceResults, regressionParameterList))
}
