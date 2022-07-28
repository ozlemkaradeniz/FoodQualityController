library(xlsx)
library(foreach)

run.analysis <- function (fileList, platformList, mlmList,  outputDir, createStatisticsFile,
                          createPerformancePlots, createPCAPlots){

        cat("########################\n#### START ANALYSIS ####\n########################\n\n")

        platformPerformanceResults <- vector(mode="list", length = length(platformList))

        platformPerformanceResults <- foreach(i=seq(1:length(platformList))) %dopar% {
                dataSet = readDataset(fileList[i])
                bestRMSE <- 100000
                bestRSquare<-0
                bestMLM<-""

                mlmPerformanceResults <- vector(mode="list", length = length(mlmList))

                for(j in 1:length(mlmList)) {
                        mlm <-mlmList[j]

                        regressionParameterList <- getRegressionParameters(mlm,dataSet, platformList[i] )
                        dataSet<-regressionParameterList$dataSet
                        regressionParameterList<-within(regressionParameterList, rm(dataSet))
                        print(regressionParameterList)
                        regressionParameterList$dataSet<-dataSet

                        mlmPerformanceResult <- run.regression(regressionParameterList)

                        if(mlmPerformanceResult$RMSE < bestRMSE){
                             bestRSquare <- mlmPerformanceResult$RSquare
                             bestRMSE <- mlmPerformanceResult$RMSE
                             bestMLM <- mlmPerformanceResult$method
                        }
                       # statisticsListForMML <- c(statisticsListForMML, paste0("RMSE: " ,  round(mmlPerformanceResult$RMSE, digit = 3) , "||R-squared: ", round(mmlPerformanceResult$RSquare, digit = 3)))
                        mlmPerformanceResults[[j]]  <- mlmPerformanceResult
                }

                cat("For ", platformList[i], " best model is ", bestMLM , " with RMSE: " , bestRMSE,  " and R-squared: ", bestRSquare, "\n")
                platformPerformanceResults[[i]] <- list("platform" = platformList[i], "bestMLM" = bestMLM, "bestRMSE" = bestRMSE, "bestRSquare" = bestRSquare,
                                                        "mlmPerformanceResults" = mlmPerformanceResults )

        }

        generateStatistics(platformPerformanceResults, outputDir, createStatisticsFile)

        if(createPerformancePlots)
                generatePerformancePlots(platformPerformanceResults, outputDir)

        #generatePCAPlots()

}



run.regression <- function(regressionParameterList){
        method<-regressionParameterList$method
        cat('run.regression is starting \n')
        cat(paste0("mehod name : ", method, "\n"))
        if(method == "SVM-Radial"){
                cat('svm.run is starting \n')
                regressionParameterList$kernel <- "radial"
                result<-svm.run(regressionParameterList)
                regressionParameterList<-within(regressionParameterList, rm(kernel))
        }
        if(method == "SVM-Polynomial"){
                cat('svm.run is starting \n')
                regressionParameterList$kernel <- "polynomial"
                result<-svm.run(regressionParameterList)
                regressionParameterList<-within(regressionParameterList, rm(kernel))

        }
        if(method == "KNN"){
                cat('knn.run is starting \n')
                result<-knn.run(regressionParameterList)
        }
        # if(method == "RFR(MLR)"){
        #         cat('run.rfr.1 is starting \n')
        #         result<-run.rfr.1(dataset)
        # }
        if(method == "RFR"){
                cat('randomForest.run is starting \n')
                result<-randomForest.run(regressionParameterList)
        }
        if(method == "PLSR" || method == "PCR"){
                cat(paste0("plsr.pca.run is starting for ", method ,"\n"))
                result<-plsr.pca.run(regressionParameterList)
        }
        if(method == "RT"){
                cat('regressionTree.run is starting \n')
                result<-regressionTree.run(regressionParameterList)
        }
        if(method == "NN"){
                cat('neuralNetwork.run is starting \n')
                result<-neuralNetwork.run(regressionParameterList)
        }

        # Linear regression models
        # Ordinary Least Squares Regression (OLSR) or Stepwize Linear Regression (SLR)
        if(method == "OLSR" || method == "SLR"){
                cat('linearRegression.run is starting \n')
                result<-linearRegression.run(regressionParameterList)

        }
        #regularized regression models() Ridge Regression, Lasso Regression)
        if(method == "RR" || method == "LR" ){
                cat('regularizedRegression.run is starting \n')
                result<-regularizedRegression.run(regressionParameterList)
        }

        # Elastic Net Regression
        if(method == "ER" ){
                cat('elasticRegression.run is starting \n')
                result<-elasticRegression.run(regressionParameterList)
        }

        # Elastic Net Regression
        if(method == "XGBoost" ){
                cat('XGBoost.run is starting \n')
                result<-XGBoost.run(regressionParameterList)
        }

        return(result)

}
# #!/usr/bin/env Rscript


#' assess.quality
#' @description assess.quality
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param platformList  platformList
#' @param mlmList mlmList
#' @param fileList fileList
#' @param fileTabList fileTabList
#' @param numberOfIterations numberOfIterations
#' @param pretreatment pretreatment
#' @param percentageForTrainingSet percentageForTrainingSet
#' @return
#' @export
#'
#' @examples
#' \dontrun{assess.quality(platformList, mlmList, fileList, numberOfIterations, pretreatment, percentageForTrainingSet
#' )}

assess.quality <- function(platformList, dataFileList, mlmConfigFile=NULL, mlmParams=NULL,
                           outputDir = NULL, createStatisticsFile = TRUE, createPerformancePlots = TRUE, createPCAPlots = TRUE){

        require(caret)

        library(doParallel)

        if(is.null(mlmConfigFile) && is.null(mlmParams))
           stop("mlmConfigFile or mlmParams should be defined!")


        if(!is.null(outputDir) && !file.exists(outputDir))
                stop("Output directory does not exist")

        if(is.null(outputDir))
                outputDir = getwd()


        outputDir = paste0(outputDir, "/BeefQualityAssessment-", Sys.time())

        platformList = strsplit(platformList, ",")[[1]]

        fileList = strsplit(dataFileList, ",")[[1]]

        if(!is.null(mlmParams))
                mlmList = strsplit(mlmParams, ",")[[1]]
        else
                mlmList = readMlmConfigFile(mlmConfigFile)

        run.analysis(fileList, platformList, mlmList, outputDir, createStatisticsFile,
                     createPerformancePlots, createPCAPlots)
}













