#' run.analysis
#' @description calls each machine learning run methods according to
#' the method parameter in regressionParameterList
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param configParams  list containing parameters provided in config json file
#' by the user
#' @import foreach
#'
#' @examples
#' \dontrun{run.analysis(configParams)}

run.analysis <- function(configParams){

        fileList <- configParams$platformList$dataFileName
        platformList <- configParams$platformList$platformName
        mlmList <- configParams$machineLearningModels

        cat("########################\n#### START ANALYSIS ####\n########################\n\n")

        platformPerformanceResults <- vector(mode="list", length = length(platformList))

       # platformPerformanceResults <- foreach(i=seq(1:length(platformList))) %dopar% {
        for(i in 1:length(platformList)) {
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

                if(configParams$createPCAPlots == TRUE)
                        generatePCAPlots(dataSet, configParams$outputDirectory, platformList[i])

                cat("For ", platformList[i], " best model is ", bestMLM , " with RMSE: " , bestRMSE,  " and R-squared: ", bestRSquare, "\n")
                platformPerformanceResults[[i]] <- list("platform" = platformList[i], "bestMLM" = bestMLM, "bestRMSE" = bestRMSE, "bestRSquare" = bestRSquare,
                                                        "mlmPerformanceResults" = mlmPerformanceResults )

        }

        generateStatistics(platformPerformanceResults, configParams$outputDirectory, configParams$createStatisticsFile)

        if(configParams$createPerformancePlots)
                generatePerformancePlots(platformPerformanceResults, configParams$outputDirectory)


}

#' run.regression
#' @description calls each machine learning run methods according to
#' the method parameter in regressionParameterList
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param regressionParameterList  list containing parameters needed by machine
#' learning models to run
#'
#' @return a list which contains performance results for machine learning
#' model
#' @examples
#' \dontrun{run.regression(regressionParameterList)}

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


#' assess.quality
#' @description assess.quality
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param configFile  configFile
#' @import caret doParallel
#' @return
#' @export
#'
#' @examples
#' \dontrun{assess.quality(configFile)}

assess.quality <- function(configFile=configFile){

        # config file should be provided by the use
        if(is.null(configFile))
           stop("ERROR configuration file should be defined!")

        configParams = readConfigFile(configFile)

        registerDoParallel(cores=4)

        run.analysis(configParams)
}













