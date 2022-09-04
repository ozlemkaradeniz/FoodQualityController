#' run.analysis
#' @description calls each machine learning run methods according to
#' the method parameter in regressionParameterList
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param configParams  list containing parameters provided in config json file
#' by the user
#' @import foreach R.utils
#'
#' @examples
#' \dontrun{run.analysis(configParams)}

run.analysis <- function(configParams){
        fileList <- configParams$platformList$dataFileName
        platformList <- configParams$platformList$platformName
        mlmList <- configParams$machineLearningModels

        cat("########################\n#### START ANALYSIS ####\n########################\n\n")

        # initialization of platformPerformanceResults
        platformPerformanceResults <- vector(mode="list", length = length(platformList))

       # platformPerformanceResults <- foreach(i=seq(1:length(platformList))) %dopar% {
        for(i in 1:length(platformList)) {
                dataSet = readDataset(fileList[i])
                bestRMSE <- 100000
                bestRSquare<-0
                bestMLM<-""

                if(configParams$createPCAPlots == TRUE)
                        generatePCAPlots(dataSet, configParams$outputDirectory, platformList[i])

                mlmPerformanceResults <- vector(mode="list", length = length(mlmList))

                # for each platforms and machine learning models following code is executed
                for(j in 1:length(mlmList)) {
                        mlm <-mlmList[j]

                        # RegressionParameterList is creates as list object which carries necessary parameters for machine learning
                        # models to run.
                        # elements of RegressionParameterList are set to default values if they are not supplied by the user.
                        # default values are defined in utils.R
                        regressionParameterList <- getRegressionParameters(mlm,dataSet, platformList[i] )
                        dataSet<-regressionParameterList$dataSet

                        # according to method parameter of regressionParameterList different machine learning model is executed in  run.regression
                        # mlmPerformanceResult <- tryCatch(
                        #                 {
                        #                         #withTimeout(run.regression(regressionParameterList), timeout = 10000, onTimeout = "silent")
                        #                         run.regression(regressionParameterList)
                        #                 },
                        #                 error=function(cond) {
                        #                         message(cond)
                        #                         print(sys.calls())
                        #                         return(NULL)
                        #                 }
                        #         )


                        mlmPerformanceResult <- run.regression(regressionParameterList)
                        if(is.null(mlmPerformanceResult)){
                                cat("For ", platformList[i], " Timeout Exception in ", regressionParameterList$method , "(",
                                    regressionParameterList$pretreatment, ")\n")

                                mlmPerformanceResult <- list("RMSE" = NA,"RSquare" = NA, method = regressionParameterList$method,
                                                             pretreatment = regressionParameterList$pretreatment, platform = regressionParameterList$platform)
                                mlmPerformanceResults[[j]]  <- mlmPerformanceResult
                                next

                        }

                        # Through the loop best machine learning model is found according to RMSE performance metric
                        if(!is.null(mlmPerformanceResult) && mlmPerformanceResult$RMSE < bestRMSE){
                             bestRSquare <- mlmPerformanceResult$RSquare
                             bestRMSE <- mlmPerformanceResult$RMSE
                             bestMLM <- mlmPerformanceResult$method
                        }
                        # machine learning model list updated
                        # each platform has got different mlmPerformanceResults list
                        mlmPerformanceResults[[j]]  <- mlmPerformanceResult
                }


                # Best machine learning model for the platform is printed
                cat("For ", platformList[i], " best model is ", bestMLM , " with RMSE: " , bestRMSE,  " and R-squared: ", bestRSquare, "\n")

                # each item in platformPerformanceResults corresponds to one platform,
                # Associated mlmPerformanceResults(performance results of machine learning model list), bestMLM, bestRMSE, bestRSquare which have been
                # created with the previous loop are appended to platformPerformanceResults list.
                platformPerformanceResults[[i]] <- list("platform" = platformList[i], "bestMLM" = bestMLM, "bestRMSE" = bestRMSE, "bestRSquare" = bestRSquare,
                                                        "mlmPerformanceResults" = mlmPerformanceResults )

        }

        # RSquare_Statistics.csv and RMSE_Statistics.csv files are created if createStatisticsFile parameter is set as TRUE in config file
        generateStatistics(platformPerformanceResults, configParams$outputDirectory, configParams$createStatisticsFile)

        # Performance plots which shows RSquare and  RMSE means through number of iterations are created for each platform
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
        result<-NULL
        if(method == "SVR-Radial"){
                cat('svr.run is starting \n')
                regressionParameterList$kernel <- "radial"
                result<-svr.run(regressionParameterList)
                regressionParameterList<-within(regressionParameterList, rm(kernel))
        }
        if(method == "SVR-Polynomial"){
                cat('svr.run is starting \n')
                regressionParameterList$kernel <- "polynomial"
                result<-svr.run(regressionParameterList)
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
        if(method == "PLS" || method == "PCR"){
                cat(paste0("pls.pcr.run is starting for ", method ,"\n"))
                result<-pls.pcr.run(regressionParameterList)
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
        # Ordinary Least Squares Regression (OLS) or Stepwise Regression (SR)
        if(method == "OLS" || method == "SR"){
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

        # config file which contains user-defined parameters for the application is parsed.
        # After parsing, configParams object is created as a list of following elements
        # platform list, machine learning models and their parameters, output directory, createStatisticsFile,
        # createPerformancePlots, createPCAPlots

        configParams = readConfigFile(configFile)

        # in run.analysis foreach method is called as parallel
        #registerDoParallel(cores=4)

        run.analysis(configParams)
}













