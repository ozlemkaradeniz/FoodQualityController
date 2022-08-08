#' generateStatistics
#' @description generates RMSE and RSQUARE statistics report for each platform
#' and machine learning models
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param platformPerformanceResults list of machine learning performance results
#' for each paltofrm
#' @param outputDir output directory name provided by the use in config json file
#' @param createStatisticsFile boolean value indicating whether to create statistics
#' file or not
#'
#' @examples
#' \dontrun{generateStatistics(platformPerformanceResults, outputDir, createStatisticsFile)}
#'
generateStatistics <- function(platformPerformanceResults, outputDir, createStatisticsFile){

        mlmLongDesc = list("NN" = "Neural Network", "SVM-Radial" = "SVM-Radial", "SVM-Polynomial" = "SVM-Polynomial",
                           "KNN" = "k-nearest neighbors", "RFR" = "Random Forest",
                           "PLSR" = "Partial least squares Regression", "PCR" = "PCA Regression", "OLSR"= "Ordinary Least Squares Regression" ,
                           "SLR" = "Stepwise Linear Regression", "RR" ="Ridge Regression","LR" = "Lasso Regression", "ER" = "Elastic Regression",
                           "XGBoost" = "XGBoost","RT"= "Regression Tree"
        )

        cat("########################\n####  ML PERFORMANCE RESULT####\n########################\n\n")

        mlmShortNameList <- unlist(lapply(platformPerformanceResults[[1]]$mlmPerformanceResults, function(x) x$method))
        mlmLongDescList <- unlist(mlmLongDesc[mlmShortNameList])
        platformList <- unlist(lapply(platformPerformanceResults, function(x) x$platform))
        Rmsedf<-data.frame()
        RSquaredf<-data.frame()

        for(platformPerformanceResult in platformPerformanceResults){

                RmseListForMLM <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$RMSE))
                RSquaredListForMLM <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$RSquare))
                if(nrow(Rmsedf) == 0){
                        Rmsedf <- cbind(RmseListForMLM)
                        RSquaredf <- cbind(RSquaredListForMLM)
                }
                else{
                        Rmsedf <- cbind(Rmsedf, RmseListForMLM)
                        RSquaredf <- cbind(RSquaredf, RSquaredListForMLM)
                }

        }

        cat("\n\nRMSE FOR ML METHODS\n\n")
        rownames(Rmsedf) <- mlmLongDescList
        colnames(Rmsedf) <- platformList
        print(Rmsedf)

        cat("\n\nR-squared FOR ML METHODS\n\n")
        rownames(RSquaredf)<- mlmLongDescList
        colnames(RSquaredf) <- platformList
        print(RSquaredf)

        if(createStatisticsFile == TRUE){
                dir.create(path = outputDir, showWarnings = FALSE)
                RMSEFile <- paste0(outputDir, "/RMSE_Statistics.csv")
                write.csv(Rmsedf, file = RMSEFile)
                RSquareFile <- paste0(outputDir, "/RSquare_Statistics.csv")
                write.csv(RSquaredf, file = RSquareFile)
        }
}
