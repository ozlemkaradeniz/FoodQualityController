#' generateStatistics
#' @description generates RMSE and RSQUARE statistics report for each platform
#' and machine learning models
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param platformPerformanceResults list of machine learning performance results
#' for each platform
#' @param outputDir output directory name provided by the use in config json file
#' @param createStatisticsFile boolean value indicating whether to create statistics
#' file or not
#' @import gplots
#'
#' @examples
#' \dontrun{generateStatistics(platformPerformanceResults, outputDir, createStatisticsFile)}
#'
generateStatistics <- function(platformPerformanceResults, outputDir, createStatisticsFile){

        mlmLongDesc = list("NN" = "Neural Network", "SVM-Radial" = "SVM-Radial", "SVM-Polynomial" = "SVM-Polynomial",
                           "KNN" = "k-nearest neighbors", "RFR" = "Random Forest",
                           "PLS" = "Partial least squares Regression", "PCR" = "PCA Regression", "OLS"= "Ordinary Least Squares Regression" ,
                           "SR" = "Stepwise Regression", "RR" ="Ridge Regression","LR" = "Lasso Regression", "ER" = "Elastic Regression",
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

        if(nrow(Rmsedf) > 1){
                # Plot best prediction method for each technique and medium according to rmse
                pdf(paste0(outputDir, "/Heatmap_ML_methods.pdf"))
                par(c(5.1,4.1,4.1,2.1))

                heatmap.2(as.matrix(Rmsedf), Rowv=FALSE, key=TRUE,
                          cexCol = 1, cexRow=1,
                          margins=c(8,14),
                          cellnote = as.matrix(Rmsedf), notecol="black", notecex=0.8,
                          col=colorRampPalette(c("green", "red")), breaks = seq(0.3, 1.3, 0.1),
                          main = paste0("ML methods by RMSE"),
                          scale = "none", density.info="none", trace="none", dendrogram="none", Colv="NA",
                )
                dev.off()
        }
}
