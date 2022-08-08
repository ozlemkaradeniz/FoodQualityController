#' generatePerformancePlot
#' @description generates performance plot for each analytical platform
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param data cumulative performance metrics results
#' @param plotName RMSE Means OR rsquare Means
#' @param dataNames machine learning model names
#' @param file output pdf file name
#' @import colorRamps
#'
#' @examples
#' \dontrun{generatePerformancePlot(data, plotName, dataNames, file)}

generatePerformancePlot <- function(data, plotName, dataNames, file) {

        # generate different colors for every data entry

        colors <- primary.colors(nrow(data) + 1, steps = 3, no.white = TRUE)[-1]

        # create pdf file
        pdf(file)

        # prepare plot drawing area
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        xAxisPreset <- c(0, ncol(data))
        yAxisPreset <- c(min(data), max(data))
        plot(xAxisPreset, yAxisPreset, xlab = "Iterations",
             ylab = "Score", main = plotName, type = "n")


        for(i in 1:nrow(data)) {
                x<-data[i,]
                n <- length(x[x!=0])
                lines(1:n, data[i, 1:n], col = colors[i])
        }

        # generate legend for plot
       legend("left", inset=c(1.02 , 1.1 ), dataNames, lty=c(1), col=colors, cex = 0.8)

        # close file
        dev.off()
}


#' generatePerformancePlots
#' @description generates performance plots for each platform. Plots show
#' RMSE and RSquare change in different number of iterations
#' @author Ozlem Karadeniz \email{ozlem.karadeniz.283@@cranfield.ac.uk}
#' @param platformPerformanceResults
#' @param outputDir
#'
#' @examples
#' \dontrun{generatePerformancePlot(data, plotName, dataNames, file)}

generatePerformancePlots <- function(platformPerformanceResults, outputDir){

        if(file.exists(outputDir) == FALSE)
                dir.create(path = outputDir, showWarnings = FALSE)

        outputDir = paste0(outputDir, "/PerformancePlots")
        dir.create(path = outputDir, showWarnings = FALSE)

        # for each platform different folder is created
        for(platformPerformanceResult in platformPerformanceResults) {

                mergedCumulativeRMSEList<-NULL
                mergedCumulativeRSquareList <- NULL

                generatePlot <- FALSE
                for(mlmPerformanceResult in platformPerformanceResult$mlmPerformanceResults){
                        if(is.null(mlmPerformanceResult$cumulativeMeanRMSEList) == FALSE){
                                if(generatePlot == FALSE){
                                        mergedCumulativeRMSEList<- mlmPerformanceResult$cumulativeMeanRMSEList
                                        mergedCumulativeRSquareList <- mlmPerformanceResult$cumulativeMeanRSquareList
                                        generatePlot <- TRUE
                                }
                                else{
                                        mergedCumulativeRMSEList <- dplyr::bind_rows(mergedCumulativeRMSEList, mlmPerformanceResult$cumulativeMeanRMSEList)
                                        mergedCumulativeRSquareList <- dplyr::bind_rows(mergedCumulativeRSquareList, mlmPerformanceResult$cumulativeMeanRSquareList)
                                }
                        }
                }

                if(generatePlot == TRUE){

                        cat("Creating performance plots for ", platformPerformanceResult$platform,"\n"  )

                        mergedCumulativeRSquareList[is.na(mergedCumulativeRSquareList)] = 0
                        mergedCumulativeRMSEList[is.na(mergedCumulativeRMSEList)] = 0

                        mlmList <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults,
                                                 function(x){
                                                         if(!is.null(x$cumulativeMeanRMSEList))
                                                                 x$method
                                                 }))

                        generatePerformancePlot(data = mergedCumulativeRMSEList*100, plotName = "RMSE Means",
                                                dataNames = mlmList,
                                                file = paste(outputDir, "/", platformPerformanceResult$platform, "_RMSE_Means.pdf", sep = ""))


                        generatePerformancePlot(data = mergedCumulativeRSquareList*100, plotName = "RSquare Means",
                                                dataNames = mlmList,
                                                file = paste(outputDir, "/", platformPerformanceResult$platform, "RSquare_Means.pdf", sep = ""))

                }
        }


}



