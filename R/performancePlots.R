generatePerformancePlot <- function(data, plotName, dataNames, file) {
        require(colorRamps)

        # generate different colors for every data entry

        colors <- primary.colors(nrow(data), steps = 3, no.white = TRUE)

        # create pdf file
        pdf(file)

        # prepare plot drawing area
        par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        xAxisPreset <- c(0, ncol(data))
        yAxisPreset <- c(min(data), max(data))
        plot(xAxisPreset, yAxisPreset, xlab = "Iterations",
             ylab = "Score", main = plotName, type = "n")
        # add steps to axis
        #axisStep <- (max(data) - min(data)) / 10
        #axis(side=2, at=seq(min(data), max(data), by = axisStep))

        for(i in 1:nrow(data)) {
                lines(1:ncol(data), data[i,], col = colors[i])
        }

        # generate legend for plot
        legend("topright", inset=c(-0.42,0), dataNames, lty=c(1), col=colors, cex = 0.8)

        # close file
        dev.off()
}


generatePerformancePlots <- function(platformPerformanceResults, outputDir){

        if(file.exists(outputDir) == FALSE)
                dir.create(path = outputDir, showWarnings = FALSE)

        outputDir = paste0(outputDir, "/PerformancePlots")
        dir.create(path = outputDir, showWarnings = FALSE)

        # for each platform different folder is created
        for(platformPerformanceResult in platformPerformanceResults) {

                outputDir = paste0(outputDir, "/", platformPerformanceResult$platform)
                dir.create(path = outputDir, showWarnings = FALSE)

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

                        cat("Creating performance plots for ", platformPerformanceResult$platform, " length ", length(platformPerformanceResult$mlmPerformanceResults) , "\n"  )

                        mergedCumulativeRSquareList[is.na(mergedCumulativeRSquareList)] = 0
                        mergedCumulativeRMSEList[is.na(mergedCumulativeRMSEList)] = 0

                        mlmList <- unlist(lapply(platformPerformanceResult$mlmPerformanceResults, function(x) x$method))

                        generatePerformancePlot(data = mergedCumulativeRMSEList*100, plotName = "RMSE Means",
                                                dataNames = mlmList,
                                                file = paste(outputDir, "/RMSE_Means.pdf", sep = ""))


                        generatePerformancePlot(data = mergedCumulativeRSquareList*100, plotName = "RSquare Means",
                                                dataNames = mlmList,
                                                file = paste(outputDir, "/RSquare_Means.pdf", sep = ""))
                }
        }


}



