# default machine learning parametedef.proportion<-0.7
defCostRange<-c(seq(1, 20, 3), seq(30, 110, 20))
defGammaRange<-c(seq(0, 0.06, 0.02), seq(0.1, 0.3, 0.1), 0.6)
defEpsilonRange<-c(seq(0,0.1,0.03), 0.2, seq(0.3,0.9,0.3))
maxK <-20
defNtree<-1000
defNumberOfIterations <- 80
defPercentageForTrainingSet <- 0.7


RMSE <- function(true, predicted){
        RMSE <- sqrt(mean((predicted - true)^2))
        return(RMSE)
}

RSQUARE <- function(true, predicted){
        RSquare <- 1 - sum((predicted - true)^2) / sum((true - mean(true))^2)
        return(RSquare)
}

evalMetrics <- function(true, predicted) {
        RSquare <- RSQUARE(true, predicted)
        RMSE = RMSE(true, predicted)
        return(list("RMSE" = RMSE, "RSquare" = RSquare))

}

gePretreatmentVector <- function(pretreatment){
        if (pretreatment == "no_pretreatment"){
                return(pretreatment)
        }
        if(pretreatment == "auto-scale"){
                return(c("center", "scale"))
        }
        if(pretreatment == "range-scale"){
                return(c("range"))
        }
        if(pretreatment == "mean-center"){
                return(c("center"))
        }
}

getRegressionParameters <- function(mlm, dataSet, platform){
        mlmParams <- strsplit(mlm, ":")[[1]]
        if(mlmParams[2] == "" || is.na(mlmParams[2]))
                mlmParams[2] <- "no_pretreatment"
        if(mlmParams[3] == "" || is.na(mlmParams[3]))
                mlmParams[3] <- defNumberOfIterations
        if(mlmParams[4] == ""  || is.na(mlmParams[4]))
                mlmParams[4] <- defPercentageForTrainingSet

        regressionParameterList <- list("method" = mlmParams[1], "pretreatment" =  gePretreatmentVector(mlmParams[2]), "numberOfIterations" = as.numeric(mlmParams[3]),
                                        "percentageForTrainingSet" = as.numeric(mlmParams[4]), dataSet = dataSet, "platform" = platform)

        return(regressionParameterList)
}

plotPrediction <- function(model, testSet) {
        predicted <- predict(model, testSet)
        RMSE <- RMSE(testSet$TVC, predicted)
        plot(predicted, testSet$TVC, xlab='Predicted log10 TVC',
             ylab='Actual log10 TVC', main=paste('RMSE:', RMSE))
        return(predicted)
}

readMlmConfigFile<-function(mlmConfigFile){
        require(configr)

        mlmConfig <-read.config(file = mlmConfigFile)
        mlmConfig<-mlmConfig$MachineLearningModels
        mlmList = c()

        for(i in 1:length(mlmConfig$shortName)){
                if(is.na(mlmConfig$pretreatment[i]))
                        mlmConfig$pretreatment[i] <- "no_pretreatment"
                if(is.na(mlmConfig$numberOfIterations[i]))
                        mlmConfig$numberOfIterations[i] <- defNumberOfIterations
                if(is.na(mlmConfig$proportionOfTrainingSet[i]))
                        mlmConfig$proportionOfTrainingSet[i] <- defPercentageForTrainingSet

                mlmList <- c(mlmList, paste0(mlmConfig$shortName[i], ":" , mlmConfig$pretreatment[i], ":",
                                             mlmConfig$numberOfIterations[i], ":" ,mlmConfig$proportionOfTrainingSet[i], ":"))

        }

        return(mlmList)
}

readDataset<-function(dataFileName){
        library(openxlsx)
        library(caret)
        library(tools)
        # Read dataset
        #data.file <- "/Users/ozlemkaradeniz/Cranfield/Thesis/data/MSI_beef_air_spoilage_GM_OK.xlsx"
        #data.tab.name <- "Φύλλο1"
        #dataset <- read.xlsx(date.file, 1, "Φύλλο1", header=TRUE, row.names=1)

        dataFileName <- paste0("/Users/ozlemkaradeniz/Cranfield/Thesis/data/", dataFileName, sep="")

        fileExtension <- tolower(file_ext(dataFileName))

        dataSet <- data.frame()
        if(fileExtension == "xlsx")
                dataSet <- openxlsx::read.xlsx(dataFileName, sheet = 1, startRow = 1, rowNames=TRUE)
        if(fileExtension == "csv")
                dataSet<-read.table(dataFileName, sep=",", header=TRUE, row.names=1)

        emptyColumns <- colSums(is.na(dataSet) | dataSet == "") == nrow(dataSet)
        dataSet<-dataSet[, !emptyColumns]

        dataSet <- na.omit(dataSet)

        colnames(dataSet)<-gsub("\\/", "\\.", colnames(dataSet))

        if(any(!is.na(as.numeric(colnames(dataSet)))))
                colnames(dataSet)[colnames(dataSet)!="TVC"] <- paste0("a", colnames(dataSet),"")

         if(ncol(dataSet) > 200){
                features<- selectFeatures(dataSet)
                dataSet<-dataSet[,c(features,"TVC")]
         }
        #dataSet<-removeRedundantFeatures(dataSet)


        # Return dataset

        return(dataSet)
}

plot.pca <- function(dataAll){
        library(mixOmics)
        #PCA
        pca.dataAll <- pca(dataAll[,-ncol(dataAll)], ncomp=4, scale=TRUE)
        print(pca.dataAll)
        plotIndiv(pca.dataAll, ind.names=as.character(dataAll[,ncol(dataAll)]), group=as.factor(dataAll[,ncol(dataAll)]), style="lattice")

        Var1<-100*(pca.dataAll$prop_expl_var$X[1])
        Var1<-round(Var1, digits=2)
        Var2<-100*(pca.dataAll$prop_expl_var$X[2])
        Var2<-round(Var2, digits=2)
        biplot(pca.dataAll, xlab=paste("PC1 ", Var1, "%"), ylab=paste("PC2 ", Var2, "%"))
}

selectFeatures<-function(dataSet){
        library(Boruta)

        # Perform Boruta search
        boruta_output <- Boruta(TVC ~ ., data=na.omit(dataSet), doTrace=0)

        names(boruta_output)

        boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
        print(boruta_signif)

        return(boruta_signif)
}


removeRedundantFeatures<-function(dataSet){
        library(caret)
        library(corrplot)
        library(plyr)

        nzv <- nearZeroVar(dataSet, saveMetrics= TRUE)
        dataSet <- dataSet[, !nzv$nzv]

        # Calculate correlation matrix
        descrCor <- cor(dataSet)

        # Print correlation matrix and look at max correlation
        #print(descrCor)
        #summary(descrCor[upper.tri(descrCor)])

        # Check Correlation Plot
        #corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

        # find attributes that are highly corrected
        highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

        # print indexes of highly correlated attributes
        #print(highlyCorrelated)

        # Indentifying Variable Names of Highly Correlated Variables
        highlyCorCol <- colnames(dataSet)[highlyCorrelated]

        # Print highly correlated attributes
        #highlyCorCol

        # Remove highly correlated variables and create a new dataset
        dat <- dataSet[, -which(colnames(dataSet) %in% highlyCorCol)]
        #dim(dat)

        return(dat)

}

