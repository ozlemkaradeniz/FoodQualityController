% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RandomForest.R
\name{randomForest.run}
\alias{randomForest.run}
\title{Main function to calculate performance of Random forest model}
\usage{
\method{randomForest}{run}(regressionParameterList)
}
\arguments{
\item{regressionParameterList}{a list which contains
number_of_iterations: number of Iterations to calculate performance
pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
is supported)
percentageForTrainingSet: percentage of samples in training dataset
dataSet: dataFrame which is read from data file and subjected to the model.}
}
\value{
a list containing performance results
RMSEList: a list which contains RMSE of each iteration
cumulativeRMSEList: a list which contains cumulative RMSE mean in
each iteration
RMSE: mean RMSE of all iterations
RSquareList: a list which contains RSquare of each iteration
cumulativeRSquareList : a list which contains cumulative RSquare mean in
each iteration
RSquare: mean RSquare of all iterations
bestHyperParamsList: a list containing best mtry value and default number of trees
}
\description{
this function calculates performance of Random forest model
through iterations and returns performance metrics.In each iteration different
partitioning is done on dataset to create training and validation datasets,
tuning is done on training dataset to find optimum mtry-value
}
\examples{
\dontrun{randomForest.run(regressionParameterList)}
}
\author{
Ozlem Karadeniz \email{ozlem.karadeniz.283@cranfield.ac.uk}
}
