% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PLS-PCR.R
\name{pls.pcr.run}
\alias{pls.pcr.run}
\title{Main function to calculate  performance of Partial least squares regression
and PCA regression}
\usage{
pls.pcr.run(regressionParameterList)
}
\arguments{
\item{regressionParameterList}{a list which contains
number_of_iterations: number of iterations to calculate performance
pretreatment: data pretreatment method (auto-scale, mean-center or range-scale
is supported)
percentageForTrainingSet: percentage of samples in training dataset
method: method name as PLS or PCA
dataSet: dataFrame which is read from data file and subjected to the model.}
}
\value{
a list containing performance results
RMSEList: a list which contains RMSE of each iteration
cumulativeRMSEList: a list which contains cumulative RMSE mean in
each iteration#'
RMSE: mean RMSE of all iterations
RSquareList: a list which contains RSquare of each iteration
cumulativeRSquareList : a list which contains cumulative RSquare mean in
each iteration
RSquare: mean RSquare of all iterations
}
\description{
This function calculates performance of Partial least squares
regression and PCA regression through iterations and returns performance metrics.
In each iteration different partitioning is done on dataset to create
training and validation datasets.
}
\examples{
\dontrun{pls.pcr.run(regressionParameterList)}
}
\author{
Ozlem Karadeniz \email{ozlem.karadeniz.283@cranfield.ac.uk}
}
