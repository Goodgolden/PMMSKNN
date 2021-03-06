% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perfCalcLMM.R
\name{perfCalcLMM}
\alias{perfCalcLMM}
\title{Calculate Performance (bias, coverage, and precision) based on fitting a linear Mixed Model to longitudinal data}
\usage{
perfCalcLMM(fit, n = 1000, train, test, parallel = FALSE, outcome)
}
\arguments{
\item{fit}{An object produced by \code{lmer} function.}

\item{n}{Number of simulations for obtaining prediction
and prediction Interval. Default = 1000.}

\item{train}{A dataframe of training observations with \code{outcome} column
included.}

\item{test}{A dataframe of testing observations with \code{outcome} column
included.}

\item{parallel}{Boolean value (i.e. \code{TRUE/FALSE}) indicating whether or not to use parallel computing.
Default is set to FALSE. If \code{true}, then uses multicore capability.}

\item{outcome}{A column name (i.e. string type) indicating \code{outcome} column}
}
\value{
A list of data frames. 1) \code{perfsum}: Data frame that summarizes the performance based on a variety of variance/confidence interval estimation of LMM; 2) \code{predictInterval1, predictInterval1a, predictInterval1b}: List of two data frames that contain the prediction (95% CI) for training and testing data; 3) \code{bootMerpar, bootMersp}: List of two data frames that contain the prediction (95% CI) for training and testing data using bootMer package
}
\description{
Creates Data Frame:
\enumerate{
  \item Bias (Difference in Obs vs Pred)
  \item Coverage (% time obs value in Pred Interval)
  \item Precision (Width of Pred Interval)}
}
