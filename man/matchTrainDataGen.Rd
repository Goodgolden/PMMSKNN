% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchTrainDataGen.R
\name{matchTrainDataGen}
\alias{matchTrainDataGen}
\title{Match Training Data Generating Function: create Data based on pmm}
\usage{
matchTrainDataGen(
  train_post = train_post,
  ord_data,
  mtype = 1,
  n,
  m = 5,
  i,
  time_elapsed = time_elapsed,
  outcome = outcome,
  seed = seed,
  loocv = loocv,
  matchprobweight
)
}
\arguments{
\item{train_post}{- Data frame that contains the post-baseline observations from the training dataset. Typically this would be the \code{train_post} list component that was generated from the \code{\link{preproc}} function}

\item{ord_data}{Data frame. Specifically, training data with patient_id ordered based on fitted distal outcome value using predicted mean matching.
Generated using \code{\link{preproc}}. Example, \code{x <- preproc()},
then \code{x$train_o} would be used for this parameter.}

\item{mtype}{- Integer value indicating matching type. Default is set to 1 which follows the
matching of patients based on recommendation from \href{https://stefvanbuuren.name/fimd/sec-pmm.html}{van Buuren et al.}. \code{mtype} values are
from \code{0} to \code{4}}

\item{n}{Number of matches}

\item{m}{- For \code{mtype = 4}, which is type 4 matching from \href{https://stefvanbuuren.name/fimd/sec-pmm.html}{van Buuren et al.}, the Number of repititions for obtaining \eqn{\dot{y}} in terms of the predictive mean matching process.}

\item{i}{(Patient) Id indicator.}

\item{time_elapsed}{Name of the time variable. (type=string)}

\item{outcome}{Name of the outcomes variable.}

\item{seed}{Seed for randomly selecting matches based on differences in predicted y}

\item{loocv}{- Logical (\code{TRUE/FALSE}) that specifies whether
or not to perform leave-one-out cross validation or just output
predictions without hyperparameter tuning. If \code{loocv=FALSE}, then
users need to specify the value of the nearest_n}

\item{matchprobweight}{Logical (\code{TRUE/FALSE}) that specifies whether to utilize probability sampling for selecting patients according to difference in the distance between the neighbor.}
}
\value{
A vector of (patient) id numbers
}
\description{
Match Training Data Generating Function: create Data based on pmm
}
