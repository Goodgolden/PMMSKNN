% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loocv_function_bs.R
\name{loocv_function_bs}
\alias{loocv_function_bs}
\title{Leave One Out Cross Validation function that Calculates
Bias, Coverage and Precision for neighbors-based prediction
Using a simplified BrokenStick prediction framework}
\usage{
loocv_function_bs(
  nearest_n = seq(20, 150, by = 10),
  train_post,
  ord_data,
  test_post,
  test_o,
  bs_obj,
  outcome,
  plot = FALSE,
  matchprobweight = FALSE,
  time_window = NULL,
  interval = NULL,
  thresh_val = 10000,
  userchoose = NULL,
  seed = 1234,
  parallel = NULL,
  loocv = TRUE,
  mtype = 1,
  biasm = "raw",
  m = 5,
  perfrank = "cov",
  opt_cov = 0.5,
  perf_round_by = 4,
  ...
)
}
\arguments{
\item{nearest_n}{Numeric vector indicating the nearest number of matches
to select from. The \code{loocv_function} will iterate through
the number within the vector and select the number of matches that has
optimal bias, coverage, and precision.
The number of nearest_n should be within the range of number of individuals
That are in the data. For example if there are 500 individuals in the data,
one could do \code{nearest_n <- 10:100}}

\item{train_post}{Data frame that contains the post-baseline observations from the training dataset. Typically this would be the \code{train_post} list component that was generated from the \code{\link{preproc}} function}

\item{ord_data}{Data frame. Specifically, training data with patient_id ordered based on fitted distal outcome value using predicted mean matching.
Generated using \code{\link{preproc}}. Example, \code{x <- preproc()},
then \code{x$train_o} would be used for this parameter.}

\item{test_post}{Data frame that contains the post-baseline observations from the testing dataset. Typically this would be the \code{train_post} list component that was generated from the \code{\link{preproc}} function}

\item{test_o}{Data frame. Specifically, testing data with patient_id ordered based on fitted distal outcome value using predicted mean matching.
Generated using \code{\link{preproc}}. Example, \code{x <- preproc()},
then \code{x$test_o} would be used for this parameter.}

\item{bs_obj}{Brokenstick object outputted from \code{\link{preproc}}}

\item{outcome}{Name of the outcomes variable (type=string)}

\item{plot}{Logical (\code{TRUE/FALSE}) that specifies whether to output individual precision plots}

\item{matchprobweight}{Logical (\code{TRUE/FALSE}) that specifies whether to utilize probability sampling
when doing the mean matching. If TRUE, matches nearest n weighted on differnce in
predicted outcome.}

\item{time_window}{vector of numbers for `centiles.pred()`, `xvalues` argument. For example, specify such as \code{c(10:30)}}

\item{interval}{Int value specifying the interval of individuals to skip}

\item{thresh_val}{Numeric value indicating value of bias to ignore (not include in output) in terms of the leave-one-out cross validation process. The default is set to \code{thresh_val = 10000}}

\item{userchoose}{Int value indicating the choice that the user wants to use for the number of nearest matches}

\item{seed}{Seed for probability sampling of the nearest matches}

\item{parallel}{Number of cores used for the leave-one-out cross validation process. Default = 1}

\item{loocv}{Logical (\code{TRUE/FALSE}) that specifies whether
or not to perform leave-one-out cross validation or just output
predictions without hyperparameter tuning. If \code{loocv=FALSE}, then
users need to specify the value of the nearest_n}

\item{mtype}{Integer value indicating matching type. Default is set to 1 which follows the
matching of patients based on recommendation from \href{https://stefvanbuuren.name/fimd/sec-pmm.html}{van Buuren et al.}. \code{mtype} values are
from \code{0} to \code{4}}

\item{biasm}{Column indicating which bias measure to use for
choosing the optimal nearest number of neighbors.
Default is \code{'raw'}. Options: \code{'raw','rmse','zsc'}.}

\item{m}{- For \code{mtype = 4}, which is type 4 matching from \href{https://stefvanbuuren.name/fimd/sec-pmm.html}{van Buuren et al.}, the Number of repititions for obtaining \eqn{\dot{y}} in terms of the predictive mean matching process.}

\item{perfrank}{String indicating how to rank the performance of the LOOCV. Default is `perfrank == "cov"`, which prioritizes LOOCV based on prefering coverage values that are close to 0.5. Then the lowest `rmse` value then `prec` value is prefered,}

\item{opt_cov}{Numeric value to indicate what the optimal coverage value is for calculating the performance. Default is `opt_cov = 0.5` (i.e. 50\% coverage).}

\item{perf_round_by}{Integer value to indicate what decimal point will the performance values should be rounded by. Default is `perf_round_by = 4`, set to smaller value to be less coarse about ranking `nearest_n` values.}

\item{\dots}{Passed down to \code{gamlss}}
}
\value{
Returns a list of 3 lists and a value. 1) \code{pred_res} contains a list of predicted values for the training data (\code{pred_train}) and test data (\code{pred_test}), the performance (\code{bias},\code{rmse},\code{zscore},\code{iqrcoverage},\code{precisionvec}, and number of dropped cases in fitting the gamlss model(\code{dropped_cases})); 2) \code{loocv_res} contains the same lists described above for each models fitted using different values of number of nearest neighbors; 3) \code{loocv_score} contains the summarised performance measures as a data frame; 4) \code{nearest_n} contains the optimal number of matches based on the aggregate performance metric.
}
\description{
The function calculates three frequency-based parameters to
demonstate statistical quality of the neighbors-based prediction.
The parameters (i.e. performances measures) are
bias, coverage and the precision (50% prediction interval)
}
