% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loocv_function_sknn.R
\name{loocv_function_sknn}
\alias{loocv_function_sknn}
\title{Calculate bias, coverage and CI width for neighbors-based prediction}
\usage{
loocv_function_sknn(
  nearest_n = seq(20, 150, by = 10),
  dist_fam = NULL,
  fulldata,
  train_test = "train_test",
  patid = "patient_id",
  formula,
  train_post,
  test_post,
  outcome,
  time_elapsed = "time",
  plot = FALSE,
  matchprobweight = FALSE,
  time_window = NULL,
  interval = NULL,
  cs = FALSE,
  dfspec = NULL,
  d_f_m = 1.8,
  ptr_m = 1,
  d_f_s = 1.2,
  d_f_n = 1,
  d_f_t = 1,
  thresh_val = 10000,
  printtrace = FALSE,
  userchoose = NULL,
  parallel = NULL,
  loocv = TRUE,
  seed = 1234,
  biasm = "raw",
  perfrank = "cov",
  opt_cov = 0.5,
  perf_round_by = 4,
  ...
)
}
\arguments{
\item{nearest_n}{Numeric vector with number of matches per scenario}

\item{dist_fam}{gamlss distribution specification}

\item{fulldata}{- dataset, the full data typically the \code{train_post} list component}

\item{train_test}{Column name indicating whether individual belongs to
Training or Testing dataset. Train = 1, Test = 2 by default.}

\item{patid}{Column name indicating patient id.}

\item{formula}{Formula indicating the variables used for matching.
(e.g. \code{ ~ var1 + var2 + var3 }).}

\item{train_post}{- datasets, typically the \code{train_post} list component
of the object produced by \code{\link{preproc}}.}

\item{test_post}{Idem, component \code{test_post}}

\item{outcome}{Name of the outcomes variable}

\item{time_elapsed}{- Name of the time variable. (type=string)}

\item{plot}{- Logical that specifies whether to output individual precision plots}

\item{matchprobweight}{- Logical that specifies whether to utilize probability sampling
when doing the mean matching. If TRUE, matches nearest n weighted on differnce in
predicted outcome.}

\item{time_window}{- vector of numbers for `centiles.pred()`, `xvalues` argument}

\item{interval}{- Int value specifying the interval of individuals to skip}

\item{cs}{Logical that specifies whether to use cubic spline.
The default \code{cs = FALSE} uses ...}

\item{dfspec}{Logical that specifies whether to the user sets
degrees of freedom (...not clear to me what it does, and how it
interacts with the next set of arguments)}

\item{d_f_m}{... explain: arguments probably mean different things for different distributions. Might be preferable to package it with \code{dist_fam}}

\item{ptr_m}{-}

\item{d_f_s}{-}

\item{d_f_n}{-}

\item{d_f_t}{-}

\item{thresh_val}{-}

\item{printtrace}{- Logical that specifies printing of gamlss parameter estimation}

\item{userchoose}{- Int value indicating the choice that the user wants to use for the number of nearest matches}

\item{parallel}{- Number of cores used for parallel computing. Default = 1}

\item{loocv}{- Whether or not to perform leave one out cross validation or just go straight to prediction. Should
have the userchoose value specified if `loocv=FALSE`}

\item{seed}{- Seed for probability sampling of the nearest matches}

\item{biasm}{- Column indicating which bias score to use for choosing optimal n. Default
is \code{'raw'}. Options: \code{'raw','rmse','zsc'}.}

\item{perfrank}{- String indicating how to rank the performance of the LOOCV. Default is `perfrank == "cov"`, which prioritizes LOOCV based on prefering coverage values that are close to 0.5. Then the lowest `rmse` value then `prec` value is prefered,}

\item{opt_cov}{- Float value indicating optimal coverage value used for `perfrank`. Defaults to 0.5}

\item{perf_round_by}{- Integer value to indicate what decimal point will the performance values should be rounded by. Default is `perf_round_by = 4`, set to smaller value to be less coarse about ranking `nearest_n` values.}

\item{\dots}{Passed down to \code{gamlss}}
}
\value{
There are many possible return values
}
\description{
The function calculate three frequency-based parameters to
demonstate statistical quality of the neighbors-based prediction.
The parameters are bias, coverage and the 50% prediction interval
width.
}
