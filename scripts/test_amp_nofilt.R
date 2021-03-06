# - - - - - - - - - - - - - - - - - - - - - #
# File Name: 	'test_amp_nofilt.R'
# Purpose:
# Created on: 	21-02-2019
# Modified on 	17-12-2018
# Created by:
# Contact Info: ck
# - - - - - - - - - - - - - - - - - - - - - #

library(pcr)
library(readxl) # read original data
library(dplyr) # ez data manipulation

# load only the TUG dataset
full  <- pcr::amp

# need to exclude the above patients
# exclude also time > 200
full <- full %>%
    #filter(!patient_id %in% exclude$patient_id & time < 200)
    filter(time < 200) %>%
    mutate(gender = as.factor(gender))

# ONLY TUG outcomes
train <- full %>% filter(train_test == 1)
test <- full %>% filter(train_test == 2)

# Train and Test split for all TKA outcomes: create 
set.seed(1234)
full <- pcr:::baselinemk(full, "patient_id", "time")

# make sure there aren't any missing data and etc
summary(full) ;  sapply(full, function(x) {
                          table(is.na(x))})

# train pre data
train_pre <- train %>%
    filter(time < 0)
# train post data select only days beyond 3 bc the 2nd day estimates are all ove rthe place.
train_post  <- train %>%
    filter(time > 3)

test_post  <- test %>%
    filter(time > 3)

#- - - - - - - - - - - - - - - - - - - - - - - - - - #
## Run the Long LOOCV Training Set Matchem Up!
#- - - - - - - - - - - - - - - - - - - - - - - - - - #

##extract fitted values for the linear model (PREOP only here)
#-- preprocess train and test data: involves
#1. Taking training post data and running broken stick to get y90 or yNU where NU is value chosen
#2. Processes test data set so that yNU is matched with training data
#3. The matched test and train data according to yNU is used to later match the personalized predicted gamlss

# Process the full data to get
# 1. Post operative test data 
# 2. Fitted 90-Day PLUSM for test patients
# 3. Post operative train data 
# 4. Fitted 90-Day PLUSM for train patients
test_proc <- preproc(
  dff=full, 
  split_var = 'train_test', # train test split variable
  #split_var = 'source', # for all_tka dataset
  trainval = 1, # training set value
  testval = 2, # test set value
  knots_exp = c(0, 50, 181), # Specify broken stick knots
  out_time = 181,  # specify which timepoint to use 
  #outcome = "tug", # specify outcome variable name
  outcome = "plusm",
  time_var = "time", # specify time variable name
  pat_id = "patient_id", # specify patient id variable name
  varlist = c("age","gender","bmi"), # specify list of covariates for pmm
  filter_exp = NULL               
)

# - - - - - - - - - - - - - - -#
# TRAINING DATA: LOOCV Result
# - - - - - - - - - - - - - - -#

# Call loocv_function
fin <- loocv_function(
  
  # specify number or vector of numbers from {1,...,total number of patients in training data} 
  nearest_n = seq(10,100, by=10),
  # enter training and testing post operative and fitted y90 dataset
  train_post = test_proc$train_post,
  ord_data = test_proc$train_o,
  test_post = test_proc$test_post,
  test_o = test_proc$test_o,
  # Specify outcome variable and time variable name
  outcome = "plusm",
  #outcome = "knee_flex",
  time_elapsed = "time",
  interval = NULL,
  
  # Specify use of cubic spline or not
  cs=TRUE,
  
  # specify degrees of freedom use or not
  #dfspec=NULL,
  dfspec=TRUE,
  
  # specify degree of freedom for location, scale and shape (d_f_* where * = {m, s} for location and scale default for shape is 1.
  # specify power transformation of location (ptr_m)
  d_f_m=2, ptr_m=1,
  #d_f_m=3, ptr_m=1,
  d_f_s=1,
  
  # Specify distribution for location, scale and shape 
  #dist_fam = gamlss.dist::NO)
  dist_fam = gamlss.dist::BCCGo)


# - - - - - - - - - - - - - - - - - - - - #
# Plots: Calibration, Bias/RMSE and Coverage
# - - - - - - - - - - - - - - - - - - - - #

#-- Bias, Cov, Pred
plot_cal(plotobj = fin, test_proc = test_proc, obs_dist = "median")
plot_cal(plotobj = fin, outcome="plusm", test_proc = test_proc, obs_dist = "median", loocv = FALSE)

# Calibration plot
# plot_cal(plotobj = fin, test_proc = test_proc, obs_dist = "median", 
#          loocv = FALSE)


# - - - - - - - - - - - - - - - - - - - - #
# Internal Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

loocvperf(fin$loocv_res, test_proc$train_o)

# - - - - - - - - - - - - - - - - - - - - #
# External Validation Performance Measures
# - - - - - - - - - - - - - - - - - - - - #

extvalid(fin, test_proc)
