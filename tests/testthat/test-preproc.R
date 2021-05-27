test_that("multiplication works", {
  expect_equal(2 * 2, 4)

    data("ChickWeight")
  set.seed(555)

  # load only the TUG dataset
  full0 <- ChickWeight %>%
    ## need to exclude the above patients
    ## exclude also time > 200
    ## filter(!patient_id %in% exclude$patient_id & time < 200)
    filter(Time < 200) %>%
    mutate(Chick = as.numeric(as.character(Chick)),
           train_test = ifelse(Chick %in% c(1, 2, 20, 30, 40), 2, 1),
           Diet = as.numeric(as.character(Diet))) %>%
    ## Need to have distinct patient id's for the full data
    distinct(Chick, Time, .keep_all = TRUE)

  full <- PMMSKNN::baselinemk(full0,
                               pat_id = "Chick",
                               time_var = "Time") %>%
    distinct(Chick, Time, .keep_all = TRUE) %>%
    mutate(Time = as.integer(Time))
  
  # dff <- full
  # outcome = "weight"
  # time_var = "Time"
  # pat_id = "Chick"
  # baseline_var = "baseline"
  # varlist = NULL
  # View(dff)
  

  test_proc <- PMMSKNN::preproc(
    ## specify full dataset name
    dff = full,
    ## train test split variable
    split_var = "train_test",
    ## training set value
    trainval = 1,
    ## test set value
    testval = 2,
    ## Specify broken stick knots
    knots_exp = c(0, 4, 8, 16),
    ## specify which timepoint to use
    out_time = 16,
    ## specify outcome variable name
    outcome = "weight",
    ## specify time variable name 
    time_var = "Time",
    ## specify patient id variable name
    pat_id = "Chick",
    ## specify list of covariates for pmm
    ## filter_exp = "time > 3"
    ## filter observations that will be included
    varlist = c("Diet"))

  
})
