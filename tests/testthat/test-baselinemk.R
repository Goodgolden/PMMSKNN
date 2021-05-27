test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  
  # Load data and wrangle {{{
  data("ChickWeight")
  
  set.seed(555)
  ## Wrangle Steup {{{
  # load only the TUG dataset
  full <- ChickWeight %>%
    ## need to exclude the above patients
    ## exclude also time > 200
    ## filter(!patient_id %in% exclude$patient_id & time < 200)
    filter(Time < 200) %>%
    mutate(Chick = as.numeric(as.character(Chick)),
           train_test = ifelse(Chick %in% c(1, 2, 20, 30, 40), 2, 1),
           Diet = as.numeric(as.character(Diet))) %>%
    ## Need to have distinct patient id's for the full data
    distinct(Chick, Time, .keep_all = TRUE) %>%
    # Mon Apr 12 12:00:05 2021 ------------------------------
    ## add time == Time
    ## add patient_id == Chick
    ## avoid overwriting amap
    mutate(time = Time + 7,
           patient_id = Chick) 
  ## }}}
  
  ## load only the TUG dataset
  newdata  <- full  %>% 
    PMMSKNN::baselinemk("patient_id", "time") %>%
    ## filter 10 patients as test with baselinemk()
    mutate(
      patient_id = as.numeric(as.character(patient_id)),
      train_test = ifelse(patient_id %in% c(1, 2, 20, 30, 40), 2, 1),
      Diet = as.numeric(as.character(Diet))) %>% 
    ## add distinct patient id's for the full data
    distinct(patient_id, time, .keep_all = TRUE)

    ## if the dataset already has baseline,
    ## avoid adding extra column
    # newdata2  <- newdata  %>% 
    #   baselinemk("patient_id", "time")  
  View(newdata)
})
