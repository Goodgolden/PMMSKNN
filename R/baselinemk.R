#' Function to add baseline variable
#'
#' @description `baseline` column is created based on the full data frame
#' provided by the user. If the `time_var` column contains
#' more than one potential baseline values (i.e. values <= 0)
#' then `baselinemk()` will arrange the times according to
#' closest value to 0 then assign that as baseline == 1
#' while the other potential baseline values will be assigned == -1.
#'
#' @param df0 data frame to transform
#'   (i.e. data frame where a new column - baseline column - will be added).
#'   The data frame must contain specified `pat_id` and `time_var`.
#' @param pat_id string of characters representing the column
#'   of patient id.
#' @param time_var string of characters representing the column
#'   of time.
#'
#' @return A data frame that contains a new `baseline` column 
#'         indicating the baseline observation (i.e. row).
#'         `baseline` == 1: the baseline time point;
#'         `baseline` == -1: the time points before baseline;
#'         `baseline` == 0: the time points after baseline.
#'
#' @export
baselinemk <- function(df0,
                       pat_id = "patient_id",
                       time_var = "time") {
  ## add conditions ---------------------------------------------
  ## Mon Apr 12 11:38:40 2021 ------------------------------
  if ("baseline" %in% names(df0)) {
    
    warning("the baseline already existed in the data frame!")
    stop("please modify your data frame!")
    
  } else {
    time_string <- paste0(time_var, " <= 0")
    ## id_string <- paste0(pat_id)
    ## group_string <- paste0("df0$", pat_id)

    df1 <- df0 %>%
      # Mon Apr 12 11:52:17 2021 ------------------------------
      dplyr::select(!! rlang::parse_expr(pat_id), 
                    !! rlang::parse_expr(time_var)) %>%
      filter(!! rlang::parse_expr(time_string)) %>%
      arrange(!! rlang::parse_expr(pat_id), desc(!! rlang::parse_expr(time_var))) %>%
      group_by(!! rlang::parse_expr(pat_id)) %>%
      dplyr::mutate(baseline = if_else(row_number() == 1, 1, -1))
  
    df2 <- left_join(df0, df1, 
                     by = c(pat_id, time_var)) %>% 
      ## problem with the original codes
      ## no matter what the baseline is always 0
      mutate(baseline = if_else(is.na(.data$baseline), 0, .data$baseline))
    
    return(df2)
  }
}


