#' Get Indices for Valid Windows
#' 
#' @description
#' From the window-level full dataset, it extracts the indices (rows) for the 
#' windows that are considered valid based on the criteria specified by the user.#' 
#'
#' @param x Window-level data frame.
#' @param includecrit_day_spt Criteria to include data from the additional sensor in 
#' the full window as a proportion of recordings available from the total expected 
#' recordings in window (i.e., 0.7 for 70% of the recordings available).
#' @param includecrit_day Criteria to include data from the additional sensor in 
#' the awake hours window as a proportion of recordings available from the total expected 
#' recordings in window (i.e., 0.7 for 70% of the recordings available).
#' @param includecrit_spt Criteria to include data from the additional sensor in 
#' the sleep period time window as a proportion of recordings available from the total expected 
#' recordings in window (i.e., 0.7 for 70% of the recordings available).
#' @param additional_sf_seconds Expected sampling frequency of the additional sensor in seconds.
#' For example, \code{additional_sf_seconds = 900}, would assume that the additional sensor collects
#' data every 15 minutes, and therefore a maximum of 96 recordings are expected in a 24-hour window.
#' @param min_window_length Minimum window length required to consider a valid window in hours.
#' @param params_output See help documentation in the \code{GGIR} function.
#' @param params_cleaning See help documentation in the \code{GGIR} function.
#'
#' @return Matrix with indices for valid windows for GGIR and for additional sensor output
#' @export
#' `r lifecycle::badge("experimental")`
#'
getValidDayIndices = function(x, 
                              includecrit_day_spt = NULL,
                              includecrit_day = NULL,
                              includecrit_spt = NULL,
                              additional_sf_seconds = NULL,
                              min_window_length = NULL,
                              params_output, params_cleaning) {
  # DISCLAIMER: -------------------------------------------------------------
  # This is a simplified version of getValidDayIndices function that can
  # be found in GGIR::g.report.part5. 
  # Additions here:
  #     - extract indices for day_spt, day, and spt windows separately
  #     - extract indices for ggir output, and for additional output
  # -------------------------------------------------------------------------
  # GGIR inclusion criteria
  # day & spt
  if (params_cleaning[["includedaycrit"]] >= 0 & 
      params_cleaning[["includedaycrit"]] <= 1) { # if includedaycrit.part5 is used as a ratio
    includedayspt_wearPercentage = params_cleaning[["includedaycrit"]] * 100
    includedayspt_absolute = 0
  } else if (params_cleaning[["includedaycrit"]] > 1 &
             params_cleaning[["includedaycrit"]] <= 25) { # if includedaycrit.part5 is used like params_cleaning[["includedaycrit"]] as a number of hours
    includedayspt_wearPercentage = 0
    includedayspt_absolute = params_cleaning[["includedaycrit"]] * 60
  }
  # day
  if (params_cleaning[["includedaycrit.part5"]] >= 0 &
      params_cleaning[["includedaycrit.part5"]] <= 1) { # if includedaycrit.part5 is used as a ratio
    includeday_wearPercentage = params_cleaning[["includedaycrit.part5"]] * 100
    includeday_absolute = 0
  } else if (params_cleaning[["includedaycrit.part5"]] > 1 &
             params_cleaning[["includedaycrit.part5"]] <= 25) { # if includedaycrit.part5 is used like params_cleaning[["includedaycrit"]] as a number of hours
    includeday_wearPercentage = 0
    includeday_absolute = params_cleaning[["includedaycrit.part5"]] * 60
  }
  # spt
  if (params_cleaning[["includenightcrit"]] >= 0 &
      params_cleaning[["includenightcrit"]] <= 1) { # if includenightcrit is used as a ratio
    includespt_wearPercentage = params_cleaning[["includenightcrit"]] * 100
    includespt_absolute = 0
  } else if (params_cleaning[["includenightcrit"]] > 1 &
             params_cleaning[["includenightcrit"]] <= 25) { # if includenightcrit is used like params_cleaning[["includedaycrit"]] as a number of hours
    includespt_wearPercentage = 0
    includespt_absolute = params_cleaning[["includenightcrit"]] * 60
  }
  
  # ADDITIONAL cleaning:
  if (any(is.na(x$start_date))) {
    row = which(is.na(x$start_date))
    ggirdate = as.Date(x$calendar_date[row])
    x$start_date[row] = as.character(ggirdate)
    x$end_date[row] = as.character(ggirdate + 1)
    x$start_time[row] = substr(x$start_end_window[row], 1, 8)
    x$end_time[row] = substr(x$start_end_window[row], 10, 17)
  }
  # extra columns to easy check
  window_st = as.POSIXct(paste(x$start_date, x$start_time), tz = Sys.timezone())
  window_end = as.POSIXct(paste(x$end_date, x$end_time), tz = Sys.timezone())
  window_length = difftime(window_end, window_st, units = "mins") + 1
  maxRecWindow = as.numeric(window_length) * 60 / additional_sf_seconds
  maxRecday = as.numeric(x$dur_day_min) * 60 / additional_sf_seconds
  maxRecspt = as.numeric(x$dur_spt_min) * 60 / additional_sf_seconds
  nrecordings = grep("IN|LIG|MOD|VIG|MVPA|sleep|wake",
                     grep("^n_", colnames(x), value = T), invert = T, value = T)
  nrecordings_day_spt = grep("day_spt$", nrecordings, value = T)
  nrecordings_day = grep("day$", nrecordings, value = T)
  nrecordings_spt = grep("day", grep("_spt$", nrecordings, value = T), invert = T, value = T)
  x$perc_ADD_day_spt = as.numeric(x[,nrecordings_day_spt]) / maxRecWindow * 100
  x$perc_ADD_day = as.numeric(x[,nrecordings_day]) / maxRecday * 100
  x$perc_ADD_spt = as.numeric(x[,nrecordings_spt]) / maxRecspt * 100
  x$ADD_window_length
  
  # GGIR cleaning:
  # Add extra columns to ease the check
  x$nonwear_perc_day_spt = as.numeric(x$nonwear_perc_day_spt)
  x$nonwear_perc_day = as.numeric(x$nonwear_perc_day)
  x$nonwear_perc_spt = as.numeric(x$nonwear_perc_spt)
  x$wear_min_day_spt = (1 - (x$nonwear_perc_day_spt / 100)) * x$dur_day_spt_min #valid minute during waking hours
  x$wear_perc_day_spt = 100 - x$nonwear_perc_day_spt #wear percentage during full window
  x$wear_min_day = (1 - (x$nonwear_perc_day / 100)) * x$dur_day_min #valid minute during waking hours
  x$wear_perc_day = 100 - x$nonwear_perc_day #wear percentage during waking hours
  x$wear_min_spt = (1 - (x$nonwear_perc_spt / 100)) * x$dur_spt_min #valid minute during waking hours
  x$wear_perc_spt = 100 - x$nonwear_perc_spt #wear percentage during waking hours
  x$lasttimestamp = as.numeric(x$lasttimestamp)
  minimumValidMinutesMM = 0 # default
  if (length(params_cleaning[["includedaycrit"]]) == 2) {
    minimumValidMinutesMM = params_cleaning[["includedaycrit"]][2] * 60
  }
  if (params_output[["require_complete_lastnight_part5"]] == FALSE) {
    x$lastnight = FALSE
  } else {
    x$lastnight = x$window_number == max(x$window_number)
  }
  
  # if there is data cleaning file, exclude the windows indicated by the user
  include_window = rep(TRUE, nrow(x))
  if (length(params_cleaning[["data_cleaning_file"]]) > 0) { # allow for forced relying on guider based on external params_cleaning[["data_cleaning_file"]]
    DaCleanFile = data.table::fread(params_cleaning[["data_cleaning_file"]], data.table = FALSE)
    days2exclude = which(paste(x$ID, x$window_number) %in% paste(DaCleanFile$ID, DaCleanFile$day_part5))
    if (length(days2exclude) > 0) {
      include_window[days2exclude] = FALSE
    }
  } else {
    include_window = rep(TRUE,nrow(x))
  }
  # this is different to GGIR, with also allowing to specify day and spt valid indices
  # 1- valid day & spt requires: 
  #    - day_spt duration >= min_window_length
  #    - wear time >= includedayspt_wearPercentage | includedayspt_absolute
  ggir_day_spt = which(x$wear_perc_day_spt >= includedayspt_wearPercentage &
                         x$wear_min_day >= includedayspt_absolute &
                         x$dur_day_spt_min >= (min_window_length * 60) &
                         # x$dur_spt_min > 0 & x$dur_day_min > 0 &
                         ((x$lastnight == TRUE & x$lasttimestamp > 9) |
                            x$lastnight == FALSE) &
                         include_window == TRUE &
                         x$wear_min_day_spt >= minimumValidMinutesMM)
  add_day_spt = which(x$perc_ADD_day_spt >= includecrit_day_spt * 100 &
                        as.numeric(x[,nrecordings_day_spt]) >= min_window_length * 60^2 / additional_sf_seconds &
                        ((x$lastnight == TRUE & x$lasttimestamp > 9) |
                           x$lastnight == FALSE) &
                        include_window == TRUE)
  
  # 2- valid day: 
  #    - day hours are defined, then dur_day_min > 0
  #    - wear time day >= includeday_wearPercentage | includeday_absolute
  ggir_day = which(x$wear_perc_day >= includeday_wearPercentage &
                     x$wear_min_day >= includeday_absolute &
                     x$dur_day_min > 0 &
                     ((x$lastnight == TRUE & x$lasttimestamp > 9) |
                        x$lastnight == FALSE) &
                     include_window == TRUE &
                     x$wear_min_day_spt >= minimumValidMinutesMM)
  add_day = which(x$perc_ADD_day >= includecrit_day * 100 &
                    x$dur_day_min > 0 &
                    ((x$lastnight == TRUE & x$lasttimestamp > 9) |
                       x$lastnight == FALSE) &
                    include_window == TRUE)
  
  # 3- valid spt: 
  #    - spt hours are defined, then dur_day_min > 0
  #    - wear time day >= includespt_wearPercentage | includespt_absolute
  ggir_spt = which(x$wear_perc_spt >= includespt_wearPercentage &
                     x$wear_min_spt >= includespt_absolute &
                     x$dur_spt_min > 0 &
                     ((x$lastnight == TRUE & x$lasttimestamp > 9) |
                        x$lastnight == FALSE) &
                     include_window == TRUE &
                     x$wear_min_day_spt >= minimumValidMinutesMM)
  add_spt = which(x$perc_ADD_spt >= includecrit_spt * 100 &
                    x$dur_spt_min > 0 &
                    ((x$lastnight == TRUE & x$lasttimestamp > 9) |
                       x$lastnight == FALSE) &
                    include_window == TRUE)
  
  # exclude first and last window?
  if (params_cleaning[["excludefirstlast.part5"]] == TRUE) {
    x$window_number = as.numeric(x$window_number)
    # identify first and last day per file
    first_days = stats::aggregate(window_number ~ filename, data = x, FUN = min, na.rm = TRUE)
    last_days = stats::aggregate(window_number ~ filename, data = x, FUN = max, na.rm = TRUE)
    
    # match first and last days with the output dataframe
    exclude_firsts = which(paste(x$filename, x$window_number) %in% paste(first_days$filename, first_days$window_number))
    exclude_lasts = which(paste(x$filename, x$window_number) %in% paste(last_days$filename, last_days$window_number))
    
    # keep only indices that do not match with first and last days
    indices2exclude = which(indices %in% c(exclude_firsts, exclude_lasts))
    if (length(indices2exclude) > 0) indices = indices[-indices2exclude]
  }
  return(list(ggir_day_spt = ggir_day_spt, ggir_day = ggir_day, ggir_spt = ggir_spt,
              add_day_spt = add_day_spt, add_day = add_day, add_spt = add_spt))
}
