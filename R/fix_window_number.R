#' Make window number consistent between GGIR output and GGIRmatcher output
#' `r lifecycle::badge("experimental")`
#' 
#' @description
#' Fix window numbers to make them consistent between GGIR output and 
#' GGIRmatcher output. This is specially relevant when additional devices has
#' started the recording in different dates than the accelerometers. Also relevant
#' for OO windows as calendar dates in GGIR output may differ from the start_date
#' in GGIRmatcher depending on whether sleep onset is before or after midnight. 
#'
#' @param WS data frame with output from GGIRmatcher
#' @param ggir_output data frame with output from GGIR part 5
#' @param ts Matched time series with \link{match_time_series}
#'
#' @return Merged output with GGIR and GGIRmatcher window-level estimates.
#' @export
#' 
fix_window_number = function(WS, ggir_output, ts = NULL) {
  # if there is no GGIR or GGIRmatcher output, then only dummy output
  if (is.null(ggir_output) | is.null(WS)) {
    if (is.null(ggir_output)) {
      ggir_output = matrix(NA, nrow = nrow(WS), ncol = 8) # very basic ggir output if not available
      colnames(ggir_output) = c("ID", "filename", "calendar_date", "weekday", "window_number",
                                "window", "start_end_window", "ggir_available")
      ggir_output = as.data.frame(ggir_output)
      ggir_output$ID = unique(WS$ID)
      ggir_output$window = WS$window
      ggir_output$window_number = WS$window_number
      ggir_output$calendar_date = WS$start_date
      ggir_output$start_end_window = paste(WS$start_time, WS$end_time, sep = "-")
      ggir_output$ggir_available = FALSE
      ggir_output$weekday = weekdays(as.Date(ggir_output$calendar_date), abbreviate = F)
      # also, additional available is true for all windows as it is the only output
      WS$additional_available = TRUE
    }
    if (is.null(WS)) {
      # fill up values needed for merging
      WS = matrix(NA, nrow = nrow(ggir_output), ncol = 8) # very basic ggir output if not available
      colnames(WS) = c("ID", "window_number", "window", 
                       "start_date", "start_time", "end_date", "end_time", 
                       "additional_available")
      WS = as.data.frame(WS)
      WS$ID = unique(ggir_output$ID)
      WS$window_number = ggir_output$window_number
      WS$window = ggir_output$window
      WS$start_date = ggir_output$calendar_date
      WS$start_time = substr(ggir_output$start_end_window,1,8)
      WS$end_date = ggir_output$calendar_date
      WS$end_time = substr(ggir_output$start_end_window,10,17)
      WS$additional_available = FALSE
      ggir_output$ggir_available = TRUE
      # revise calendar date in ggir_output
      ooi = which(WS$window == "OO") 
      if (length(ooi) > 0) {
        # detect start and end dates and times from time series
        onsets = which(diff(c(0, ts$SleepPeriodTime)) == 1)
        WS$start_date[ooi] = as.character(as.Date(strptime(ts$timestamp[onsets],
                                                           format = "%Y-%m-%dT%H:%M:%S%z",
                                                           tz = Sys.timezone())))[1:length(ooi)]
        WS$start_time[ooi] = format(strptime(ts$timestamp[onsets], format = "%Y-%m-%dT%H:%M:%S%z",
                                             tz = Sys.timezone()), format = "%H:%M:%S")[1:length(ooi)]
        WS$end_date[ooi] = as.character(as.Date(strptime(ts$timestamp[onsets - 1],
                                                         format = "%Y-%m-%dT%H:%M:%S%z",
                                                         tz = Sys.timezone())))[-1]
        WS$end_time[ooi] = format(strptime(ts$timestamp[onsets - 1], format = "%Y-%m-%dT%H:%M:%S%z",
                                           tz = Sys.timezone()), format = "%H:%M:%S")[-1]
        # and now revise ggir calendar dates based on WS start and end datetimes
        from = sort(as.POSIXlt(paste(WS$start_date[ooi], WS$start_time[ooi]), 
                               tz = Sys.timezone()))
        dates = lapply(from, FUN = function(x) ifelse(x$hour < 12, as.Date(x), as.Date(x) + 1))
        dates = as.Date(unlist(dates))
        names(dates) = ggir_output$window_number[ooi]
        for (oi in ooi) {
          datei = dates[which(names(dates) == ggir_output$window_number[oi])]
          ggir_output$calendar_date[oi] = as.character(as.Date(datei))
        }
      }
    }
  } else { # there is GGIR AND GGIRmatcher output, then fix window number
    WS$additional_available = TRUE
    WS$window_number = as.numeric(WS$window_number)
    ggir_output$ggir_available = TRUE
    ggir_output$window_number = as.numeric(ggir_output$window_number)
    # FIX WINDOW NUMBERS
    for (window_type in c("MM", "WW", "OO", "Segments")) {
      if (window_type == "Segments") {
        selggir = grep("MM|WW|OO", ggir_output$window, invert = T)
        seladd = grep("MM|WW|OO", WS$window, invert = T)
      } else {
        selggir = which(ggir_output$window == window_type)
        seladd = which(WS$window == window_type)
      }
      if (length(selggir) > 0 | length(seladd) > 0) { # at least 1 window in either ggir or ggirmatcher
        # handle daylight saving time in ggir calendar dates
        ggirdates = ggir_output$calendar_date[selggir]
        adddates = WS$start_date[seladd]
        if (window_type %in% c("MM")) {
          # then, calendar date == start date
          datesrange = range(c(as.Date(ggirdates), as.Date(adddates)))
          dates = as.character(as.Date(min(datesrange):max(datesrange)))
          names(dates) = rep(1:length(dates), times = table(dates))
          for (seli in selggir) {
            ggir_output$window_number[seli] = names(dates)[which(dates == ggir_output$calendar_date[seli])]
          }
          for (seli in seladd) {
            WS$window_number[seli] = names(dates)[which(dates == WS$start_date[seli])]
          }
          # fill up missing windows in GGIR output
          ggirmissing = which(!WS$window_number[seladd] %in% ggir_output$window_number[selggir])
          if (length(ggirmissing) > 0) {
            m2add = matrix(NA, nrow = length(ggirmissing), ncol = ncol(ggir_output))
            colnames(m2add) = colnames(ggir_output)
            m2add = as.data.frame(m2add)
            # fill up values needed for merging
            m2add$ID = unique(ggir_output$ID)
            m2add$filename = unique(ggir_output$filename)
            m2add$sleepparam = unique(ggir_output$sleepparam)
            m2add$boutcriter.in = unique(ggir_output$boutcriter.in)
            m2add$boutcriter.lig = unique(ggir_output$boutcriter.lig)
            m2add$boutcriter.mvpa = unique(ggir_output$boutcriter.mvpa)
            m2add$boutdur.in = unique(ggir_output$boutdur.in)
            m2add$boutdur.lig = unique(ggir_output$boutdur.lig)
            m2add$boutdur.mvpa = unique(ggir_output$boutdur.mvpa)
            m2add$TRLi = unique(ggir_output$TRLi)
            m2add$TRMi = unique(ggir_output$TRMi)
            m2add$TRVi = unique(ggir_output$TRVi)
            m2add$GGIRversion = unique(ggir_output$GGIRversion)
            m2add$calendar_date = WS$start_date[seladd][ggirmissing]
            m2add$weekday = weekdays(as.Date(m2add$calendar_date), FALSE)
            m2add$window_number = WS$window_number[seladd][ggirmissing]
            m2add$window = WS$window[seladd][ggirmissing]
            m2add$start_end_window = paste(WS$start_time[seladd][ggirmissing],
                                           WS$end_time[seladd][ggirmissing], sep = "-")
            m2add$ggir_available = FALSE
            ggir_output = rbind(ggir_output, m2add)
            
          }
          # fill up missing windows in GGIRmatcher output
          addmissing = which(!ggir_output$window_number[selggir] %in% WS$window_number[seladd])
          if (length(addmissing) > 0) {
            m2add = matrix(NA, nrow = length(addmissing), ncol = ncol(WS))
            m2add = as.data.frame(m2add)
            colnames(m2add) = colnames(WS)
            # fill up values needed for merging
            m2add$ID = unique(WS$ID)
            m2add$window_number = ggir_output$window_number[selggir][addmissing]
            m2add$window = ggir_output$window[selggir][addmissing]
            m2add$start_date = ggir_output$calendar_date[selggir][addmissing]
            m2add$start_time = substr(ggir_output$start_end_window[selggir][addmissing],1,8)
            m2add$end_date = ggir_output$calendar_date[selggir][addmissing]
            m2add$end_time = substr(ggir_output$start_end_window[selggir][addmissing],10,17)
            m2add$additional_available = FALSE
            WS = rbind(WS, m2add)
          }
        } else if (window_type %in% "WW") {
          # As WW windows are detected in ggir output,
          # there should never be missing ggir output.
          # fill up missing windows in GGIRmatcher output
          addmissing = which(!ggir_output$window_number[selggir] %in% WS$window_number[seladd])
          if (length(addmissing) > 0) {
            # sometimes ggir classifies last partial window as WW
            missing_wn = as.numeric(ggir_output$window_number[selggir][addmissing])
            if (missing_wn == max(as.numeric(ggir_output$window_number[selggir]))) {
              ggir_output = ggir_output[-selggir[addmissing],]
            }
          }
        } else if (window_type %in% "OO") {
          # Here, GGIR is not consistent in the labeling of window numbers
          # and calendar dates as sleep onset might be before or after midnight.
          # GGIR fails when onset 1 < midnight and onset 2 > midnight
          # then, calendar date == date between first and second onset
          from = sort(as.POSIXlt(paste(WS$start_date[seladd], WS$start_time[seladd]), 
                                 tz = Sys.timezone()))
          dates = lapply(from, FUN = function(x) ifelse(x$hour < 12, as.Date(x), as.Date(x) + 1))
          dates = as.Date(unlist(dates))
          names(dates) = rep(1:length(dates), times = table(dates))
          for (seli in selggir) {
            datei = dates[which(names(dates) == ggir_output$window_number[seli])]
            ggir_output$calendar_date[seli] = as.character(as.Date(datei))
          }
          # there should not be missing GGIR windows as they are detected over GGIR output
          # No GGIRmatcher output missing either, as both GGIR and GGIRmatcher exclude partial OO windows 
        } else if (window_type == "Segments") {
          # then, calendar date == start date
          datesrange = range(c(as.Date(ggirdates), as.Date(adddates)))
          dates = as.character(as.Date(min(datesrange):max(datesrange)))
          names(dates) = rep(1:length(dates), times = table(dates))
          for (seli in selggir) {
            ggir_output$window_number[seli] = names(dates)[which(dates == ggir_output$calendar_date[seli])]
          }
          for (seli in seladd) {
            WS$window_number[seli] = names(dates)[which(dates == WS$start_date[seli])]
          }
          # fill up missing windows in GGIR output
          windowsggir = paste(ggir_output$window_number[selggir], ggir_output$window[selggir])
          windowsadd = paste(WS$window_number[seladd], WS$window[seladd])
          ggirmissing = which(!windowsadd %in% windowsggir)
          if (length(ggirmissing) > 0) {
            m2add = matrix(NA, nrow = length(ggirmissing), ncol = ncol(ggir_output))
            m2add = as.data.frame(m2add)
            colnames(m2add) = colnames(ggir_output)
            # fill up values needed for merging
            m2add$ID = unique(ggir_output$ID)
            m2add$filename = unique(ggir_output$filename)
            m2add$calendar_date = WS$start_date[seladd][ggirmissing]
            m2add$weekday = weekdays(as.Date(m2add$calendar_date), FALSE)
            m2add$window_number = WS$window_number[seladd][ggirmissing]
            m2add$window = WS$window[seladd][ggirmissing]
            m2add$start_end_window = paste(WS$start_time[seladd][ggirmissing],
                                           WS$end_time[seladd][ggirmissing], sep = "-")
            m2add$sleepparam = unique(ggir_output$sleepparam)
            m2add$boutcriter.in = unique(ggir_output$boutcriter.in)
            m2add$boutcriter.lig = unique(ggir_output$boutcriter.lig)
            m2add$boutcriter.mvpa = unique(ggir_output$boutcriter.mvpa)
            m2add$boutdur.in = unique(ggir_output$boutdur.in)
            m2add$boutdur.lig = unique(ggir_output$boutdur.lig)
            m2add$boutdur.mvpa = unique(ggir_output$boutdur.mvpa)
            m2add$TRLi = unique(ggir_output$TRLi)
            m2add$TRMi = unique(ggir_output$TRMi)
            m2add$TRVi = unique(ggir_output$TRVi)
            m2add$GGIRversion = unique(ggir_output$GGIRversion)
            m2add$ggir_available = FALSE
            ggir_output = rbind(ggir_output, m2add)
          }
          # fill up missing windows in GGIRmatcher output
          addmissing = which(!windowsggir %in% windowsadd)
          if (length(addmissing) > 0) {
            m2add = matrix(NA, nrow = length(addmissing), ncol = ncol(WS))
            m2add = as.data.frame(m2add)
            colnames(m2add) = colnames(WS)
            # fill up values needed for merging
            m2add$ID = unique(WS$ID)
            m2add$window_number = ggir_output$window_number[selggir][addmissing]
            m2add$window = ggir_output$window[selggir][addmissing]
            m2add$start_date = ggir_output$calendar_date[selggir][addmissing]
            m2add$start_time = substr(ggir_output$start_end_window[selggir][addmissing],1,8)
            m2add$end_date = ggir_output$calendar_date[selggir][addmissing]
            m2add$end_time = substr(ggir_output$start_end_window[selggir][addmissing],10,17)
            m2add$additional_available = FALSE
            WS = rbind(WS, m2add)
          }
        }
      }
    }
  }
  output = merge(WS, ggir_output, by = c("ID", "window_number", "window"))
  # test everything went good
  output2 = merge(WS, ggir_output, by = c("ID", "window_number", "window"), all = T)
  return(output)
}
