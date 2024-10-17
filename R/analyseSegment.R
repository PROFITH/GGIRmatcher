#' Applies user-specified functions over the additional sensor output
#' 
#' @description
#' Applies the user-specified functions over the time series output of the 
#' additional sensor in the pre-specified windows of interest (i.e., 
#' midnight to midnight, waking-up to waking-up, sleep onset to sleep onset,
#' and segments of the day).
#'
#' @param ts The matched time series from \link{match_time_series}
#' @param splits Indices to separate windows of interest.
#' @param id ID to be used in the output data frames.
#' @param lmv Thresholds for light, moderate, and vigorous intensity from GGIR.
#' @param FUNs List of functions to be applied to the additional sensor data.
#' @param qwindow_names Character vector with names to be assigned to the segments of the day.
#' @param window_type Time window of interest for the aggregates ("MM", "WW", "OO", or "segments").
#' @param legend GGIR legend for class_id in the part5 time series output.
#'
#' @return aggregated data frame.
#' @export
#' @import plyr
#' @importFrom methods formalArgs
#' @import stats
analyseSegment = function(ts, id, splits, FUNs,
                          qwindow_names = NULL,
                          lmv = NULL,
                          window_type = "segments",
                          legend = NULL) {
  # identify additional metric name
  addmetrici = which(!colnames(ts) %in% c("timestamp","timenum","additional_available",
                                          "ACC","SleepPeriodTime","invalidepoch","guider",             
                                          "window","angle","class_id","invalid_fullwindow",  
                                          "invalid_sleepperiod","invalid_wakinghours","ggir_available"))
  an = colnames(ts)[addmetrici]
  # if all additional sensor is na, meaning no additional sensor info is available
  # return NULL
  if (all(is.na(ts[, an]))) return(NULL)
  # internal functions
  mincounter = function(x, epoch) sum(x) * epoch/60
  agg_per_levels = function(ws, x, time, windows, levels, FUN,
                            expected_levels = unique(levels),
                            levels_names = unique(levels),
                            preffix = "", ...) {
    exp_params = methods::formalArgs(FUN)
    if (any(grepl("time|df", exp_params, ignore.case = T))) {
      df = data.frame(time, windows, levels, x)
      df = df[complete.cases(df),]
      if (nrow(df) == 0) return(ws)
      tmp = tapply(df, list(df$windows, df$levels), FUN)
    } else {
      tmp = tapply(x, list(windows, levels), FUN)
    }
    # if output from user function is a matrix, then we need to unlist the window-level output
    M = matrix(NA, nrow = nrow(ws), ncol = 0)
    rownames(M) = rownames(ws)
    # here we make sure that all expected levels get in the matrix
    for (ci in 1:length(expected_levels)) {
      doFakeLevel = FALSE
      column = which(colnames(tmp) == expected_levels[ci])
      if (length(column) == 0 || length(column) > 0 && is.na(column)) doFakeLevel = T
      if (doFakeLevel == FALSE) {
        if (class(tmp[,1]) == "list") {
          thislevel = do.call(rbind, tmp[,column])
        } else {
          thislevel = matrix(tmp[,column], ncol = 1)
          rownames(thislevel) = rownames(tmp)
        }
      } else {
        if (class(tmp[,1]) == "list") {
          thislevel = do.call(rbind, tmp[,1])
          thislevel[which(!is.na(thislevel))] = NA
        } else {
          thislevel =  matrix(NA, nrow = nrow(tmp), ncol = 1)
          rownames(thislevel) = rownames(tmp)
        }
      }
      cnames = paste0(preffix, levels_names[ci])
      if (any(!is.null(colnames(thislevel)))) cnames = paste0(cnames, "_", colnames(thislevel))
      colnames(thislevel) = cnames
      # merge with M, avoiding warning on duplicated column name (it is expected)
      M = withCallingHandlers({
        merge(M, thislevel, by = "row.names", all = T, no.dups = F)
      }, warning = function(w) {
        if (grepl(pattern = "‘Row.names’", x = conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      })
      rownames(M) = M[,1]
    }
    # fix colnames
    if (any(duplicated(colnames(M)))) M = M[,-which(duplicated(colnames(M)))]
    colnames(M) = gsub("Row.names", "window_number", colnames(M))
    ws = merge(ws, M, by = "window_number", all.x = T)
    # ws = ws[order(as.numeric(ws[, "window_number"])), ]
    return(ws)
  }
  # identify epoch
  e0 = strptime(ts$timestamp[1], "%Y-%m-%dT%H:%M:%S%z")
  e1 = strptime(ts$timestamp[2], "%Y-%m-%dT%H:%M:%S%z")
  epoch = as.numeric(difftime(e1, e0, units = "secs"))
  # Only for MM and Segments, include first and last partial windows
  # For WW/OO we don't want this as they are from detected wake/onset until detected wake/onset
  if (window_type %in% c("MM", "segments")) {
    if (splits[1] > 1) {
      # add partial first window
      splits = c(1, splits)
      if (!is.null(qwindow_names)) {
        which_level = which(levels(qwindow_names) == qwindow_names[1]) - 1
        lvsegments = levels(qwindow_names)
        if (which_level == 0) which_level = length(lvsegments)
        qwindow_names = c(lvsegments[which_level], as.character(qwindow_names))
        qwindow_names = factor(qwindow_names, levels = lvsegments)
      }
    }
    if (nrow(ts) > max(splits)) {
      # add partial last window
      splits = c(splits, nrow(ts) + 1) # plus 1 becase end times would be splits - 1
    } 
  } else {
    # for windows WW or OO, fix last awake only epoch from GGIR
    if (max(splits) == nrow(ts)) splits = splits[-length(splits)]
  }
  # derive start and end from splits (first always is start of window)
  st = splits[-length(splits)]
  end = splits[-1] - 1
  nWindows = length(st)
  # start and end timestamp for splits
  t0 = c(ts$timestamp[st]); t0 = unique(t0)
  t1 = c(ts$timestamp[end]); t1 = unique(t1)
  t0 = strptime(t0, "%Y-%m-%dT%H:%M:%S%z")
  t1 = strptime(t1, "%Y-%m-%dT%H:%M:%S%z")
  nDates = length(unique(as.Date(t0)))
  # subset ts
  ts = ts[st[1]:end[length(end)],]
  # day and spt
  day = which(ts$SleepPeriodTime == 0)
  spt = which(ts$SleepPeriodTime == 1)
  # Now that ts is clean, derive windows and levels
  windows = rep(seq(length.out = length(splits) - 1), times = diff(splits))
  if (!is.null(lmv)) {
    levels = cut(ts$ACC, breaks = c(0, lmv, Inf), labels = c("IN", "LIG", "MOD", "VIG"), right = F)
    total_levels = c("IN", "LIG", "MOD", "VIG")
    dayspt_levels = c("day", "spt")
    legend_ids = legend$class_id
    legend_names = legend$class_name
  } else {
    levels = rep(1, nrow(ts))
    ts$SleepPeriodTime = 0
    total_levels = c("ignoreIN", "ignoreLIG", "ignoreMOD", "ignoreVIG")
    dayspt_levels = c("ignoreday", "ignorespt")
    ts$class_id = 0
    legend_ids = legend$class_id
    legend_names = paste0("ignore", legend$class_name)
  }
  # Initialize window summary
  # recording ID
  ws = matrix(id, nrow = nWindows, ncol = 1)
  colnames(ws)[1] = "ID"
  # date number
  if (window_type == "segments") {
    segments_all = paste(rep(1:nDates, table(as.Date(t0))), qwindow_names)
    ws = cbind(ws, segments_all)
    windows = rep(segments_all, times = table(windows))
  } else {
    ws = cbind(ws, seq(from = 1, length.out = nrow(ws)))
  }
  colnames(ws)[ncol(ws)] = "window_number"
  # start and end times
  ws = cbind(ws, format(t0, "%Y-%m-%d"), format(t0, "%H:%M:%S"))
  colnames(ws)[(ncol(ws) - 1):ncol(ws)] = c("start_date", "start_time")
  ws = cbind(ws, format(t1, "%Y-%m-%d"), format(t1, "%H:%M:%S"))
  colnames(ws)[(ncol(ws) - 1):ncol(ws)] = c("end_date", "end_time")
  # segment names
  if (window_type == "segments") {
    ws = cbind(ws, as.character(qwindow_names))
  } else {
    ws = cbind(ws, window_type)
  }
  colnames(ws)[ncol(ws)] = "window"
  ws = as.data.frame(ws)
  # ws$window_number = as.numeric(ws$window_number)
  # TOTAL WINDOW ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts[, an], time = ts$timenum,
                        windows = windows, levels = rep(1, nrow(ts)),
                        expected_levels = 1, levels_names = "day_spt",
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # DAY & SPT ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts[, an], time = ts$timenum,
                        windows = windows, levels = ts$SleepPeriodTime,
                        expected_levels = 0:1, levels_names = dayspt_levels,
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # TOTAL LEVELS ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts[day, an], time = ts$timenum[day],
                        windows = windows[day], levels = as.numeric(levels[day]),
                        expected_levels = 1:length(total_levels), 
                        levels_names = total_levels,
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # CLASS_ID LEVELS ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts[, an], time = ts$timenum,
                        windows = windows, levels = ts$class_id,
                        expected_levels = legend_ids, 
                        levels_names = legend_names, 
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # CLEAN UP WINDOW_NUMBER IF SEGMENTS --------------------------------------
  if (window_type == "segments") {
    ws$window_number = unlist(lapply(ws$window_number, 
                                     FUN = function(x) as.numeric(unlist(strsplit(x, "[ ]"))[1])))
  } 
  
  # ORDER COLUMNS ------------------
  first =  c("ID",
             grep("^window", colnames(ws), value = T), 
             grep("^start", colnames(ws), value = T), 
             grep("^end", colnames(ws), value = T),
             grep("^segment", colnames(ws), value = T))
  addCols = grep(an, colnames(ws), value = T)
  ws = ws[, c(first, addCols)]
  
  # REMOVE IGNORE COLUMNS ---------------------------------------------------
  # When ggir file is not available, output has been simmulated and should be removed
  del = grep("ignore", colnames(ws))
  if (length(del) > 0) {
    ws[,del][!is.na(ws[,del])] = NA
    colnames(ws) = gsub("ignore", "", colnames(ws))
  }
  # return ------------------
  return(ws)
}

