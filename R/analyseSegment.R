#' Title
#'
#' @param ts 
#' @param splits 
#' @param id 
#' @param lmv 
#' @param FUNs 
#' @param addCutpoints 
#' @param qwindow_names 
#' @param window_type 
#' @param legend 
#'
#' @return aggregated data frame.
#' @export
#' @import plyr
analyseSegment = function(ts, id, splits, FUNs,
                          addCutpoints = NULL,
                          qwindow_names = NULL,
                          lmv = c(30, 100, 430),
                          window_type = "segments",
                          legend = NULL) {
  # identify additional metric name
  addmetrici = which(!colnames(ts) %in% c("timestamp","timenum","additional_available",
                                          "ACC","SleepPeriodTime","invalidepoch","guider",             
                                          "window","angle","class_id","invalid_fullwindow",  
                                          "invalid_sleepperiod","invalid_wakinghours","ggir_available"))
  an = colnames(ts)[addmetrici]
  # internal functions
  mincounter = function(x, epoch) sum(x) * epoch/60
  agg_per_levels = function(ws, x, windows, levels, FUN,
                            expected_levels = unique(levels),
                            levels_names = unique(levels),
                            preffix = "", suffix = "", ...) {
    otherinput = list(...)
    exp_params = formalArgs(FUN)
    if (length(exp_params) > 1) {
      dat = as.data.frame(cbind(x, do.call(cbind, otherinput), windows, levels))
      colnames(dat) = c(exp_params, "windows", "levels")
      FF <<- FUN
      M = plyr::ddply(dat, .(windows, levels), summarise, auc = FF(x, time))
    } else {
      M = aggregate(x ~ windows + levels, FUN = FUN)
    }
    if (length(unique(levels)) > 1) {
      M = reshape(M, direction = "wide", idvar = "windows", timevar = "levels")
      colnames(M) = gsub("^x.", "", colnames(M))
      if (any(!expected_levels %in% colnames(M))) {
        addvar = expected_levels[which(!expected_levels %in% colnames(M))]
        m2add = matrix(NA, nrow = nrow(M), ncol = length(addvar))
        M = cbind(M, m2add)
        colnames(M)[(ncol(M) - ncol(m2add) + 1):ncol(M)] = addvar
      }
      M = M[, c("windows", expected_levels)]
    } else {
      M = M[,-2]
    }
    # order
    M = M[order(M[, "windows"]), ]
    colnames(M) = c("window_number", paste0(preffix, levels_names, suffix))
    ws = merge(ws, M, by = "window_number", all.x = T)
    return(ws)
  }
  # identify epoch
  e0 = strptime(ts$timestamp[1], "%Y-%m-%dT%H:%M:%S%z")
  e1 = strptime(ts$timestamp[2], "%Y-%m-%dT%H:%M:%S%z")
  epoch = as.numeric(difftime(e1, e0, units = "secs"))
  # get windows and levels vector
  windows = rep(seq(length.out = length(splits) - 1), times = diff(splits))
  levels = cut(ts$ACC, breaks = c(0, lmv, Inf), labels = c("IN", "LIG", "MOD", "VIG"), right = F)
  st = splits; end = splits - 1
  # add partial first and last window (only in MM and Segments)
  w0 = 1
  if (splits[1] > 1) {
    windows = c(rep(0, splits[1] - 1), windows)
    st = c(1, st)
    if (window_type %in% c("WW", "OO")) w0 = 0
    if (!is.null(qwindow_names)) {
      which_level = which(levels(qwindow_names) == qwindow_names[1]) - 1
      lvsegments = levels(qwindow_names)
      if (which_level == 0) which_level = length(lvsegments)
      qwindow_names = c(lvsegments[which_level], as.character(qwindow_names))
      qwindow_names = factor(qwindow_names, levels = lvsegments)
    }
  }
  # del_last_window = FALSE
  if (nrow(ts) > max(splits)) {
    windows = c(windows, rep(max(windows) + 1, nrow(ts) - max(splits) + 1))
    end = c(end, nrow(ts))
    # if (window_type %in% c("WW", "OO")) del_last_window = TRUE
  }
  # fix last epoch awake from GGIR
  if (max(st) == nrow(ts)) {
    if (length(windows) < nrow(ts)) windows = c(windows, rep(max(windows), nrow(ts) - length(windows)))
    end[length(end)] = st[length(st)]
    st = st[-length(st)]
  }
  nWindows = length(unique(windows))
  # start and end timestamp for splits
  t0 = c(ts$timestamp[st]); t0 = unique(t0)
  t1 = c(ts$timestamp[end]); t1 = unique(t1)
  t0 = strptime(t0, "%Y-%m-%dT%H:%M:%S%z")
  t1 = strptime(t1, "%Y-%m-%dT%H:%M:%S%z")
  nDates = length(unique(as.Date(t0)))
  # if (w0 == 0) {
  #   nDates = nDates - 1
  #   nWindows = nWindows - 1
  # }
  # if (del_last_window) {
  #   nDates = nDates - 1
  #   nWindows = nWindows - 1  
  # }
  # day and spt
  day = which(ts$SleepPeriodTime == 0)
  spt = which(ts$SleepPeriodTime == 1)
  # Initialize window summary
  # recording ID
  ws = matrix(id, nrow = nWindows, ncol = 1)
  colnames(ws)[1] = "ID"
  # date number
  if (window_type == "segments") {
    ws = cbind(ws, rep(w0:nDates, table(as.Date(t0))))
  } else {
    ws = cbind(ws, seq(from = w0, length.out = nrow(ws)))
  }
  colnames(ws)[ncol(ws)] = "window_number"
  # start and end times
  ws = cbind(ws, format(t0, "%Y-%m-%d"), format(t0, "%H:%M:%S"))
  colnames(ws)[(ncol(ws) - 1):ncol(ws)] = c("start_date", "start_time")
  ws = cbind(ws, format(t1, "%Y-%m-%d"), format(t1, "%H:%M:%S"))
  colnames(ws)[(ncol(ws) - 1):ncol(ws)] = c("end_date", "end_time")
  # start_end_window
  ws = cbind(ws, paste(format(t0, "%H:%M:%S"), format(t1, "%H:%M:%S"), sep = "-"))
  colnames(ws)[ncol(ws)] = "start_end_window"
  # segment names
  if (!is.null(qwindow_names)) {
    ws = cbind(ws, as.character(qwindow_names))
  } else {
    ws = cbind(ws, window_type)
  }
  colnames(ws)[ncol(ws)] = "window"
  ws = as.data.frame(ws)
  ws$window_number = as.numeric(ws$window_number)
  # TOTAL WINDOW ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC, time = ts$timenum,
                        windows = windows, levels = rep(1, nrow(ts)),
                        expected_levels = 1, levels_names = "day_spt",
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # DAY & SPT ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC, time = ts$timenum,
                        windows = windows, levels = ts$SleepPeriodTime,
                        expected_levels = 0:1, levels_names = c("day","spt"),
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # TOTAL LEVELS ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC[day], time = ts$timenum[day],
                        windows = windows[day], levels = levels[day],
                        expected_levels = c("IN", "LIG", "MOD", "VIG"), 
                        levels_names = c("IN", "LIG", "MOD", "VIG"),
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # CLASS_ID LEVELS ------------------
  for (i in 1:length(FUNs)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC, time = ts$timenum,
                        windows = windows, levels = ts$class_id,
                        expected_levels = legend$class_id, 
                        levels_names = legend$class_name,
                        FUN = FUNs[[i]],
                        preffix = paste0(names(FUNs)[i],"_",an,"_"))
  }
  # ORDER COLUMNS
  first =  c("ID",
             grep("^window", colnames(ws), value = T), 
             grep("^start", colnames(ws), value = T), 
             grep("^end", colnames(ws), value = T),
             grep("^segment", colnames(ws), value = T))
  addCols = grep(an, colnames(ws), value = T)
  ws = ws[, c(first, addCols)]
  # return
  return(ws)
}

