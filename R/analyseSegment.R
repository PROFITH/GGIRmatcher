#' Title
#'
#' @param ts 
#' @param splits 
#' @param id 
#' @param legend 
#' @param lmv 
#' @param addFUN 
#' @param addCutpoints 
#' @param segment_names 
#'
#' @return aggregated data frame.
#' @export
#' @import plyr
analyseSegment = function(ts, id, splits, addFUN,
                          addCutpoints = NULL,
                          segment_names = NULL,
                          lmv = c(30, 100, 430),
                          legend) {
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
  # add partial first and last window
  if (splits[1] > 1) {
    windows = c(rep(0, splits[1] - 1), windows)
    st = c(1, st)
    if (!is.null(segment_names)) {
      which_level = which(levels(segment_names) == segment_names[1]) - 1
      lvsegments = levels(segment_names)
      if (which_level == 0) which_level = length(lvsegments)
      segment_names = c(lvsegments[which_level], as.character(segment_names))
      segment_names = factor(segment_names, levels = lvsegments)
    }
  }
  if (nrow(ts) > max(splits)) {
    windows = c(windows, rep(max(windows) + 1, nrow(ts) - max(splits) + 1))
    end = c(end, nrow(ts))
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
  ndates = length(unique(as.Date(t0)))
  # day and spt
  day = which(ts$SleepPeriodTime == 0)
  spt = which(ts$SleepPeriodTime == 1)
  # Initialize window summary
  # recording ID
  ws = matrix(id, nrow = nWindows, ncol = 1)
  colnames(ws)[1] = "ID"
  # window indicators
  ws = cbind(ws, unique(windows))
  colnames(ws)[ncol(ws)] = "window_number"
  # date number
  ws = cbind(ws, rep(1:ndates, times = table(as.Date(t0))))
  colnames(ws)[ncol(ws)] = "date_number"
  # start and end times
  ws = cbind(ws, format(t0, "%Y-%m-%d"), format(t0, "%H:%M:%S"))
  colnames(ws)[(ncol(ws) - 1):ncol(ws)] = c("start_date", "start_time")
  ws = cbind(ws, format(t1, "%Y-%m-%d"), format(t1, "%H:%M:%S"))
  colnames(ws)[(ncol(ws) - 1):ncol(ws)] = c("end_date", "end_time")
  # segment names
  if (!is.null(segment_names)) {
    ws = cbind(ws, as.character(segment_names))
    colnames(ws)[ncol(ws)] = "segment_name"
  }
  ws = as.data.frame(ws)
  ws$window_number = as.numeric(ws$window_number)
  ws$date_number = as.numeric(ws$date_number)
  if (!is.null(segment_names)) {
    ws$segment_name = factor(ws$segment_name, levels = lvsegments)
  }
  # TOTAL WINDOW ------------------
  # time in window
  ws = agg_per_levels(ws = ws, x = rep(1, nrow(ts)), 
                      windows = windows, levels = rep(1, nrow(ts)),
                      expected_levels = 1, levels_names = "day_spt",
                      FUN = function(x) mincounter(x, epoch = epoch),
                      preffix = "dur_", suffix = "_min")
  # valid acc time in window
  ws = agg_per_levels(ws = ws, x = ts$invalidepoch == 0, 
                      windows = windows, levels = rep(1, nrow(ts)),
                      expected_levels = 1, levels_names = c("day_spt_valid"),
                      FUN = function(x) mincounter(x, epoch = epoch),
                      preffix = "dur_", suffix = "_min")
  # mean ACC in window
  ws = agg_per_levels(ws = ws, x = ts$ACC, 
                      windows = windows, levels = rep(1, nrow(ts)),
                      expected_levels = 1, levels_names = "day_spt",
                      FUN = function(x) mean(x, na.rm = T),
                      preffix = "ACC_", suffix = "_mg")
  # additional metric addFUN
  for (i in 1:length(addFUN)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC, time = ts$timenum,
                        windows = windows, levels = rep(1, nrow(ts)),
                        expected_levels = 1, levels_names = "day_spt",
                        FUN = addFUN[[i]],
                        preffix = paste0(names(addFUN)[i],"_",an,"_"))
  }
  # DAY & SPT ------------------
  # time in windows
  ws = agg_per_levels(ws = ws, x = rep(1, nrow(ts)), 
                      windows = windows, levels = ts$SleepPeriodTime,
                      expected_levels = 0:1, levels_names = c("day","spt"),
                      FUN = function(x) mincounter(x, epoch = epoch),
                      preffix = "dur_", suffix = "_min")
  # valid acc time in window
  ws = agg_per_levels(ws = ws, x = ts$invalidepoch == 0, 
                      windows = windows, levels = ts$SleepPeriodTime,
                      expected_levels = 0:1, levels_names = c("day_valid","spt_valid"),
                      FUN = function(x) mincounter(x, epoch = epoch),
                      preffix = "dur_", suffix = "_min")
  # mean ACC in window
  ws = agg_per_levels(ws = ws, x = ts$ACC, 
                      windows = windows, levels = ts$SleepPeriodTime,
                      expected_levels = 0:1, levels_names = c("day","spt"),
                      FUN = function(x) mean(x, na.rm = T),
                      preffix = "ACC_", suffix = "_mg")
  # additional metric addFUN
  for (i in 1:length(addFUN)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC, time = ts$timenum,
                        windows = windows, levels = ts$SleepPeriodTime,
                        expected_levels = 0:1, levels_names = c("day","spt"),
                        FUN = addFUN[[i]],
                        preffix = paste0(names(addFUN)[i],"_",an,"_"))
  }
  # TOTAL LEVELS ------------------
  # time in windows
  ws = agg_per_levels(ws = ws, x = ts$SleepPeriodTime == 0, 
                      windows = windows, levels = levels,
                      expected_levels = c("IN", "LIG", "MOD", "VIG"), 
                      levels_names = c("IN", "LIG", "MOD", "VIG"),
                      FUN = function(x) mincounter(x, epoch = epoch),
                      preffix = "dur_day_total_", suffix = "_min")
  # mean ACC in window
  ws = agg_per_levels(ws = ws, x = ts$ACC[day], 
                      windows = windows[day], levels = levels[day],
                      expected_levels = c("IN", "LIG", "MOD", "VIG"), 
                      levels_names = c("IN", "LIG", "MOD", "VIG"),
                      FUN = function(x) mean(x, na.rm = T),
                      preffix = "ACC_day_total_", suffix = "_mg")
  # additional metric addFUN
  for (i in 1:length(addFUN)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC[day], time = ts$timenum[day],
                        windows = windows[day], levels = levels[day],
                        expected_levels = c("IN", "LIG", "MOD", "VIG"), 
                        levels_names = c("IN", "LIG", "MOD", "VIG"),
                        FUN = addFUN[[i]],
                        preffix = paste0(names(addFUN)[i],"_",an,"_"))
  }
  # CLASS_ID LEVELS ------------------
  # Time in windows
  ws = agg_per_levels(ws = ws, x = rep(1, nrow(ts)), 
                      windows = windows, levels = ts$class_id,
                      expected_levels = legend$class_id, 
                      levels_names = legend$class_name,
                      FUN = function(x) mincounter(x, epoch = epoch),
                      preffix = "dur_", suffix = "_min")
  # mean ACC in windows
  ws = agg_per_levels(ws = ws, x = ts$ACC, 
                      windows = windows, levels = ts$class_id,
                      expected_levels = legend$class_id, 
                      levels_names = legend$class_name,
                      FUN = function(x) mean(x, na.rm = T),
                      preffix = "ACC_", suffix = "_mg")
  # additional metric addFUN
  for (i in 1:length(addFUN)) {
    ws = agg_per_levels(ws = ws, x = ts$GLUC, time = ts$timenum,
                        windows = windows, levels = ts$class_id,
                        expected_levels = legend$class_id, 
                        levels_names = legend$class_name,
                        FUN = addFUN[[i]],
                        preffix = paste0(names(addFUN)[i],"_",an,"_"))
  }
  # sleep efficiency and WASO
  ws$sleep_efficiency = ws$dur_spt_sleep_min / ws$dur_spt_min * 100
  ws$WASO = rowSums(ws[,grep("spt_wake", colnames(ws))], na.rm = T)
  # ORDER COLUMNS
  first =  c("ID", "date_number", "window_number", 
             grep("^start", colnames(ws), value = T), 
             grep("^end", colnames(ws), value = T),
             grep("^segment", colnames(ws), value = T))
  durCols = grep("^dur", colnames(ws), value = T)
  accCols = grep("^ACC", colnames(ws), value = T)
  addCols = grep(an, colnames(ws), value = T)
  last = c("sleep_efficiency", "WASO")
  ws = ws[, c(first, durCols, accCols, addCols, last)]
  # return
  return(ws)
}
  
