#' Aggregates Time Series Per Window
#' 
#' @description
#' Aggregates time series per window of interest, including midnight-to-midnight,
#' wakingup-to-wakingup, sleeponset-to-sleeponset, and user-defined segments
#' based on parameter \code{hour_splits}.
#' 
#' @param tspath path to RData files containing the matched time series as done with 
#' \code{match_time_series}
#' @param qwindow Numeric vector with the hours in the day for which extract
#' @param tz Character indicating the time zone to read the timestamps.
#' @param FUNs 
#' @param outputdir 
#' @param qwindow_names 
#' @param overwrite 
#' @param verbose 
#' @param GGIR_output_dir 
#' segments. Only integers allowed for now.
#' @return aggregated data frame.
#' @export
#' @import plyr
#'
aggregate_per_window = function(tspath, outputdir, GGIR_output_dir,
                                FUNs = list(n = function(x) sum(!is.na(x)),
                                              mean = mean),
                                qwindow = NULL, qwindow_names = NULL,
                                overwrite = FALSE, 
                                tz = Sys.timezone(),
                                verbose = TRUE) {
  # create directory to save daylevel output
  suppressWarnings(
    dir.create(file.path(outputdir, "GGIRmatcher", "daylevel"), recursive = T)
  )
  files = dir(tspath, full.names = T)
  ggir_files = dir(file.path(GGIR_output_dir, "meta", "ms5.out"), full.names = T)
  output = NULL
  for (fi in 1:length(files)) {
    id = ts = cutpoints = legend = ggir_available = additional_available = NULL
    load(files[fi])
    fn2save = file.path(outputdir, "GGIRmatcher", "daylevel", paste0(id, ".RData"))
    if (file.exists(fn2save) & overwrite == FALSE) next
    if (verbose) cat(paste0(fi, "-", id, " "))
    
    if (is.null(id)) id = gsub(".RData$", "", basename(files[fi]))
    # get indices for window definitions
    time = strptime(ts$timestamp, tz = tz, format = "%Y-%m-%dT%H:%M:%S%z")
    H = as.numeric(format(time, "%H"))
    M = as.numeric(format(time, "%M"))
    S = as.numeric(format(time, "%S"))
    midnights = which(H == 0 & M == 0 & S == 0)
    WU = which(diff(ts$SleepPeriodTime) == -1) + 1
    SO = which(diff(ts$SleepPeriodTime) == 1) + 1
    # aggregate per windows
    MM = analyseSegment(ts = ts, id = id, splits = midnights, FUNs = FUNs, 
                        lmv = cutpoints, window_type = "MM", legend = legend)
    WW = analyseSegment(ts = ts, id = id, splits = WU, FUNs = FUNs, 
                        lmv = cutpoints, window_type = "WW", legend = legend)
    OO = analyseSegment(ts = ts, id = id, splits = SO, FUNs = FUNs, 
                        lmv = cutpoints, window_type = "OO", legend = legend)
    # combine
    WS = do.call(plyr::rbind.fill, list(MM, WW, OO))
    # segments
    if (length(qwindow) > 0) {
      segments = which(H %in% qwindow & M == 0 & S == 0)
      # segment names
      if (is.null(qwindow_names)) {
        qwindow_names = paste0("segment", 1:length(qwindow))
      }
      qwindow_names_all = factor(H[segments], levels = qwindow,
                                 labels = qwindow_names)
      SS = analyseSegment(ts = ts, id = id, splits = segments,
                          qwindow_names = qwindow_names_all,
                          FUNs = FUNs, lmv = cutpoints, legend = legend)
      WS = plyr::rbind.fill(SS, WS)
    }
    # ORDER COLUMNS
    cid = which(colnames(WS) %in% c("ID", "window_number", "window", "start_end_window"))
    WS = WS[, c(cid, which(!1:ncol(WS) %in% c(cid)))]
    # merge with GGIR
    ggir_fi = grep(id, ggir_files, value = T)
    load(ggir_fi)
    output = merge(output, WS, by = c("ID", "window_number", "window", "start_end_window"), all = T)
    # save
    save(output, file = fn2save)
  }
}