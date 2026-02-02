#' Aggregates Time Series Per Window
#' `r lifecycle::badge("experimental")`
#' 
#' @description
#' Aggregates time series per window of interest, including midnight-to-midnight,
#' wakingup-to-wakingup, sleeponset-to-sleeponset, and user-defined segments
#' based on parameter \code{qwindow}.
#' 
#' @param qwindow Same as in GGIR, but it only supports integer vector for now. 
#' @param tz Character indicating the time zone to read the timestamps.
#' @param FUNs Named list of functions to be applied to the additional sensor data.
#' @param GGIRmatcher_outputdir Path to the directory where the output of GGIRmatcher should be stored.
#' @param qwindow_names Character vector with names to apply to the qwindows.
#' @param overwrite Logical indicating whether to overwrite previously-derived output.
#' @param GGIR_outputdir Path to folder where the GGIR output is stored.
#' @param verbose Logical indicating whether to print progress messages in the console.
#'
#' @return aggregated data frame.
#' @export
#' @import plyr
#'
aggregate_per_window = function(GGIRmatcher_outputdir, GGIR_outputdir,
                                FUNs = list(n = function(x) sum(!is.na(x)),
                                              mean = mean),
                                qwindow = NULL, qwindow_names = NULL,
                                overwrite = FALSE, 
                                tz = Sys.timezone(),
                                verbose = TRUE) {
  # safe checks in function object
  if (is.null(names(FUNs))) stop("Please provide names to the functions in the FUNs parameter list")
  # include function to count number of recordings in window
  FUNs = append(FUNs, c(n = function(x) sum(!is.na(x))), after = 0)
  # create directory to save daylevel output
  dir2save = file.path(GGIRmatcher_outputdir, "meta", "ms5.out")
  suppressWarnings(dir.create(dir2save, recursive = T))
  tspath = file.path(GGIRmatcher_outputdir, "meta", "ms5.outraw")
  files = dir(tspath, full.names = T)
  ggir_files = dir(file.path(GGIR_outputdir, "meta", "ms5.out"), full.names = T)
  output = NULL
  for (fi in 1:length(files)) {
    ts = cutpoints = legend = ggir_available = additional_available = NULL
    # overwrite?
    id = gsub(".RData$", "", basename(files[fi]))
    fn2save = file.path(dir2save, paste0(id, ".RData"))
    if (file.exists(fn2save) & overwrite == FALSE) next
    if (verbose) cat(paste0(fi, "-", id, " "))
    load(files[fi])
    # get indices for window definitions
    time = strptime(ts$timestamp, tz = tz, format = "%Y-%m-%dT%H:%M:%S%z")
    H = as.numeric(format(time, "%H"))
    M = as.numeric(format(time, "%M"))
    S = as.numeric(format(time, "%S"))
    midnights = which(H == 0 & M == 0 & S == 0)
    WU = which(diff(ts$SleepPeriodTime) == -1) + 1
    SO = which(diff(ts$SleepPeriodTime) == 1) + 1
    # aggregate per windows
    if (ggir_available == FALSE) cutpoints = NULL
    WS = analyseSegment(ts = ts, id = id, splits = midnights, FUNs = FUNs, 
                        lmv = cutpoints, window_type = "MM", legend = legend)
    if (ggir_available == TRUE) {
      if (length(WU) > 1) { #at least 2 wake ups
        WW = analyseSegment(ts = ts, id = id, splits = WU, FUNs = FUNs, 
                            lmv = cutpoints, window_type = "WW", legend = legend)
        WS = do.call(plyr::rbind.fill, list(WS, WW))
      }
      if (length(SO) > 1) { #at least 2 onsets
      OO = analyseSegment(ts = ts, id = id, splits = SO, FUNs = FUNs, 
                          lmv = cutpoints, window_type = "OO", legend = legend)
      WS = do.call(plyr::rbind.fill, list(WS, OO))
      }
    }
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
    # load GGIR output
    if (ggir_available == T) {
      ggir_fi = grep(id, ggir_files, value = T)
      output = NULL
      load(ggir_fi)
      output$ID = gsub(".gt3x$", "", output$ID)
    } else {
      output = NULL
    }
    # make consistent window_numbers between ggir output and additional output and merge
    output = fix_window_number(WS = WS, ggir_output = output, ts = ts)
    # order columns
    first = c("ID", "filename", "ggir_available", "additional_available",
              "window_number", "window", "calendar_date", "weekday", 
              "start_end_window", "start_date", "start_time", "end_date", "end_time")
    rest = colnames(output)[which(!colnames(output) %in% first)]
    output = output[, c(first, rest)]
    # including GGIR-required output
    tail_expansion_log = NULL
    last_timestamp = as.POSIXct(time[length(time)])
    save(output, tail_expansion_log, last_timestamp, 
         file = fn2save)
  }
}
