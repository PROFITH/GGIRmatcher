#' Aggregates Time Series Per Window
#' 
#' @description
#' Aggregates time series per window of interest, including midnight-to-midnight,
#' wakingup-to-wakingup, sleeponset-to-sleeponset, and user-defined segments
#' based on parameter \code{hour_splits}.
#' 
#' @param tspath path to RData files containing the matched time series as done with 
#' \code{match_time_series}
#' @param hour_splits Numeric vector with the hours in the day for which extract
#' @param tz Character indicating the time zone to read the timestamps.
#' @param addFUN 
#' @param GGIR_cutpoints 
#' @param class_legend 
#' @param outputdir 
#' @param segment_names 
#' @param overwrite 
#' @param verbose 
#' segments. Only integers allowed for now.
#' @return aggregated data frame.
#' @export
#' @import plyr
#'
aggregate_per_window = function(tspath, outputdir,
                                addFUN = list(n = function(x) sum(!is.na(x)),
                                              mean = mean),
                                hour_splits = NULL, segment_names = NULL,
                                GGIR_cutpoints = NULL, class_legend = NULL,
                                overwrite = FALSE, 
                                tz = Sys.timezone(),
                                verbose = TRUE) {
  # create directory to save daylevel output
  suppressWarnings(
    dir.create(file.path(outputdir, "GGIRmatcher", "daylevel"), recursive = T)
  )
  # GGIR classes legend
  if (!is.null(class_legend)) {
    legend = read.csv(class_legend)
  } else {
    stop("\nGGIR classes legend not provided.", call. = F)
  }
  files = dir(tspath, full.names = T)
  for (fi in 1:length(files)) {
    id = ts = cutpoints = NULL
    load(files[fi])
    fn2save = file.path(outputdir, "GGIRmatcher", "daylevel", paste0(id, ".RData"))
    if (file.exists(fn2save) & overwrite == FALSE) next
    if (verbose) cat(paste0(fi, "-", id, " "))
    
    if (is.null(id)) id = gsub(".RData$", "", basename(files[fi]))
    if (is.null(GGIR_cutpoints)) {
      GGIR_cutpoints = cutpoints
    } else if (!is.null(cutpoints)) {
      if (GGIR_cutpoints != cutpoints) {
        warning("\nUser defined GGIR_cutpoints are different to the cutpoints",
                "actually used in GGIR.", cutpoints, "cutpoints for light, moderate, and
                      vigorous are used.", call. = FALSE)
        GGIR_cutpoints = cutpoints
      }
    }
    # get indices for window definitions
    time = strptime(ts$timestamp, tz = tz, format = "%Y-%m-%dT%H:%M:%S%z")
    H = as.numeric(format(time, "%H"))
    M = as.numeric(format(time, "%M"))
    S = as.numeric(format(time, "%S"))
    midnights = which(H == 0 & M == 0 & S == 0)
    WU = which(diff(ts$SleepPeriodTime) == -1) + 1
    SO = which(diff(ts$SleepPeriodTime) == 1) + 1
    # aggregate per windows
    MM = analyseSegment(ts = ts, id = id, splits = midnights, 
                        addFUN = addFUN, 
                        lmv = GGIR_cutpoints, legend = legend)
    MM$window_type = "MM"
    WW = analyseSegment(ts = ts, id = id, splits = WU, 
                        addFUN = addFUN, 
                        lmv = GGIR_cutpoints, legend = legend)
    WW$window_type = "WW"
    OO = analyseSegment(ts = ts, id = id, splits = SO, 
                        addFUN = addFUN, 
                        lmv = GGIR_cutpoints, legend = legend)
    OO$window_type = "OO"
    # combine
    WS = do.call(plyr::rbind.fill, list(MM, WW, OO))
    # segments
    if (length(hour_splits) > 0) {
      segments = which(H %in% hour_splits & M == 0 & S == 0)
      # segment names
      if (is.null(segment_names)) {
        segment_names = paste0("segment", 1:length(hour_splits))
      }
      segment_names_all = factor(H[segments], levels = hour_splits,
                                 labels = segment_names)
      SS = analyseSegment(ts = ts, id = id, splits = segments,
                          segment_names = segment_names_all,
                          addFUN = addFUN, 
                          lmv = GGIR_cutpoints, legend = legend)
      SS$window_type = "segments"
      WS = plyr::rbind.fill(SS, WS)
    }
    # ORDER COLUMNS
    cid = which(colnames(WS) == "ID")
    ctw = which(colnames(WS) == "window_type")
    csegment = which(colnames(WS) == "segment_name")
    WS = WS[, c(cid, ctw, csegment, which(!1:ncol(WS) %in% c(cid, ctw, csegment)))]
    # save
    save(WS, file = fn2save)
  }
  
  
}