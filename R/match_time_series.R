#' Matches GGIR Time Series with Other Additional Time Series
#' 
#' @description
#' Matches the GGIR exported time series in part 5 with other additional
#' user-provided time series at second level. 
#' 
#' @param GGIR_output_dir GGIR output directory (e.g., "output_myproject/"). This 
#' output directory is expected to contain only one single time-series subdirectory. 
#' GGIR generates a subdirectory for each combination of cut-points provided. If more
#' than one is found, only the first folder (in alphabetical order) will be used for now.
#' Also, files are expected to be stored in RData format. If you are interested in 
#' extending this feature to work with multiple thresholdn combinations, or with
#' other data formats, please contact the package maintainer.
#' @param additional_ts_dir Additional output directory (e.g., "my_other_sensor_timeseries/").
#' This is expected to contain an RData file per each recording. RData files should
#' contain a data frame (and only a data frame) with two columns: timestamp and the 
#' metric to include. 
#' @param idloc Character indicating how to find the id within the filename in the
#' time series. This is expected to match between GGIR file names and additional 
#' output directory file names. For example, if a GGIR time series file is named
#' "ID001_myproject_hipdata.RData" and \code{idloc = "_"}, then the extracted
#' ID would be "ID001".
#' @param tz Character indicating the time zone to read the timestamps.
#' @param outputdir Character with path to output directory to store the matched
#' time series and MM, WW, and OO reports.
#' @param add_metric_name Character indicating the name to be given to the new added metric.
#' @param verbose Logical indicating whether to print progress messages in console
#' @param overwrite Whether to overwrite the previously generated time series.
#'
#' @return Does not return any value, it stores RData files with merged time series
#' @export
#' 
#' @importFrom GGIR POSIXtime2iso8601 is.ISO8601
#'
match_time_series = function(GGIR_output_dir, additional_ts_dir,
                             outputdir, add_metric_name = NA,
                             idloc = "_", tz = Sys.timezone(),
                             overwrite = F,
                             verbose = T) {
  # create output directory
  suppressWarnings(
    dir.create(file.path(outputdir, "GGIRmatcher", "timeseries"),
               recursive = T)
  )
  
  # IDENTIFY DIRECTORIES ----------------------------------------------------
  # GGIR
  ggir_path = list.dirs(dir(file.path(GGIR_output_dir, "meta", "ms5.outraw"),
                            full.names = T))[1]
  legend_paths = dir(file.path(GGIR_output_dir, "meta", "ms5.outraw"),
                       full.names = T, pattern = "*.csv")
  legend_paths = grep("behavioralcodes", legend_paths, value = T)
  legend = read.csv(legend_paths[length(legend_paths)])
  if (is.na(ggir_path)) {
    stop("\nPath ", file.path(GGIR_output_dir, "meta", "ms5.outraw"), 
         " does not exist, or does not contain any subfolder with GGIR exported time series.", 
         call. = FALSE)
  } else {
    ggir_fnames = dir(ggir_path, pattern = "*.RData$")
    cutpoints = as.numeric(unlist(strsplit(basename(ggir_path), "_")))
    if (length(ggir_fnames) == 0) {
      stop(paste0("\nNo RData files found inside the expected GGIR time series directory: ", 
                  ggir_path), call. = FALSE)
    }
  }
  # Additional directory
  if (!dir.exists(additional_ts_dir)) {
    stop("\nPath ", additional_ts_dir, " does not exist.", 
         call. = FALSE)
  }
  add_fnames = dir(additional_ts_dir, pattern = "*.RData$")
  if (length(add_fnames) == 0) {
    stop(paste0("\nNo RData files found inside the expected additional time series directory: ", 
                additional_ts_dir), call. = FALSE)
  }
  # IDENTIFY IDs -------------------------------------------------------
  ggir_ids = unlist(lapply(ggir_fnames, function(x) unlist(strsplit(x, idloc, fixed = T))[1]))
  ggir_ids = gsub(".RData$", "", ggir_ids)
  add_ids = unlist(lapply(add_fnames, function(x) unlist(strsplit(x, idloc, fixed = T))[1]))
  add_ids = gsub(".RData$", "", add_ids)
  # is there any duplicate ID?
  ggir_dups = which(duplicated(ggir_ids))
  if (length(ggir_dups) > 0) {
    warning("\nGGIR IDs ", ggir_ids[ggir_dups], " are duplicated in the file names.
            Only the first file name with this ID will be used.", 
            call. = FALSE)
    ggir_ids = ggir_ids[-ggir_dups]
  }
  add_dups = which(duplicated(add_ids))
  if (length(add_dups) > 0) {
    warning("\nAdditional IDs ", add_ids[add_dups], " are duplicated in the file names.
            Only the first file name with this ID will be used.", 
            call. = FALSE)
    ggir_ids = add_ids[-add_dups]
  }
  # case report for available IDs
  all_ids = unique(c(ggir_ids, add_ids))
  CR = data.frame(ID = all_ids,
                  ggir_available = ifelse(all_ids %in% ggir_ids, 1, 0),
                  additional_available = ifelse(all_ids %in% add_ids, 1, 0))
  # IDENTIFY GGIR EPOCH -----------------------------------------------------
  ggfile1 = file.path(ggir_path, ggir_fnames[which(CR$ggir_available == 1)][1])
  gg4epoch = loadRData(ggfile1)
  epoch = diff(gg4epoch$timenum[1:2])
  # MERGE TIME SERIES -------------------------------------------------------
  # load files
  for (fi in 1:length(all_ids)) {
    id = all_ids[fi]
    fn2save = file.path(outputdir, "GGIRmatcher", "timeseries", paste0(id, ".RData"))
    if (file.exists(fn2save) & overwrite == FALSE) next
    if (verbose) cat(paste0(fi, "-", id, " "))
    ggirav = CR[which(CR$ID == id), "ggir_available"]
    addav = CR[which(CR$ID == id), "additional_available"]
    if (ggirav == 1) {
      f2load = file.path(ggir_path, ggir_fnames[which(ggir_ids == id)])
      gg = loadRData(f2load)
    }
    if (addav == 1) {
      f2load = file.path(additional_ts_dir, add_fnames[which(add_ids == id)])
      aa = loadRData(f2load)
      # impute timestamps
      t0 = GGIR::iso8601chartime2POSIX(aa$timestamp[1], tz = tz)
      t1 = GGIR::iso8601chartime2POSIX(aa$timestamp[nrow(aa)], tz = tz)
      time = seq.POSIXt(t0, t1, by = epoch)
      aa2 = data.frame(timestamp = GGIR::POSIXtime2iso8601(time, tz = tz))
      aa = merge(aa2, aa, by = "timestamp", all = T)
    } 
    # if not available....
    if (ggirav == 0) {
      gg = data.frame(timestamp = aa$timestamp,
                      ACC = NA, SleepPeriodTime = 0,    
                      invalidepoch = NA, guider = NA,
                      window = NA, angle = NA,
                      class_id = NA, invalid_fullwindow = NA, 
                      invalid_sleepperiod = NA,
                      invalid_wakinghours = NA, 
                      timenum = as.numeric(GGIR::iso8601chartime2POSIX(aa$timestamp, tz = tz)))
    }
    # GGIR time stamps
    if (!"timestamp" %in% colnames(gg)) {
      gg$timestamp = as.POSIXct(gg$timenum, tz = tz)
    }
    if (!GGIR::is.ISO8601(gg$timestamp[1])) {
      gg$timestamp = GGIR::POSIXtime2iso8601(gg$timestamp, tz = tz)
    }
    # additional metric
    if (addav == 0) {
      aa = data.frame(timestamp = gg$timestamp,
                      metric = NA)
    }
    if (!is.na(add_metric_name)) {
      colnames(aa)[grep("timestamp", colnames(aa), invert = T)] = add_metric_name
    }
    ggir_available = ggirav
    additional_available = addav
    # Additional time stamps
    if (!GGIR::is.ISO8601(aa$timestamp[1])) stop("\nTime stamps in additional file should be in iso8601 format for now.")
    # merge
    ts = merge(aa, gg, by = "timestamp", all.x = F, all.y = T)
    ts = ts[, c("timestamp", "timenum", 
                grep("^time", colnames(ts), invert = T, value = T))]
    # save
    save(ts, id, legend, cutpoints, additional_available, ggir_available, file = fn2save)
    # remove files and clean memory before next iteration
    rm(gg, aa, ts)
    gc()
  }
}