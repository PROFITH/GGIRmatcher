#' Combines Window-Level Output
#' 
#' @description
#' Takes the window-level output stored in the meta/ms5.out folder and combines
#' them into full window-level report that are stored in csv format in the
#' results folder.
#' 
#'
#' @param outputdir Pathname to folder to store the full window-level reports.
#' @param ... GGIR output parameters (for example, sep_reports and dec_reports can be used).
#' @param GGIR_output_dir Pathname to folder with the GGIR output.
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
#' @param minimum_WW_OO_length Minimum window length required to consider a valid window in hours for the WW and the OO window types.
#' @param minimum_segments_length Minimum window length required to consider a valid window for the segments of the day in hours.
#' @param verbose Logical indicating whether to print progress messages in the console.
#'
#' @return does not return anything. It stores reports in the results folder.
#' @export
#' @import data.table
#' @importFrom GGIR extract_params
#' @importFrom plyr rbind.fill
full_window_report = function(outputdir, GGIR_output_dir = NULL,
                              includecrit_day_spt = NULL,
                              includecrit_day = NULL,
                              includecrit_spt = NULL,
                              additional_sf_seconds = NULL,
                              minimum_WW_OO_length = 0,
                              minimum_segments_length = 0,
                              verbose = TRUE, ...) {
  
  # DISCLAIMER: -------------------------------------------------------------
  # Most of this function is copied from GGIR::g.report.part5
  # Additions here:
  #    - Allow to specify criteria for full window, day hours, and spt hours
  #    - Allow for definition of minimum WW and OO length
  #    - Extract indices for GGIR output and for Additional output
  #    - Only extracts full window-level report, including indices for valid windows
  # -------------------------------------------------------------------------
  input = list(...)
  # create output directory
  dir2save = file.path(outputdir, "GGIRmatcher", "results", "QC")
  suppressWarnings(dir.create(dir2save, recursive = T))
  # get parameters for cleaning GGIR output
  if (!is.null(GGIR_output_dir)) {
    configpath = file.path(GGIR_output_dir, "config.csv")
    configfile = ifelse(file.exists(configpath), configpath, NULL)
    params = GGIR::extract_params(input = input, configfile_csv = configfile)
    params_cleaning = params$params_cleaning
    params_output = params$params_output
  }
  # files
  fnames = dir(file.path(outputdir, "GGIRmatcher", "meta", "ms5.out"), full.names = T)
  if (is.null(input$f0)) f0 = 1 else f0 = input$f0
  if (is.null(input$f1)) f1 = length(fnames) else f1 = input$f1
  # load fun (from GGIR::g.report.part5)
  myfun = function(x, expectedCols = c()) {
    tail_expansion_log = last_timestamp = output = NULL
    load(file = x)
    cut = which(output[, 1] == "")
    if (length(cut) > 0 & length(cut) < nrow(output)) {
      output = output[-cut, which(colnames(output) != "")]
    }
    if (exists("last_timestamp") == TRUE) {
      output$lasttimestamp = as.numeric(format(last_timestamp, "%H"))
    } else {
      output$lasttimestamp = Inf # use dummy value
    }
    out = as.matrix(output)
    if (length(expectedCols) > 0) {
      tmp = as.data.frame(matrix(0, 0, length(expectedCols)))
      colnames(tmp) = expectedCols
      out = base::merge(tmp, out, all = TRUE)
    }
    if (!is.null(expectedCols) && ncol(out) > length(expectedCols)) {
      warning(paste0("Columns dropped in output part5 for ",
                     basename(x),
                     " because these could not be matched to columns for earlier",
                     " recordings in this dataset. Please check whether these recordings ",
                     " were processed with a",
                     " different GGIR version or configuration. If yes, reprocess",
                     " consistently. If not, ",
                     " consider renaming this file such that it is",
                     " alphabetically first and by that processed first, which",
                     " should address the issue."), call. = FALSE)
    }
    if (length(tail_expansion_log) != 0) {
      col2na = grep(pattern = paste0("sleep_efficiency|N_atleast5minwakenight|daysleeper|",
                                     "daysleeper|sleeplog_used|_spt_sleep|_spt_wake"),
                    x = names(out), value = FALSE)
      window_number = as.numeric(out[,"window_number"])
      lastwindow = which(window_number == max(window_number, na.rm = TRUE))
      if (length(col2na) > 0 & length(lastwindow) > 0) {
        out[lastwindow, col2na] = "" # set last row to NA for all sleep related variables
      }
    }
    return(out)
  }
  # get expected columns (copied from GGIR::g.report.part5)
  expectedCols = NULL
  set.seed(1234)
  # Create list of file indices to try
  testfiles = unique(sample(x = f0:f1, size = pmin(5, f1 - f0 + 1)))
  for (testf in testfiles) {
    out_try = myfun(fnames[testf])
    if (ncol(out_try) > length(expectedCols)) {
      # expectedCols should equal the column names of the milestone
      # file with the largest number of columns
      expectedCols = colnames(out_try) 
    }
  }
  # Rbind all files
  outputfinal = do.call(plyr::rbind.fill, lapply(fnames[f0:f1], myfun, expectedCols))
  outputfinal = as.data.frame(outputfinal, stringsAsFactors = FALSE)
  # Find columns filled with missing values
  cut = which(sapply(outputfinal, function(x) all(x == "")) == TRUE)
  if (length(cut) > 0) {
    outputfinal = outputfinal[,-cut]
  }
  # revise filename
  outputfinal$filename = gsub(".RData$", "", outputfinal$filename)
  outputfinal$filename = outputfinal$ID 
  # order data.frame
  outputfinal$window_number = as.numeric(gsub(" ", "", outputfinal$window_number))
  outputfinal = outputfinal[order(outputfinal$filename, outputfinal$window_number, outputfinal$window), ]
  # split results to different spreadsheets in order to minimize individual
  # filesize and to ease organising dataset
  uwi = as.character(unique(outputfinal$window))
  if (!all(uwi %in% c("MM", "WW", "OO"))) {
    uwi = c(uwi[uwi %in% c("MM", "WW", "OO")], "Segments")
  }
  uTRLi = as.character(unique(outputfinal$TRLi))
  if (any(is.na(uTRLi))) uTRLi = uTRLi[-which(is.na(uTRLi))]
  uTRMi = as.character(unique(outputfinal$TRMi))
  if (any(is.na(uTRMi))) uTRMi = uTRMi[-which(is.na(uTRMi))]
  uTRVi = as.character(unique(outputfinal$TRVi))
  if (any(is.na(uTRVi))) uTRVi = uTRVi[-which(is.na(uTRVi))]
  usleepparam = as.character(unique(outputfinal$sleepparam))
  if (any(is.na(usleepparam))) usleepparam = usleepparam[-which(is.na(usleepparam))]
  # replace NaN by empty cell value
  for (kra in 1:ncol(outputfinal)) {
    krad = which(is.nan(outputfinal[,kra]) == TRUE)
    if (length(krad) > 0) {
      outputfinal[krad,kra] = ""
    }
  }
  # outputfinal$daytype = 0
  # outputfinal$daytype[which(outputfinal$weekday == "Sunday" |
  #                             outputfinal$weekday == "Saturday")] = "WE"
  # outputfinal$daytype[which(outputfinal$weekday == "Monday" |
  #                             outputfinal$weekday == "Tuesday" |
  #                             outputfinal$weekday == "Wednesday" |
  #                             outputfinal$weekday == "Thursday" |
  #                             outputfinal$weekday == "Friday")] = "WD"
  outputfinal$nonwear_perc_day = as.numeric(outputfinal$nonwear_perc_day)
  outputfinal$nonwear_perc_spt = as.numeric(outputfinal$nonwear_perc_spt)
  outputfinal$dur_spt_min = as.numeric(outputfinal$dur_spt_min)
  outputfinal$dur_day_min = as.numeric(outputfinal$dur_day_min)
  outputfinal$guider = as.character(outputfinal$guider)
  outputfinal$sleeplog_used = as.numeric(outputfinal$sleeplog_used)
  outputfinal$dur_spt_min = as.numeric(outputfinal$dur_spt_min)
  outputfinal$dur_day_min = as.numeric(outputfinal$dur_day_min)
  outputfinal$dur_day_spt_min = as.numeric(outputfinal$dur_day_spt_min)
  # avoid NA in these variables to avoid issues when aggregating to day/person level
  # "weekday", "daytype", "boutcriter.in",
  # "boutcriter.lig", "boutcriter.mvpa",
  # "boutdur.in", "boutdur.lig", "boutdur.mvpa",
  # "GGIRversion"
  outputfinal$boutcriter.in = names(table(outputfinal$boutcriter.in))
  outputfinal$boutcriter.lig = names(table(outputfinal$boutcriter.lig))
  outputfinal$boutcriter.mvpa = names(table(outputfinal$boutcriter.mvpa))
  outputfinal$boutdur.in = names(table(outputfinal$boutdur.in))
  outputfinal$boutdur.lig = names(table(outputfinal$boutdur.lig))
  outputfinal$boutdur.mvpa = names(table(outputfinal$boutdur.mvpa))
  outputfinal$GGIRversion = names(table(outputfinal$GGIRversion))
  # loop to store various variants of the analysis separately
  outputfinal_bu = outputfinal
  if (verbose == TRUE) cat(" generating csv report for every parameter configurations...\n")
  for (j in 1:length(uwi)) {
    for (h1 in 1:length(uTRLi)) {
      for (h2 in 1:length(uTRMi)) {
        for (h3 in 1:length(uTRVi)) {
          for (h4 in 1:length(usleepparam)) {
            if (verbose == TRUE) {
              cat(paste0(" ", uwi[j], "-", uTRLi[h1], "-", uTRMi[h2],
                         "-", uTRVi[h3], "-", usleepparam[h4]))
            }
            select_window = as.character(outputfinal$window) == uwi[j]
            if (!(uwi[j] %in% c("MM", "WW", "OO"))) select_window = !(as.character(outputfinal$window) %in% c("MM", "WW", "OO"))
            seluwi = which(select_window &
                             as.character(outputfinal$TRLi) == uTRLi[h1] &
                             as.character(outputfinal$TRMi) == uTRMi[h2] &
                             as.character(outputfinal$TRVi) == uTRVi[h3] &
                             as.character(outputfinal$sleepparam) == usleepparam[h4])
            # store spreadsheet
            if (nrow(outputfinal[seluwi,]) == 0) {
              if (verbose == TRUE) cat("report not stored, because no results available")
            } else {
              CN = colnames(outputfinal)
              outputfinal2 = outputfinal
              colnames(outputfinal2) = CN
              delcol = grep(pattern = "TRLi|TRMi|TRVi|sleepparam",
                            x = colnames(outputfinal2))
              if (uwi[j] != "Segments") {
                delcol = c(delcol, which(colnames(outputfinal2) == "window"))
              }
              outputfinal2 = outputfinal2[,-delcol]
              OF3 = outputfinal2[seluwi,]
              OF3 = as.data.frame(OF3, stringsAsFactors = TRUE)
              # if any missing start_date (very unlikely but may happen in WW windows)
              # then, use ggir-defined date as start_date for that window.
              # Those windows would be excluded later on, this is only to avoid 
              # unnecesary errors.
              if (any(is.na(OF3$start_date))) {
                row = which(is.na(OF3$start_date))
                ggirdate = as.Date(OF3$calendar_date[row])
                OF3$start_date[row] = as.character(ggirdate)
                OF3$end_date[row] = as.character(ggirdate + 1)
                OF3$start_time[row] = substr(OF3$start_end_window[row], 1, 8)
                OF3$end_time[row] = substr(OF3$start_end_window[row], 10, 17)
              }
              wdays = weekdays(as.Date(4, "1970-01-01", tz = "GMT") + 0:6, abbreviate = F)
              if (uwi[j] %in% c("MM", "Segments", "WW")) OF3$weekday = weekdays(as.Date(OF3$start_date), abbreviate = F)
              if (uwi[j] == "OO") {
                from = as.POSIXlt(paste(OF3$start_date, OF3$start_time), tz = Sys.timezone())
                dates = lapply(from, FUN = function(x) ifelse(x$hour < 18, as.Date(x), as.Date(x) + 1))
                dates = as.Date(unlist(dates))
                OF3$weekday = weekdays(as.Date(dates), abbreviate = F)
              }
              OF3$daytype = ifelse(OF3$weekday %in% wdays[6:7], "WE",
                                   ifelse(OF3$weekday %in% wdays[1:5], "WD", NA))
              #-------------------------------------------------------------
              # if criteria for inclusion of additional output is null, 
              # then use GGIR cleaning parameters:
              # include crits
              if (is.null(includecrit_day)) includecrit_day = params_cleaning[["includedaycrit.part5"]]
              if (is.null(includecrit_day_spt)) includecrit_day_spt = params_cleaning[["includedaycrit"]]
              if (is.null(includecrit_spt)) includecrit_spt = params_cleaning[["includenightcrit"]]
              # minimum window lengths
              if (uwi[j] == "MM") min_window_length = params_cleaning[["minimum_MM_length.part5"]]
              if (uwi[j] == "WW" | uwi[j] == "OO") min_window_length = minimum_WW_OO_length 
              if (uwi[j] == "Segments") min_window_length = minimum_segments_length
              # extract indices
              OF3$GGIR_valid_day_spt = OF3$GGIR_valid_day = OF3$GGIR_valid_spt = FALSE
              OF3$additional_valid_day_spt = OF3$additional_valid_day = OF3$additional_valid_spt = FALSE
              validdaysi = getValidDayIndices(x = OF3, 
                                              includecrit_day_spt = includecrit_day_spt,
                                              includecrit_day = includecrit_day,
                                              includecrit_spt = includecrit_spt,
                                              additional_sf_seconds = additional_sf_seconds,
                                              min_window_length = min_window_length,
                                              params_output = params_output,
                                              params_cleaning = params_cleaning)
              OF3$GGIR_valid_day_spt[validdaysi$ggir_day_spt] = TRUE
              OF3$GGIR_valid_day[validdaysi$ggir_day] = TRUE
              OF3$GGIR_valid_spt[validdaysi$ggir_spt] = TRUE
              OF3$additional_valid_day_spt[validdaysi$add_day_spt] = TRUE
              OF3$additional_valid_day[validdaysi$add_day] = TRUE
              OF3$additional_valid_spt[validdaysi$add_spt] = TRUE
              # store all summaries in csv files 
              # all days
              OF3_clean = GGIR::tidyup_df(OF3)
              if ("lasttimestamp" %in% colnames(OF3_clean)) {
                OF3_clean = OF3_clean[, -which(colnames(OF3_clean) == "lasttimestamp")]
              }
              data.table::fwrite(
                OF3_clean,
                file.path(outputdir, "GGIRmatcher", "results", "QC",
                          paste("part5_daysummary_full_",
                                uwi[j], "_L", uTRLi[h1], "M", uTRMi[h2], "V", uTRVi[h3],
                                "_", usleepparam[h4], ".csv", sep = "")), row.names = FALSE, na = "",
                sep = params_output[["sep_reports"]],
                dec = params_output[["dec_reports"]])
            }
          }
        }
      }
    }
  }
}
