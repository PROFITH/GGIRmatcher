#' Creates Folder Structure for GGIRmatcher project
#' 
#' @description
#' This function creates the folder structure to a standard project that will
#' use the \code{GGIRmatcher} package. It will create a 3 folders: for the GGIR output,
#' for the additional sensor output, and for the matched output.
#'
#' @param outputdir Character (no default value) with the parent folder in which all the output generated will be stored (ideally an existing empty folder).
#' @param GGIR_dirname Character (default = "GGIR") with the name to be given to the folder that will store the GGIR output.
#' @param additional_dirname Character (default = "CGM") with the name to be given to the folder that will store the additional sensor output.
#' @param matched_dirname Character (default = "GGIRmatcher") with the name to be given to the folder that will store the matched output.
#' 
#' @return list including paths to the directories to be used for GGIR, additional sensor, and matched output.
#' @export
create_folder_structure = function(outputdir, 
                                   GGIR_dirname = "GGIR", 
                                   additional_dirname = "CGM",
                                   matched_dirname = "GGIRmatcher") {
  # safe check that outputdir exists
  if (!dir.exists(outputdir)) stop("Your defined output directory does not exist, please provide path to a existing (ideally empty) folder")
  # folder names
  GGIR_output_folder = file.path(outputdir, GGIR_dirname)
  matched_output_folder = file.path(outputdir, matched_dirname)
  additional_output_folder = file.path(outputdir, additional_dirname)
  # create directories
  if (!dir.exists(GGIR_output_folder)) dir.create(GGIR_output_folder)
  if (!dir.exists(additional_output_folder)) dir.create(additional_output_folder)
  if (!dir.exists(matched_output_folder)) dir.create(matched_output_folder)
  # return directories
  return(list(GGIR_ouputdir = GGIR_output_folder,
              additional_outputdir = additional_output_folder,
              GGIRmatcher_outputdir = matched_output_folder))
}