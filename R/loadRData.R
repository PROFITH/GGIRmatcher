#' Loads RData File with Assigned Name
#' @description
#' Loads RData file with single object and assigns the user-defined name to 
#' the loaded object. 
#'
#' @param fileName Path to the RData object to load.
#'
#' @return The content of the RData file under the assigned name
#' @export
#'
#' @examples 
#' \dontrun{
#' x = load("myfile.RData")
#' exists("x")
#' }
#' 
loadRData = function(fileName){
  #loads an RData file, and returns it
  mdat = NULL
  load(fileName)
  if (!is.null("mdat")) {
    ts = mdat
    rm(mdat)
    gc()
  } else {
    ts = get(ls()[ls() != "fileName"])
  }
  return(ts)
}
