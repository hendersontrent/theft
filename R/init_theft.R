#' Communicate to R the Python virtual environment containing the relevant libraries for calculating features
#' 
#' @importFrom reticulate use_virtualenv import
#'
#' @param venv \code{character} specifying the name of the to the Python virtual environment where \code{"tsfresh"}, \code{"TSFEL"}, and/or \code{"Kats"} are installed
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test")
#' init_theft("theft-test")
#' }
#' 

init_theft <- function(venv){
  reticulate::use_virtualenv(venv)
  tsfresh <- reticulate::import("tsfresh")
  TSFEL <- reticulate::import("TSFEL")
  Kats <- reticulate::import("kats")
}
