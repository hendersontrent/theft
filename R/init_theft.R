#' Communicate to R the Python virtual environment containing the relevant libraries for calculating features
#' @importFrom reticulate use_virtualenv
#' @param python_path \code{string} specifying the filepath to the version of Python you wish to use
#' @param venv_path \code{string} specifying the filepath to the Python virtual environment where "tsfresh", "tsfel", and/or "kats" are installed
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' 

init_theft <- function(python_path, venv_path){
  stopifnot(is.character(python_path) || is.character(venv_path))
  Sys.setenv(RETICULATE_PYTHON = python_path)
  reticulate::use_virtualenv(venv_path, required = TRUE)
}
