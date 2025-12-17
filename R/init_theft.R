#' Communicate to R the Python virtual environment containing the relevant libraries for calculating features
#' 
#' @importFrom reticulate use_virtualenv import
#'
#' @param venv \code{character} specifying the name of the to the Python virtual environment where \code{"tsfresh"}, \code{"TSFEL"}, and/or \code{"Kats"} are installed
#' @param hctsa \code{Boolean} denoting whether pyhctsa was installed or not. Defaults to \code{FALSE}
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test")
#' init_theft("theft-test")
#' }
#' 

init_theft <- function(venv, hctsa = FALSE){
  reticulate::use_virtualenv(venv)
  tsfresh <- reticulate::import("tsfresh")
  tsfel <- reticulate::import("tsfel")
  kats <- reticulate::import("kats")
  
  if(hctsa){
    pyhctsa <- reticulate::import("pyhctsa")
  }
}

#' Communicate to R the Python virtual environment containing tsfresh only
#' 
#' @importFrom reticulate use_virtualenv import
#'
#' @param venv \code{character} specifying the name of the to the Python virtual environment where \code{"tsfresh"} is installed
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test")
#' init_theft_tsfresh("theft-test")
#' }
#' 

init_theft_tsfresh <- function(venv){
  reticulate::use_virtualenv(venv)
  tsfresh <- reticulate::import("tsfresh")
}

#' Communicate to R the Python virtual environment containing tsfel only
#' 
#' @importFrom reticulate use_virtualenv import
#'
#' @param venv \code{character} specifying the name of the to the Python virtual environment where \code{"tsfel"} is installed
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test")
#' init_theft_tsfel("theft-test")
#' }
#' 

init_theft_tsfel <- function(venv){
  reticulate::use_virtualenv(venv)
  tsfel <- reticulate::import("tsfel")
}

#' Communicate to R the Python virtual environment containing kats only
#' 
#' @importFrom reticulate use_virtualenv import
#'
#' @param venv \code{character} specifying the name of the to the Python virtual environment where \code{"kats"} is installed
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test")
#' init_theft_kats("theft-test")
#' }
#' 

init_theft_kats <- function(venv){
  reticulate::use_virtualenv(venv)
  kats <- reticulate::import("kats")
}

#' Communicate to R the Python virtual environment containing pyhctsa only
#' 
#' @importFrom reticulate use_virtualenv import
#'
#' @param venv \code{character} specifying the name of the to the Python virtual environment where \code{"pyhctsa"} is installed
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test")
#' init_theft_hctsa("theft-test")
#' }
#' 

init_theft_hctsa <- function(venv){
  reticulate::use_virtualenv(venv)
  pyhctsa <- reticulate::import("pyhctsa")
}
