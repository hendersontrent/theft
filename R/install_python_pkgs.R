#' Download and install all the relevant Python packages into a target location
#' 
#' @importFrom utils download.file unzip
#' @importFrom reticulate virtualenv_create virtualenv_install use_virtualenv
#' 
#' @param venv \code{character} specifying the name of the new virtual environment to create
#' @param standard_kats \code{Boolean} denoting whether to try a standard installation of \code{Kats} from PyPI using \code{reticulate::virtualenv_install} or to install a safer version with less dependencies. Defaults to \code{TRUE}
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test")
#' }
#' 

install_python_pkgs <- function(venv, standard_kats = TRUE){
  reticulate::virtualenv_create(venv)
  reticulate::virtualenv_install(venv, "tsfresh")
  reticulate::virtualenv_install(venv, "TSFEL")
  
  if(standard_kats){
    reticulate::virtualenv_install(venv, "kats")
  } else{
    reticulate::use_virtualenv(venv)
    system("pip install git+git://github.com/hendersontrent/theft-python-libraries.git@main")
  }
}
