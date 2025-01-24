#' Download and install tsfresh, TSFEL, and Kats from Python into a new virtual environment
#'
#' @importFrom reticulate virtualenv_create virtualenv_install
#' 
#' @param venv \code{character} specifying the name of the new virtual environment to create
#' @param python \code{character} specifying the filepath to the Python interpreter to use. Python 3.10 is recommended
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_python_pkgs("theft-test", "/usr/local/bin/python3.10")
#' }
#' 
install_python_pkgs <- function(venv, python){
  reticulate::virtualenv_create(venv, python)
  reticulate::virtualenv_install(venv, "tsfel")
  reticulate::virtualenv_install(venv, "tsfresh")
  reticulate::virtualenv_install(venv, "git+https://github.com/hendersontrent/theft-kats.git")
  reticulate::virtualenv_install(venv, "scipy==1.14.0")
  reticulate::virtualenv_install(venv, "packaging==21.3")
  reticulate::virtualenv_install(venv, "numpy==1.26")
}

#' Download and install tsfresh from Python into a new virtual environment
#'
#' @importFrom reticulate virtualenv_create virtualenv_install
#' 
#' @param venv \code{character} specifying the name of the new virtual environment to create
#' @param python \code{character} specifying the filepath to the Python interpreter to use. Python 3.10 is recommended
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_tsfresh("theft-test", "/usr/local/bin/python3.10")
#' }
#' 
install_tsfresh <- function(venv, python){
  reticulate::virtualenv_create(venv, python)
  reticulate::virtualenv_install(venv, "tsfresh")
  reticulate::virtualenv_install(venv, "scipy==1.14.0")
  reticulate::virtualenv_install(venv, "packaging==21.3")
  reticulate::virtualenv_install(venv, "numpy==1.26")
}

#' Download and install TSFEL from Python into a new virtual environment
#'
#' @importFrom reticulate virtualenv_create virtualenv_install
#' 
#' @param venv \code{character} specifying the name of the new virtual environment to create
#' @param python \code{character} specifying the filepath to the Python interpreter to use. Python 3.10 is recommended
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_tsfel("theft-test", "/usr/local/bin/python3.10")
#' }
#' 
install_tsfel <- function(venv, python){
  reticulate::virtualenv_create(venv, python)
  reticulate::virtualenv_install(venv, "tsfel")
  reticulate::virtualenv_install(venv, "scipy==1.14.0")
  reticulate::virtualenv_install(venv, "packaging==21.3")
  reticulate::virtualenv_install(venv, "numpy==1.26")
}

#' Download and install Kats from Python into a new virtual environment
#'
#' @importFrom reticulate virtualenv_create virtualenv_install
#' 
#' @param venv \code{character} specifying the name of the new virtual environment to create
#' @param python \code{character} specifying the filepath to the Python interpreter to use. Python 3.10 is recommended
#' @return no return value; called for side effects
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' install_kats("theft-test", "/usr/local/bin/python3.10")
#' }
#' 
install_kats <- function(venv, python){
  reticulate::virtualenv_create(venv, python)
  reticulate::virtualenv_install(venv, "git+https://github.com/hendersontrent/theft-kats.git")
  reticulate::virtualenv_install(venv, "scipy==1.14.0")
  reticulate::virtualenv_install(venv, "packaging==21.3")
  reticulate::virtualenv_install(venv, "numpy==1.26")
}
