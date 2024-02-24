#' Download and install all the relevant Python packages into a target location
#' 
#' @importFrom utils download.file unzip
#' @importFrom reticulate virtualenv_create virtualenv_install virtualenv_root
#' 
#' @param venv \code{character} specifying the name of the new virtual environment to create
#' @param simple_kats \code{Boolean} denoting whether to try a standard installation of \code{Kats} from PyPI using \code{reticulate::virtualenv_install} or to install a safer version with less dependencies. Defaults to \code{TRUE}
#' @author Trent Henderson
#' @export
#' 

install_python_pkgs <- function(venv, simple_kats = TRUE){
  reticulate::virtualenv_create(venv)
  reticulate::virtualenv_install(venv, "tsfresh")
  reticulate::virtualenv_install(venv, "TSFEL")
  
  if(simple_kats){
    reticulate::virtualenv_install(venv, "TSFEL")
  } else{
    reticulate::use_virtualenv(venv)
    utils::download.file("https://github.com/hendersontrent/theft-python-libraries/raw/main/Kats.zip", paste0(reticulate::virtualenv_root(), "/", venv, "/Kats.zip"))
    utils::unzip(paste0(reticulate::virtualenv_root(), "/", venv, "/Kats.zip"), exdir = paste0(reticulate::virtualenv_root(), "/", venv))
    
    system(paste(paste0("cd ", normalizePath(paste0(reticulate::virtualenv_root(), "/", venv, "/Kats"))),
                 paste0("&& python", reticulate::py_version(), " -m venv ", venv),
                 paste0("&& source ", venv, "/bin/activate"),
                 paste0("&& pip install -e ", normalizePath(paste0(reticulate::virtualenv_root(), "/", venv, "/Kats"))), sep = " "))
    
    # tryCatch({
    #   reticulate::virtualenv_install(venv, "kats")
    # },
    # error = function(e){
    #   reticulate::use_virtualenv(venv)
    #   utils::download.file("https://github.com/hendersontrent/theft-python-libraries/raw/main/Kats.zip", paste0(reticulate::virtualenv_root(), "/", venv, "/Kats.zip"))
    #   utils::unzip(paste0(reticulate::virtualenv_root(), "/", venv, "/Kats.zip"), exdir = paste0(reticulate::virtualenv_root(), "/", venv))
    #   
    #   system(paste(paste0("cd ", normalizePath(paste0(reticulate::virtualenv_root(), "/", venv, "/Kats"))), 
    #                paste0("&& python", reticulate::py_version(), " -m venv ", venv), 
    #                paste0("&& source ", venv, "/bin/activate"),
    #                paste0("&& pip install -e ", normalizePath(paste0(reticulate::virtualenv_root(), "/", venv, "/Kats"))), sep = " "))
    # })
  }
}
