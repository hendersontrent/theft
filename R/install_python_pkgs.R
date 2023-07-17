#' Download and install all the relevant Python packages into a target location
#' 
#' @importFrom utils download.file unzip
#' 
#' @param python_path \code{character} specifying the filepath to the location of Python 3.9 on your machine
#' @param path \code{character} denoting the filepath to install the Python libraries and virtual environment to
#' @author Trent Henderson
#' @export
#' 

install_python_pkgs <- function(python_path, path){
  
  message("install_python_pkgs assumes Python 3.9 is installed on your machine.")
  stopifnot(is.character(python_path) || is.character(path))
  Sys.setenv(RETICULATE_PYTHON = python_path)
  kats_path <- paste0(path, "/Kats.zip")
  kats_path_short <- gsub(".zip", "\\1", kats_path)
  
  # Prepare folder structure and download stable version of Kats with modified dependency files to avoid errors
  
  link <- "https://github.com/hendersontrent/theft-python-libraries/raw/main/Kats.zip"
  utils::download.file(link, kats_path)
  utils::unzip(kats_path, exdir = path)
  
  # Run bash commands to install all Python side of things -- NOTE: assumes Python 3.9 is already installed
  
  system(paste(paste0("cd ", path), "&& python3.9 -m venv theft", "&& source theft/bin/activate",
               "&& pip install tsfresh", "&& pip install tsfel", "&& pip install pandas==1.5.3",
               paste0("&& pip install -e ", kats_path_short), sep = " "))
  
  message(paste0("All libraries installed. Virtual environment created is called theft at ", path, "/theft", 
                 "\nYou should be good to go!"))
}
