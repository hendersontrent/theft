#' Download and install all the relevant Python packages
#' 
#' @author Trent Henderson
#' @export
#' 

install_python_pkgs <- function(){
  
  message("install_python_pkgs assumed Python 3.9 is installed on your machine.")
  
  # Prepare folder structure and download stable version of Kats to avoid errors
  
  if(!dir.exists(paste0(system.file(package = "theft"), "/Python"))) dir.create(paste0(system.file(package = "theft"), "/Python"))
  link <- "https://minhaskamal.github.io/DownGit/#/home?url=https://github.com/hendersontrent/theft-python-libraries/tree/main/Kats"
  download.file(link, paste0(system.file(package = "theft"), "/Python"))
  
  # Run bash commands to install all Python side of things -- NOTE: assumes Python 3.9 is already installed
  
  system(paste0("cd ", paste0(system.file(package = "theft"), "/Python")))
  system("python3.9 -m venv theft")
  system("sourcetheft/bin/activate")
  system("pip install tsfresh")
  system("pip install tsfel")
  system("pip install pandas==1.5.3")
  system(paste0("pip install -e ", paste0(system.file(package = "theft"), "/Python/Kats")))
  
  message("All libraries installed. You should be good to go!")
}