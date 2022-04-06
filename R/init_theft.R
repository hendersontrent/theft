#' Communicate to R the correct Python version containing the relevant libraries for calculating features
#' @importFrom reticulate use_python
#' @param path_to_python a string specifying the filepath to the version of Python containing the relevant libraries for calculating features
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' init_theft("~/opt/anaconda3/bin/python")
#' }
#' 

init_theft <- function(path_to_python){
  reticulate::use_python(path_to_python, required = TRUE)
}
