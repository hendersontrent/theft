#' Check for presence of NAs and non-numerics in a vector
#' @param x input data vector
#' @return Boolean of whether the data is good to extract features on or not
#' 

check_vector_quality <- function(x){
  return(!anyNA(x) && is.numeric(x))
}
