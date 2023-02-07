#' Check for presence of NAs and non-numerics in a vector
#' 
#' @param x input \code{vector}
#' @return \code{Boolean} of whether the data is good to extract features on or not
#' @author Trent Henderson
#' 

check_vector_quality <- function(x){
  return(!anyNA(x) && is.numeric(x))
}
