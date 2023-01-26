#' Check data quality of a vector
#' @param x input data vector
#' @return Boolean of whether the data is good to extract features on or not
#' @examples
#' x <- stats::rnorm(10)
#' check_vector_quality(x)
#' 

check_vector_quality <- function(x){
  return(!anyNA(x) && is.numeric(x))
}
