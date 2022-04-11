#' Check data quality of a vector
#' 
#' @param x input data vector
#' @return Boolean of whether the data is good to extract features on or not
#' @examples
#' \dontrun{
#' x <- stats::rnorm(x)
#' check_vector_quality(x)
#' }
#' 

check_vector_quality <- function(x){
  
  if(anyNA(x) || !is.numeric(x)){
    return(FALSE)
  } else{
    return(TRUE)
  }
}
