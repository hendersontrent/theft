#' Compute correlated t-statistic and p-value for resampled data from {correctR} package
#' 
#' @importFrom stats var pt
#' 
#' @param x \code{numeric} vector of values for model A
#' @param y \code{numeric} vector of values for model B
#' @param n \code{integer} denoting number of repeat samples. Defaults to \code{length(x)}
#' @param n1 \code{integer} denoting train set size
#' @param n2 \code{integer} denoting test set size
#' @return object of class \code{data.frame}
#' @references Nadeau, C., and Bengio, Y. Inference for the Generalization Error. Machine Learning 52, (2003).
#' @references Bouckaert, R. R., and Frank, E. Evaluating the Replicability of Significance Tests for Comparing Learning Algorithms. Advances in Knowledge Discovery and Data Mining. PAKDD 2004. Lecture Notes in Computer Science, 3056, (2004).
#' @author Trent Henderson
#'

resampled_ttest <- function(x, y, n, n1, n2){
  
  # Arg checks
  
  if(length(x) != length(y)){
    stop("x and y are not the same length.")
  }
  
  if(!is.numeric(x) || !is.numeric(y)){
    stop("x and y should be numeric vectors of the same length.")
  }
  
  if(!is.numeric(n) || !is.numeric(n1) || !is.numeric(n2) ||
     length(n) != 1 || length(n1) != 1 || length(n2) != 1){
    stop("n, n1, and n2 should all be integer scalars.")
  }
  
  if(missing(n) || is.null(n)){
    n <- length(x)
    message("n argument missing. Using length(x) as default.")
  }
  
  # Calculations
  
  d <- x - y # Calculate differences
  
  # Catch for when there is zero difference(s) between the models
  
  if (sum(d) == 0) {
    tmp <- data.frame(statistic = 0, p.value = 1)
  } else{
    statistic <- mean(d, na.rm = TRUE) / sqrt(stats::var(d, na.rm = TRUE) * (1/n + n2/n1)) # Calculate t-statistic
    
    if(statistic < 0){
      p.value <- stats::pt(statistic, n - 1) # p-value for left tail
    } else{
      p.value <- stats::pt(statistic, n - 1, lower.tail = FALSE) # p-value for right tail
    }
    
    tmp <- data.frame(statistic = statistic, p.value = p.value)
  }
  
  return(tmp)
}
