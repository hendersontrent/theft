#' Fit a neural network classifier
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom randomForest randomForest
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @return an object of the class of model that was fit
#' @author Trent Henderson
#' @references Stefan Fritsch, Frauke Guenther and Marvin N. Wright (2019). neuralnet: Training of Neural Networks. R package version 1.44.2. https://CRAN.R-project.org/package=neuralnet
#' @export
#' @examples
#' #' \dontrun{
#' }

fit_nn <- function(data, stepmax = 1e6){
  
  # Print prompt to normalise data
  
  message("Function assumes input data has been normalised. Please revise with sawlog::normalise_features() if your data is not normalised. This has large implications for model accuracy.")
  
  #------------------- Fit model ----------------
  
  message("Fitting model... This may take a long time.")
  
  mm <- as.formula(paste("group ~ ", paste(data[!data %in% c("group", "id")], collapse = " + ")))
  
  # Conditional hidden weights based on input data size
  # NOTE: Should these weights be different?
  # NOTE: Do thresholds need to be increased due to 'difficult' real data?
  
  if(nrow(data) <= 1000){
    m1 <- neuralnet::neuralnet(formula = mm, data = data, hidden = c(5), linear.output = FALSE, stepmax = stepmax)
  }
  
  if(nrow(data) > 1000){
    m1 <- neuralnet::neuralnet(formula = mm, data = data, hidden = c(3,2), linear.output = FALSE, stepmax = stepmax)
  }
  
  return(m1)
}
