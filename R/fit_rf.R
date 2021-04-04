#' Fit a random forest classifier
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom randomForest randomForest
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @return an object of the class of model that was fit
#' @author Trent Henderson
#' @references A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.
#' @export
#' @examples
#' #' \dontrun{
#' }

fit_rf <- function(data, ntree = 100, importance = TRUE){
  
  # Print prompt to normalise data
  
  message("Function assumes input data has been normalised. Please revise with sawlog::normalise_features() if your data is not normalised. This has large implications for model accuracy.")
  
  #------------------- Fit model ----------------
  
  message("Fitting model... This may take a long time.")
  
  mm <- as.formula(paste("group ~ ", paste(data[!data %in% c("group", "id")], collapse = " + ")))
  
  m1 <- randomForest::randomForest(formula = mm, data = data, importance = importance, ntree = ntree)
  
  return(m1)
}
