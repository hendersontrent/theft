#' Fit a support vector machine (SVM) classification model
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom e1071 svm
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @return an object of the class of model that was fit
#' @author Trent Henderson
#' @references David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel and Friedrich Leisch (2019). e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.7-3. https://CRAN.R-project.org/package=e1071
#' @export
#' @examples
#' #' \dontrun{
#' }

fit_svm <- function(data, kernel = 'radial'){
  
  # Print prompt to normalise data
  
  message("Function assumes input data has been normalised. Please revise with sawlog::normalise_features() if your data is not normalised. This has large implications for model accuracy.")
  
  #------------------- Fit model ----------------
  
  message("Fitting model... This may take a long time.")
  
  mm <- as.formula(paste("group ~ ", paste(data[!data %in% c("group", "id")], collapse = " + ")))
  
  m1 <- e1071::svm(formula = mm, data = data, kernel = kernel)
  
  return(m1)
}
