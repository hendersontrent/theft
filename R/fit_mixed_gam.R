#' Fit a mixed-effects GAM classification model
#' @import dplyr
#' @importFrom magrittr %>%
#' @import mgcv
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @return an object of the class of model that was fit
#' @author Trent Henderson
#' @references Wood, S.N. (2011) Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36
#' @export
#' @examples
#' #' \dontrun{
#' }

fit_mixed_gam <- function(data){
  
  # Print prompt to normalise data
  
  message("Function assumes input data has been normalised. Please revise with sawlog::normalise_features() if your data is not normalised. This has large implications for model accuracy.")
  
  #------------------- Fit model ----------------
  
  message("Fitting model... This may take a long time.")
  
  mm <- as.formula(paste("group ~ ", paste(final[!final %in% c("group", "id")], collapse = " + ")))
  mm <- as.formula(paste(mm,"s(id, bs = 're')", collapse = " + ")) # Random effects for (1|id)
  
  m1 <- mgcv::gam(formula = mm, data = data, method = "REML", family = binomial("logit"))
  
  return(m1)
}
