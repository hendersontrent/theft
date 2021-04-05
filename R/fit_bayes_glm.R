#' Fit a Bayesian logistic regression model
#' @import dplyr
#' @importFrom magrittr %>%
#' @import rstan
#' @importFrom parallel detectCores
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @param iter the number of iterations to simulate per chain. Defaults to 3000
#' @param chains the number of Markov chains to fit. Defaults to 3
#' @param max_treedepth the maximum tree depth for the HMC algorithm. Defaults to 10
#' @return an object of the class of model that was fit
#' @author Trent Henderson
#' @references Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. http://mc-stan.org/.
#' @export
#' @examples
#' #' \dontrun{
#' }

fit_bayes_glm <- function(data, id_var = NULL, group_var = NULL, iter = 3000, chains = 3, max_treedepth = 10){
  
  # Argument checks
  
  if(!is.numeric(eps)){
    stop("eps should be a numeric value. Defaults to 1e6")
  }
  
  if(!is.null(id_var) & !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  if(!is.null(group_var) & !is.character(group_var)){
    stop("group_var should be a string specifying a variable in the input data that identifies the group membership variable/column.")
  }
  
  # Print prompt to normalise data
  
  message("Function assumes input data has been normalised. Please revise with sawlog::normalise_features() if your data is not normalised. This has large implications for model accuracy.")
  
  #------------------- Fit model ----------------
  
  options(mc.cores = parallel::detectCores()) # Parallel processing
  
  # Set up data for Stan
  
  name_list <- as.vector(colnames(final))
  name_list <- name_list[!name_list %in% c("group", "id")]
  
  X <- data %>%
    dplyr::select(-c(id, group))
  
  stan_data <- list(N = nrow(final),
                    K = as.integer(length(name_list)),
                    y = final$group,
                    X = as.matrix(X))
  
  # Run model
  
  message("Fitting model... This may take a long time.")
  
  m1 <- rstan::stan(file = system.file("stan", "BayesGLM.stan", package = "sawlog"), 
                    data = stan_data, iter = iter, chains = chains, seed = 123, control = list(max_treedepth = max_treedepth))
  
  return(m1)
}
