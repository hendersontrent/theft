#' Produce a PDF report of all the model fit and diagnostic plots for a Bayesian classification program
#' @import ggplot2
#' @import bayesplot
#' @importFrom Cairo CairoPDF
#' @param model a model output from one of the Bayes function arguments contained in {sawlog}
#' @return a PDF document saved in the current work directory
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' print_bayes_outputs(model)
#' }
#'

print_bayes_outputs <- function(model){
  
  path <- getwd()
  path <- paste0(path,"/BayesOutput.pdf")
  
  if(class(model) != 'stanfit'){
    stop("model should be an object of class 'stanfit', such as that produced by sawlog::run_classification_engine")
  }
  
  #------------ Plots -------------
  
  # Posterior predictive checks
  
  bayesplot::color_scheme_set("red")
  p <- bayesplot::pp_check(model, type = "bars", nsamples = 100) +
    ggplot2::labs(title = "Posterior predictive check",
                  x = "Group",
                  y = "Count")
  
  p1 <- pp_check(model, type = "ecdf_overlay", nsamples = 100) +
    ggplot2::labs(title = "Posterior predictive check of cumulative probability function",
                  x = "Group",
                  y = "Cumulative Probability")
  
  # Chain convergence
  
  bayesplot::color_scheme_set("mix-blue-red")
  p2 <- bayesplot::mcmc_trace(model)
  
  # Leave-one-out
  
  
  
  # Coefficients
  
  bayesplot::color_scheme_set("red")
  p4 <- bayesplot::mcmc_hist(model) +
    ggplot2::labs(title = "Coefficient posterior distributions") +
    ggplot2::geom_vline(xintercept = 0, lty = "dashed", colour = "black", size = 1)
  
  #------------ Render ------------
  
  CairoPDF(path,11,8)
  print(p4)
  print(p)
  print(p1)
  print(p2)
  dev.off()
}