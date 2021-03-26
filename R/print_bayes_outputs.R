#' Produce an R Markdown report of all the model fit and diagnostic plots for a Bayesian classification program
#' @import rmarkdown
#' @param model a model output from one of the Bayes function arguments contained in sawlog
#' @return a R Markdown document saved in the user-specified location
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' print_bayes_outputs(model, filepath)
#' }
#'

print_bayes_outputs <- function(model){
  
  #------------ Checks ------------
  
  # Model class
  
  x
  
  #------------ Render Rmd --------
  
  rmarkdown::render(source(system.file("inst/produce_Rmd.R", package = "sawlog")), "pdf_document")
  
}
