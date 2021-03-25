#' Fit a statistical or machine learning model to predict group membership of feature-space time-series data
#' @import dplyr
#' @importFrom magrittr %>%
#' @import mgcv
#' @import rstan
#' @importFrom e1071 svm
#' @importFrom randomForest randomForest
#' @importFrom caTools sample.split
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @param premise the type of analytical work to be conducted. Defaults to 'prediction'
#' @param method the classification model to use. Defaults to non-mixed-effects Generalised Additive Model 'GAM'
#' @return a object of the class of model that was fit
#' @author Trent Henderson
#' @export
#' @examples
#' xxxxxxxxxxxxxxxxx
#'

run_classification_engine <- function(data, id_var = NULL, group_var = NULL, premise = c("inference", "prediction"),
                                   method = c("GAM","MixedGAM","BayesGLM","MixedBayesGLM","SVM", "RandomForest")){
  
  #------------ Checks and argument validation ------------
  
  # Make prediction the default premise
  
  if(missing(premise)){
    premise <- "prediction"
  } else{
    premise <- match.arg(premise)
  }
  
  # Make GAM the default method
  
  if(missing(method)){
    method <- "GAM"
  } else{
    method <- match.arg(method)
  }
  
  # Argument checks
  
  premises <- c("inference", "prediction")
  methods <- c("GAM", "MixedGAM", "BayesGLM", "MixedBayesGLM", "SVM", "RandomForest")
  '%ni%' <- Negate('%in%')
  
  if(premise %ni% premises){
    stop("premise should be a single selection of: 'inference' or 'prediction'.")
  }
  
  if(length(premise) != 1){
    stop("premise should be a single selection of: 'inference' or 'prediction'.")
  }
  
  if(method %ni% methods){
    stop("method should be a single selection of: 'GAM', 'MixedGAM', 'BayesGLM', 'MixedBayesGLM', 'SVM' or 'RandomForest'.")
  }
  
  if(length(method) != 1){
    stop("method should be a single selection of: 'GAM', 'MixedGAM', 'BayesGLM', 'MixedBayesGLM', 'SVM' or 'RandomForest'.")
  }
  
  if(!is.null(id_var) & !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  if(!is.null(group_var) & !is.character(group_var)){
    stop("group_var should be a string specifying a variable in the input data that identifies the group membership variable/column.")
  }
  
  # Print prompt to normalise data
  
  message("Function assumes input data has been normalised. Please revise with sawlog::normalise_features() if your data is not normalised. This has large implications for model accuracy.")
  
  #------------ Quantitative data checks ------------------
  
  #-----------------
  # Outcome variable
  #-----------------
  
  if(length(unique(data$group_var)) != 2){
    stop()
  }
  
  # Recode into binary outcome and retain mapping
  
  x
  
  #------------
  # ID variable
  #------------
  
  # Check if integer, recode into integer if not
  
  x
  
  #-----------
  # Predictors
  #-----------
  
  x
  
  #------------ Model specification and fit ---------------
  
  #----------
  # Inference
  #----------
  
  if(premise == "inference"){
    
    if(method %ni% c('GAM', "MixedGAM", "BayesGLM", "MixedBayesGLM")){
      stop("for premise 'inference', method should be a single selection of: 'GAM', 'MixedGAM', 'BayesGLM' or 'MixedBayesGLM'.")
    }
    
    if(method == "GAM"){
      
      x
      
    }
    
    if(method == "MixedGAM"){
      
      x
      
    }
    
    if(method == "BayesGLM"){
      
      x
      
    }
    
    if(method == "MixedBayesGLM"){
      
      x
      
    }
    
  }
  
  #-----------
  # Prediction
  #-----------
  
  if(premise == "prediction"){
    
    if(method %ni% c('GAM', "BayesGLM", "SVM", "RandomForest")){
      stop("for premise 'inference', method should be a single selection of: 'GAM', 'BayesGLM', 'SVM' or 'RandomForest'.")
    }
    
    if(method == "GAM"){
      
      x
      
    }
    
    if(method == "BayesGLM"){
      
      x
      
    }
    
    if(method == "SVM"){
      
      x
      
    }
    
    if(method == "RandomForest"){
      
      x
      
    }
    
  }
  
}
