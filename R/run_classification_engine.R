#' Fit a statistical or machine learning model to predict group membership based on time-series feature values
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom caTools sample.split
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @param method the classification model to use. Defaults to Bayesian GLM 'BayesGLM'
#' @return an object of the class of model that was fit
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' temp <- tempfile()
#' download.file("https://ndownloader.figshare.com/files/24950795",temp)
#' ts <- read.csv(temp, header = FALSE)
#' 
#' ts <- ts %>%
#'   dplyr::mutate(id = dplyr::row_number()) %>%
#'   tidyr::pivot_longer(!id, names_to = "timepoint", values_to = "value") %>%
#'   dplyr::mutate(timepoint = as.numeric(gsub("V", "\\1", timepoint)))
#'
#' temp1 <- tempfile()
#' download.file("https://ndownloader.figshare.com/files/24950798",temp1)
#' ts_info <- read.csv(temp1, header = TRUE)
#'
#' main <- ts %>%
#'   dplyr::left_join(ts_info, by = c("id" = "ID")) %>%
#'   dplyr::mutate(group = dplyr::case_when(
#'                 grepl("synthetic", Keywords)  ~ 1,
#'                 !grepl("synthetic", Keywords) ~ 0)) # Creates binary
#'
#' ids <- unique(main$id)
#' ids_filt <- sample(ids, 50) # Random small sample to test
#' 
#' d1 <- main %>%
#'   dplyr::filter(id %in% ids_filt)
#'  
#'  outs <- calculate_features(data = main_filt, id_var = "id", group_var = "group", time_var = "timepoint", value_var = "value", feature_set = "feasts")
#'  outsN <- normalise_feature_frame(data = outs, names_var = "names", values_var = "values", method = "RobustSigmoid")
#'  mod <- run_classification_engine(data = outsN, id_var = "id", group_var = "group", premise = "inference", method = "BayesGLM")
#'}
#'

run_classification_engine <- function(data, id_var = NULL, group_var = NULL,
                                      method = c("GP", "GAM", "MixedGAM", "BayesGLM", "MixedBayesGLM", "SVM", "RandomForest", "NeuralNet")){
  
  #------------ Checks and argument validation ------------
  
  # Make BayesGLM the default method
  
  if(missing(method)){
    method <- "BayesGLM"
  } else{
    method <- match.arg(method)
  }
  
  # Argument checks
  
  methods <- c("GP", "GAM", "MixedGAM", "BayesGLM", "MixedBayesGLM", "SVM", "RandomForest", "NeuralNet")
  '%ni%' <- Negate('%in%')
  
  if(method %ni% methods){
    stop("method should be a single selection of: 'GP', 'GAM', 'MixedGAM', 'BayesGLM', 'MixedBayesGLM', 'SVM', 'RandomForest' or 'NeuralNet'.")
  }
  
  if(length(method) != 1){
    stop("method should be a single selection of: 'GP', 'GAM', 'MixedGAM', 'BayesGLM', 'MixedBayesGLM', 'RandomForest' or 'NeuralNet'.")
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
  
  # Recode into binary outcome and retain mapping
  
  if(is.null(group_var)){
    stop("group_var must be specified.")
  } else{
    data_group <- data %>%
      dplyr::rename(group = dplyr::all_of(group_var))
  }
  
  if(length(unique(data_group$group)) != 2){
    stop("group_var should have two levels.")
  }
  
  data_group <- data_group %>%
    dplyr::mutate(group = as.integer(as.factor(group)))
  
  #------------
  # ID variable
  #------------
  
  # Check if integer, recode into integer if not
  
  if(is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  } else{
    data_id <- data_group %>%
      dplyr::rename(id = dplyr::all_of(id_var))
  }
  
  data_id <- data_id %>%
    dplyr::mutate(id = as.integer(id))
  
  #-----------
  # Predictors
  #-----------
  
  pred_check <- data_id %>%
    dplyr::select(-c(id, group))
  
  ncol_1 <- ncol(pred_check)
  
  # Check if numeric
  
  checker <- pred_check %>%
    dplyr::select(where(is.numeric))
  
  ncol_2 <- ncol(checker)
  
  if(ncol_1 != ncol_2){
    stop("Non-numeric values detected in feature vectors. Please re-assess data.")
  }
  
  # Final return
  
  final <- data_id
  
  #------------ Model specification and fit ---------------
    
  #------- Make train-test split --------
    
  set.seed(123) # Fix RNG
  split <- caTools::sample.split(final$group_var, SplitRatio = 0.75) 
  train <- subset(final, split == TRUE) 
  test <- subset(final, split == FALSE)
  
  if(method == "GAM"){
  
    # Build model formula
      
    train <- train %>%
      dplyr::mutate(group = as.factor(group))
      
    test <- test %>%
      dplyr::mutate(group = as.factor(group))
      
    #------- Train -------
      
    m1 <- fit_gam(data = train)
      
    #------- Test -------
      
    preds <- predict(m1, newdata = test, type = "response")
      
    confusion_matrix <- ftable(test$group, preds)
    accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
      
    print(paste0("Test set classification accuracy: ", accuracy))
      
    return(m1)
  }
    
  if(method == "MixedGAM"){
      
    # Build model formula
      
    final <- final %>%
      dplyr::mutate(group = as.factor(group))
      
    # Fit model
      
    m1 <- fit_mixed_gam(data = final)
      
    return(m1)
  }
    
  if(method == "BayesGLM"){
    
    m1 <- fit_bayes_glm(data = final, id_var = id_var, group_var = group_var, iter = 3000, chains = 3, max_treedepth = 10)
      
    return(m1)
  }
    
  if(method == "MixedBayesGLM"){
    
    m1 <- fit_mixed_bayes_glm(data = final, id_var = id_var, group_var = group_var, iter = 3000, chains = 3, max_treedepth = 15)
      
    return(m1)
  }
  
  if(method == "GP"){
    
    m1 <- fit_gp(data, id_var = id_var, group_var = group_var, eps = 1e6, iter = 3000, chains = 3, max_treedepth = 15)
    
    return(m1)
  }
    
  if(method == "SVM"){
      
    # Build model formula
      
    train <- train %>%
      dplyr::mutate(group = as.factor(group))
      
    test <- test %>%
      dplyr::mutate(group = as.factor(group))
    
    m1 <- fit_svm(data = train, kernel = 'radial')
      
    #------- Test -------
      
    preds <- predict(m1, newdata = test)
      
    confusion_matrix <- ftable(test$group, preds)
    accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
    print(paste0("Test set classification accuracy: ", accuracy))
      
    return(m1)
  }
    
  if(method == "RandomForest"){
      
    # Build model formula
      
    train <- train %>%
      dplyr::mutate(group = as.factor(group))
      
    test <- test %>%
      dplyr::mutate(group = as.factor(group))
      
    m1 <- fit_rf(data = train, ntree = 100, importance = TRUE)
      
    #------- Test -------
      
    preds <- predict(m1, newdata = test)
      
    confusion_matrix <- ftable(test$group, preds)
    accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
    print(paste0("Test set classification accuracy: ", accuracy))
      
    return(m1)
  }
    
  if(method == "NeuralNet"){
      
    # Build model formula
      
    train <- train %>%
      dplyr::mutate(group = as.factor(group))
      
    test <- test %>%
      dplyr::mutate(group = as.factor(group))
      
    #------- Train -------
      
    message("Fitting model... This may take a long time.")
    
    m1 <- fit_nn(data = train, stepmax = 1e6)
      
    #------- Test -------
      
    preds <- predict(m1, newdata = test)
      
    confusion_matrix <- ftable(test$group, preds)
    accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
    print(paste0("Test set classification accuracy: ", accuracy))
      
    return(m1)
  }
}
