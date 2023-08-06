#' Fit classification model and compute key metrics
#' 
#' @importFrom stats predict
#' @importFrom dplyr select mutate
#' 
#' @param data \code{list} containing train and test sets
#' @param iter_data \code{data.frame} containing the values to iterate over for seed and either feature name or set name
#' @param row_id \code{integer} denoting the row ID for \code{iter_data} to filter to
#' @param is_null_run \code{Boolean} whether the calculation is for a null model. Defaults to \code{FALSE}
#' @param classifier \code{function} specifying the classifier to fit. Should be a function with 2 arguments: \code{formula} and \code{data}. Please note that \code{tsfeature_classifier} z-scores data prior to modelling using the train set's information so disabling default scaling if your function uses it is recommended.
#' @return \code{data.frame} of classification results
#' @author Trent Henderson
#'

fit_models <- function(data, iter_data, row_id, is_null_run = FALSE, classifier){
  
  if(!is_null_run){
    message(paste0("Fitting model ", row_id, "/", nrow(iter_data)))
  } else{
    message(paste0("Fitting null model ", row_id, "/", nrow(iter_data)))
  }
  
  # Set up data for modelling
  
  iter_filt <- iter_data[row_id, ]
  train <- data[[iter_filt$seed]]$Train
  test <- data[[iter_filt$seed]]$Test
  
  if("feature_name" %in% colnames(iter_data)){
    train <- train[, c("group", iter_filt$feature_name)]
    test <- test[, c("group", iter_filt$feature_name)]
  } else{
    train <- train %>%
      dplyr::select(c("group", contains(iter_filt$set_name)))
    
    test <- test %>%
      dplyr::select(c("group", contains(iter_filt$set_name)))
  }
  
  if(is_null_run){
    set.seed(iter_filt$seed)
    shuffles <- sample(train$group, replace = FALSE) # Shuffle class labels for train set
    train$group <- shuffles
  }
  
  #---------------
  # Normalise data
  #---------------
  
  # Get numbers to rescale by from train set
  
  rescalers <- get_rescale_vals(train)
  
  # Apply rescaling
  
  train <- rescale_zscore(train, rescalers)
  test <- rescale_zscore(test, rescalers)
  
  # Fit classifier, generate predictions, and calculate metrics
  
  mod <- classifier(formula = group ~ ., data = train)
  cm <- t(as.matrix(table(stats::predict(mod, newdata = test), test$group)))
  acc <- sum(diag(cm)) / sum(cm)
  precision <- diag(cm) / rowSums(cm)
  recall <- diag(cm) / colSums(cm)
  mean_precision <- mean(precision, na.rm = TRUE)
  mean_recall <- mean(recall, na.rm = TRUE)
  mean_f1_score <- mean(2 * (precision * recall) / (precision + recall), na.rm = TRUE)
  
  results <- data.frame(model_type = ifelse(is_null_run, "Null", "Main"),
                        resample = iter_filt$seed,
                        accuracy = acc,
                        mean_precision = mean_precision,
                        mean_recall = mean_recall,
                        mean_f1_score = mean_f1_score)
  
  if("feature_name" %in% colnames(iter_data)){
    results <- results %>%
      dplyr::mutate(names = iter_filt$feature_name)
  } else{
    results <- results %>%
      dplyr::mutate(feature_set = iter_filt$set_name)
  }
  
  return(results)
}
