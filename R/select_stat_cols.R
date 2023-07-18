#' Helper function to select only the relevant columns for statistical testing
#' 
#' @import dplyr
#' 
#' @param data \code{data.frame} of classification accuracy results
#' @param by_set \code{Boolean} specifying whether you want to compare feature sets (if \code{TRUE}) or individual features (if \code{FALSE}).
#' @param hypothesis \code{character} denoting whether p-values should be calculated for each feature set or feature (depending on \code{by_set} argument) individually relative to the null if \code{use_null = TRUE} in \code{tsfeature_classifier} through \code{"null"}, or whether pairwise comparisons between each set or feature should be conducted on main model fits only through \code{"pairwise"}.
#' @param metric \code{character} denoting the classification performance metric to use in statistical testing. Can be one of \code{"accuracy"}, \code{"precision"}, \code{"recall"}, \code{"f1"}. Defaults to \code{"accuracy"}
#' @returns object of class \code{data.frame}
#' @author Trent Henderson
#' 

select_stat_cols <- function(data, by_set, metric, hypothesis){
  
  if(hypothesis == "null"){
    if(by_set){
      if(metric == "accuracy"){
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$method, .data$accuracy)) %>% dplyr::rename(mymetric = .data$accuracy)
      } else if(metric == "precision"){
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$method, .data$mean_precision)) %>% dplyr::rename(mymetric = .data$mean_precision)
      } else if(metric == "recall"){
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$method, .data$mean_recall)) %>% dplyr::rename(mymetric = .data$mean_recall)
      } else{
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$method, .data$mean_f1_score)) %>% dplyr::rename(mymetric = .data$mean_recall)
      }
    } else{
      if(metric == "accuracy"){
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$names, .data$accuracy)) %>% dplyr::rename(mymetric = .data$accuracy)
      } else if(metric == "precision"){
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$names, .data$mean_precision)) %>% dplyr::rename(mymetric = .data$mean_precision)
      } else if(metric == "recall"){
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$names, .data$mean_recall)) %>% dplyr::rename(mymetric = .data$mean_recall)
      } else{
        tmp <- data %>% dplyr::select(c(.data$model_type, .data$names, .data$mean_f1_score)) %>% dplyr::rename(mymetric = .data$mean_f1_score)
      }
    }
  } else{
    if(by_set){
      if(metric == "accuracy"){
        tmp <- data %>% dplyr::select(c(.data$method, .data$accuracy)) %>% dplyr::rename(mymetric = .data$accuracy)
      } else if(metric == "precision"){
        tmp <- data %>% dplyr::select(c(.data$method, .data$mean_precision)) %>% dplyr::rename(mymetric = .data$mean_precision)
      } else if(metric == "recall"){
        tmp <- data %>% dplyr::select(c(.data$method, .data$mean_recall)) %>% dplyr::rename(mymetric = .data$mean_recall)
      } else{
        tmp <- data %>% dplyr::select(c(.data$method, .data$mean_f1_score)) %>% dplyr::rename(mymetric = .data$mean_f1_score)
      }
    } else{
      if(metric == "accuracy"){
        tmp <- data %>% dplyr::select(c(.data$names, .data$accuracy)) %>% dplyr::rename(mymetric = .data$accuracy)
      } else if(metric == "precision"){
        tmp <- data %>% dplyr::select(c(.data$names, .data$mean_precision)) %>% dplyr::rename(mymetric = .data$mean_precision)
      } else if(metric == "recall"){
        tmp <- data %>% dplyr::select(c(.data$names, .data$mean_recall)) %>% dplyr::rename(mymetric = .data$mean_recall)
      } else{
        tmp <- data %>% dplyr::select(c(.data$names, .data$mean_f1_score)) %>% dplyr::rename(mymetric = .data$mean_f1_score)
      }
    }
  }
  
  return(tmp)
}
