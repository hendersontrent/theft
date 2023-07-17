#' Helper function to find features in both train and test set that are "good"
#' 
#' @importFrom stats sd
#' 
#' @param data \code{list} of \code{"Train"} and \code{"Test"} data
#' @param x \code{integer} denoting the resample index to operate on
#' @return \code{character} vector of "good" feature names
#' @author Trent Henderson
#' 

find_good_features <- function(data, x){
  
  # Do train set
  
  train_feats <- data[[x]]$Train %>%
    dplyr::select(-c(.data$group)) %>%
    dplyr::select_if(~ stats::sd(.) > 0) # Delete features with SD = 0
  
  train_feats <- colnames(train_feats)
  
  # Do test set
  
  test_feats <- data[[x]]$Test %>%
    dplyr::select(-c(.data$group)) %>%
    dplyr::select_if(~ sd(.) > 0) # Delete features with SD = 0
  
  test_feats <- colnames(test_feats)
  
  # Return consistent good values across both
  
  vals <- unlist(list(train_feats, test_feats))
  return(unique(vals[duplicated(vals)]))
}

#' Filter resample data sets according to good feature list
#' 
#' @param data \code{list} of \code{"Train"} and \code{"Test"} data
#' @param x \code{integer} denoting the resample index to operate on
#' @param good_features \code{character} vector of good features to keep
#' @return \code{list} of filtered train and test data
#' @author Trent Henderson
#' 

filter_good_features <- function(data, x, good_features){
  train_feats <- data[[x]]$Train
  train_feats <- train_feats[append("group", good_features)]
  test_feats <- data[[x]]$Test
  test_feats <- test_feats[append("group", good_features)]
  filtered_data <- list(train_feats, test_feats)
  names(filtered_data) <- c("Train", "Test")
  return(filtered_data)
}
