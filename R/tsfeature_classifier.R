#' Fit classifiers using time-series features using a resample-based approach and get a fast understanding of performance
#' 
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider crossing
#' @importFrom purrr map map_dfr
#' @importFrom e1071 svm
#' 
#' @param data \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features} with an included \code{group} column as per \code{theft::calculate_features}
#' @param classifier \code{function} specifying the classifier to fit. Should be a function with 2 arguments: \code{formula} and \code{data} containing a classifier compatible with R's \code{predict} functionality. Please note that \code{tsfeature_classifier} z-scores data prior to modelling using the train set's information so disabling default scaling if your function uses it is recommended. Defaults to \code{NULL} which means the following linear SVM is fit: \code{classifier = function(formula, data){mod <- e1071::svm(formula, data = data, kernel = "linear", scale = FALSE, probability = TRUE)}}
#' @param train_size \code{numeric} denoting the proportion of samples to use in the training set. Defaults to \code{0.75}
#' @param n_resamples \code{integer} denoting the number of resamples to calculate. Defaults to \code{30}
#' @param by_set \code{Boolean} specifying whether to compute classifiers for each feature set. Defaults to \code{TRUE}. If \code{FALSE}, the function will instead find the best individually-performing features
#' @param use_null \code{Boolean} whether to fit null models where class labels are shuffled in order to generate a null distribution that can be compared to performance on correct class labels. Defaults to \code{FALSE}
#' @param seed \code{integer} to fix R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param preference \code{character} denoting which feature set to keep (meaning the others will be filtered out) between \code{"feasts"}, \code{"tsfeatures"}, and \code{"Kats"} since there is considerable overlap between these three sets. Defaults to \code{"feasts"}. Only applies if \code{by_set = TRUE} (since a set of "All features" is constructed automatically as a comparator)
#' @return \code{list} containing a named \code{vector} of train-test set sizes, and a \code{data.frame} of classification performance results
#' @author Trent Henderson
#' @export
#' @examples
#' \donttest{
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22",
#'   seed = 123)
#'   
#' classifiers <- tsfeature_classifier(featMat,
#'   by_set = FALSE)
#'}
#' 

tsfeature_classifier <- function(data, classifier = NULL, train_size = 0.75, n_resamples = 30, by_set = TRUE,
                                 use_null = FALSE, seed = 123, preference = c("feasts", "tsfeatures", "Kats")){
  
  stopifnot(inherits(data, "feature_calculations") == TRUE)
  
  if(train_size < 0 || train_size > 1){
    stop("train_size should be a proportion between 0 and 1.")
  }
  
  if(n_resamples < 0){
    stop("n_resamples must be an integer >= 1.")
  }
  
  # Check preference
  
  preference <- match.arg(preference)
  '%ni%' <- Negate('%in%')
  
  if(preference %ni% c("feasts", "tsfeatures", "Kats")){
    stop("preference must be one of 'feasts', 'tsfeatures' or 'Kats'.")
  }
  
  # Set up data
  
  tmp <- data[[1]] %>%
    dplyr::mutate(group = as.factor(as.character(.data$group)),
                  names = paste0(.data$method, "_", .data$names)) %>%
    dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>% # Delete features that are all NaNs
    dplyr::select(mywhere(~dplyr::n_distinct(.) > 1)) # Delete features with constant values
  
  # Add "All features" if by_set = TRUE
  
  if(by_set){
    
    # Remove duplicate features
    
    tmp2 <- filter_duplicates(data = data, preference = preference)
    
    # Construct set of all features
    
    tmp2 <- tmp2[[1]] %>%
      dplyr::mutate(method = "allfeatures",
                    group = as.factor(as.character(.data$group)),
                    names = paste0(.data$method, "_", .data$names)) %>%
      dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select_if(~sum(!is.na(.)) > 0) %>% # Delete features that are all NaNs
      dplyr::select(mywhere(~dplyr::n_distinct(.) > 1)) # Delete features with constant values
    
    tmp <- tmp %>%
      dplyr::left_join(tmp2, by = c("id" = "id", "group" = "group"))
  }
  
  # Assign samples to train or test
  
  dt <- sort(sample(nrow(tmp), nrow(tmp) * train_size))
  train <- tmp[dt, ] %>% dplyr::mutate(set_split = "Train")
  test <- tmp[-dt, ] %>% dplyr::mutate(set_split = "Test")
  
  # Pivot back to tidy dataframe
  
  tmp <- dplyr::bind_rows(train, test) %>%
    tidyr::pivot_longer(cols = -c("id", "group", "set_split"), names_to = "names", values_to = "values")
  
  # Check the classifier function argument and set default if NULL
  
  if(is.null(classifier)){
    classifier <- function(formula, data){
      mod <- e1071::svm(formula, data = data, scale = FALSE, probability = TRUE)
    }
  } else{
    if(length(names(formals(classifier))) != 2){
      stop("classifier should be a function with 2 arguments: 'formula' and 'data'.")
    }
    if(!identical(names(formals(classifier)), c("formula", "data"))){
      stop("classifier should be a function with 2 arguments: 'formula' and 'data'.")
    }
  }
  formals(classifier)
  
  #------------------ Find good features to retain across resamples ---------------
  
  # Get number of cases in each set
  
  train_rows <- nrow(train)
  test_rows <- nrow(test)
  
  # Get proportion of samples in each group in train-test splits
  
  train_props <- tmp %>%
    dplyr::filter(.data$set_split == "Train") %>%
    dplyr::select(c(.data$id, .data$group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  test_props <- tmp %>%
    dplyr::filter(.data$set_split == "Test") %>%
    dplyr::select(c(.data$id, .data$group)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup()
  
  #-------------------------------------------------
  # Keep all features that have enough unique values
  # to not ruin models with resampling
  #-------------------------------------------------
  
  # Generate resamples
  
  res_data <- 1:n_resamples %>%
    purrr::map(~ resample_data(tmp, train_rows = train_rows, test_rows = test_rows, train_props, test_props, .x))
  
  # Find only features across all resamples that have SD > 0
  
  good_features <- 1:n_resamples %>%
    purrr::map(~ find_good_features(res_data, .x)) %>%
    unlist()
  
  good_features <- data.frame(names = good_features) %>%
    group_by(.data$names) %>%
    summarise(counter = n()) %>%
    ungroup() %>%
    filter(.data$counter == max(.data$counter)) %>%
    pull(.data$names)
  
  # Filter each resample by the new "good" feature vector
  
  res_data <- 1:n_resamples %>%
    purrr::map(~ filter_good_features(res_data, .x, good_features = good_features))
  
  #---------------- Model fitting ----------------
  
  if(by_set){
    
    feature_set_names <- colnames(res_data[[1]]$Train)
    feature_set_names <- feature_set_names[!feature_set_names %in% c("group")]
    feature_set_names <- unique(gsub("_.*", "\\1", feature_set_names))
    
    iters <- tidyr::crossing(1:n_resamples, feature_set_names) %>%
      dplyr::rename(seed = 1, set_name = 2)
    
    if(use_null){
      # Compute main results
      
      outs <- 1:nrow(iters) %>%
        purrr::map_dfr(~ fit_models(res_data, iters, .x, is_null_run = FALSE, classifier = classifier))
      
      # Compute null results
      
      message("\n")
      
      outs_null <- 1:nrow(iters) %>%
        purrr::map_dfr(~ fit_models(res_data, iters, .x, is_null_run = TRUE, classifier = classifier))
      
      outs <- dplyr::bind_rows(outs, outs_null) %>%
        dplyr::mutate(method = ifelse(.data$method == "allfeatures", "All features", .data$method)) %>%
        dplyr::arrange(.data$method)
      
    } else{
      outs <- 1:nrow(iters) %>%
        purrr::map_dfr(~ fit_models(res_data, iters, .x, is_null_run = FALSE, classifier = classifier)) %>%
        dplyr::mutate(method = ifelse(.data$method == "allfeatures", "All features", .data$method))
    }
  } else{
    
    feature_names <- unique(names(res_data[[1]]$Train))
    feature_names <- feature_names[!feature_names %in% "group"]
    
    iters <- tidyr::crossing(1:n_resamples, feature_names) %>%
      dplyr::rename(seed = 1, feature_name = 2)
    
    if(use_null){
      # Compute main results
      
      outs <- 1:nrow(iters) %>%
        purrr::map_dfr(~ fit_models(res_data, iters, .x, is_null_run = FALSE, classifier = classifier))
      
      # Compute null results
      
      outs_null <- 1:nrow(iters) %>%
        purrr::map_dfr(~ fit_models(res_data, iters, .x, is_null_run = TRUE, classifier = classifier))
      
      outs <- dplyr::bind_rows(outs, outs_null)
      
    } else{
      
      outs <- 1:nrow(iters) %>%
        purrr::map_dfr(~ fit_models(res_data, iters, .x, is_null_run = FALSE, classifier = classifier))
    }
  }
  
  sizes <- c("train_size" = train_rows, "test_size" = test_rows)
  outs <- list(sizes, outs)
  names(outs) <- c("TrainTestSizes", "ClassificationResults")
  return(outs)
}
