#' Helper function to create a resampled dataset
#' 
#' @param data \code{data.frame} containing time-series features
#' @param train_rows \code{integer} denoting the number of cases in the train set
#' @param test_rows \code{integer} denoting the number of cases in the test set
#' @param train_groups \code{data.frame} containing proportions of each class in original train split
#' @param test_groups \code{data.frame} containing proportions of each class in original test split
#' @param seed \code{integer} denoting fixed value for R's pseudorandom number generator
#' @return \code{list} containing new train and test data
#' @author Trent Henderson
#' 

resample_data <- function(data, train_rows, test_rows, train_groups, test_groups, seed){
  
  if(seed == 1){
    
    # Use pre-designated train-test split
    
    train <- data %>%
      dplyr::filter(.data$set_split == "Train") %>%
      tidyr::pivot_wider(id_cols = c("id", "set_split", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id, .data$set_split))
    
    test <- data %>%
      dplyr::filter(.data$set_split == "Test") %>%
      tidyr::pivot_wider(id_cols = c("id", "set_split", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id, .data$set_split))
    
  } else{
    
    set.seed(seed)
    
    # Create resampled dataset
    
    newdata <- list()
    
    # Randomly allocate the correct number of each class to train and test as manual stratified sample
    
    resampled <- list()
    
    for(i in unique(data$group)){
      feasible_ids <- data %>%
        dplyr::filter(.data$group == i) %>%
        dplyr::select(c(.data$id)) %>%
        dplyr::distinct() %>%
        dplyr::pull(.data$id)
      
      n <- train_groups %>%
        dplyr::filter(.data$group == i) %>%
        dplyr::pull(.data$counter)
      
      traindata <- data.frame(id = sample(feasible_ids, size = n)) %>%
        dplyr::mutate(set_split_new = "Train")
      
      testdata <- data.frame(id = feasible_ids[!feasible_ids %in% traindata$id]) %>%
        dplyr::mutate(set_split_new = "Test")
      
      stopifnot((nrow(traindata) + nrow(testdata)) == length(feasible_ids))
      
      joined <- dplyr::bind_rows(traindata, testdata)
      resampled[[i]] <- joined
    }
    
    resampled <- do.call(rbind, resampled)
    rownames(resampled) <- c()
    
    # Properly set up train and test data
    
    newdata <- data %>%
      dplyr::left_join(resampled, by = c("id" = "id"))
    
    train <- newdata %>%
      dplyr::filter(.data$set_split_new == "Train") %>%
      dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
    
    test <- newdata %>%
      dplyr::filter(.data$set_split_new == "Test") %>%
      dplyr::select(c(.data$id, .data$group, .data$names, .data$values)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
  }
  
  data_list <- list(train, test)
  names(data_list) <- c("Train", "Test")
  return(data_list)
}
