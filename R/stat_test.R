#' Calculate p-values for feature sets or features relative to an empirical null or each other using resampled t-tests
#' 
#' @importFrom stats sd
#' @import dplyr
#' 
#' @param data \code{data.frame} of raw classification accuracy results
#' @param iter_data \code{data.frame} containing the values to iterate over for seed and either feature name or set name
#' @param row_id \code{integer} denoting the row ID for \code{iter_data} to filter to
#' @param by_set \code{Boolean} specifying whether you want to compare feature sets (if \code{TRUE}) or individual features (if \code{FALSE}).
#' @param hypothesis \code{character} denoting whether p-values should be calculated for each feature set or feature (depending on \code{by_set} argument) individually relative to the null if \code{use_null = TRUE} in \code{tsfeature_classifier} through \code{"null"}, or whether pairwise comparisons between each set or feature should be conducted on main model fits only through \code{"pairwise"}.
#' @param metric \code{character} denoting the classification performance metric to use in statistical testing. Can be one of \code{"accuracy"}, \code{"precision"}, \code{"recall"}, \code{"f1"}. Defaults to \code{"accuracy"}
#' @param train_test_sizes \code{integer} vector containing the train and test set sample sizes
#' @param n_resamples \code{integer} denoting the number of resamples that were calculated
#' @returns object of class \code{data.frame}
#' @author Trent Henderson
#' 

stat_test <- function(data, iter_data, row_id, by_set = FALSE, hypothesis, metric, train_test_sizes, n_resamples){
  
  message(paste0("Calculating comparison ", row_id, "/", nrow(iter_data)))
  
  # Filter input data
  
  iter_filt <- iter_data[row_id, ]
  
  if(hypothesis == "null"){
    if(by_set){
      tmp_data <- data %>%
        dplyr::filter(.data$feature_set == iter_filt)
    } else{
      tmp_data <- data %>%
        dplyr::filter(.data$names == iter_filt)
    }
  } else{
    if(by_set){
      tmp_data <- data %>%
        dplyr::filter(.data$feature_set %in% c(iter_filt$method_a, iter_filt$method_b))
      } else{
      tmp_data <- data %>%
        dplyr::filter(.data$names %in% c(iter_filt$names_a, iter_filt$names_b))
      }
    if("model_type" %in% colnames(tmp_data)){
      tmp_data <- tmp_data %>%
        dplyr::filter(.data$model_type == "Main") # Catch cases where null results were calculated but we don't want these in pairwise comparisons
    }
  }
  
  # Select only relevant columns and rename for easier use later
  
  tmp_data <- select_stat_cols(data = tmp_data, by_set = by_set, metric = metric, hypothesis = hypothesis)
  
  # Check for 0 variance
  
  if(hypothesis == "null"){
    if(by_set){
      sd_check <- tmp_data %>%
        dplyr::group_by(.data$model_type, .data$feature_set) %>%
        dplyr::summarise(stddev = stats::sd(.data$mymetric, na.rm = TRUE)) %>%
        dplyr::ungroup()
    } else{
      sd_check <- tmp_data %>%
        dplyr::group_by(.data$model_type, .data$names) %>%
        dplyr::summarise(stddev = stats::sd(.data$mymetric, na.rm = TRUE)) %>%
        dplyr::ungroup()
    }
  } else{
    if(by_set){
      sd_check <- tmp_data %>%
        dplyr::group_by(.data$feature_set) %>%
        dplyr::summarise(stddev = stats::sd(.data$mymetric, na.rm = TRUE)) %>%
        dplyr::ungroup()
    } else{
      sd_check <- tmp_data %>%
        dplyr::group_by(.data$names) %>%
        dplyr::summarise(stddev = stats::sd(.data$mymetric, na.rm = TRUE)) %>%
        dplyr::ungroup()
    }
  }
  
  # Set up vectors
  
  if(hypothesis == "null"){
    x <- tmp_data %>% dplyr::filter(.data$model_type == "Main") %>% dplyr::pull(.data$mymetric)
    y <- tmp_data %>% dplyr::filter(.data$model_type == "Null") %>% dplyr::pull(.data$mymetric)
  } else{
    if(by_set){
      x <- tmp_data %>% dplyr::filter(.data$feature_set == iter_filt$method_a) %>% dplyr::pull(.data$mymetric)
      y <- tmp_data %>% dplyr::filter(.data$feature_set == iter_filt$method_b) %>% dplyr::pull(.data$mymetric)
    } else{
      x <- tmp_data %>% dplyr::filter(.data$names == iter_filt$names_a) %>% dplyr::pull(.data$mymetric)
      y <- tmp_data %>% dplyr::filter(.data$names == iter_filt$names_b) %>% dplyr::pull(.data$mymetric)
    }
  }
  
  # Do calculation
  
  if(0 %in% sd_check$stddev){
    if(hypothesis == "null"){
      if(by_set){
        outs <- data.frame(hypothesis = paste0(iter_filt, " != ", iter_filt, " (null)"),
                           feature_set = iter_filt, metric = metric, t_statistic = NA, p.value = NA)
      } else{
        outs <- data.frame(hypothesis = paste0(iter_filt, " != ", iter_filt, " (null)"),
                           feature_set = iter_filt, metric = metric, t_statistic = NA, p.value = NA)
      }
    } else{
      if(by_set){
        outs <- data.frame(hypothesis = paste0(iter_filt$method_a, " != ", iter_filt$method_b),
                           feature_set_a = iter_filt$method_a, feature_set_b = iter_filt$method_b, metric = metric, t_statistic = NA, p.value = NA)
      } else{
        outs <- data.frame(hypothesis = paste0(iter_filt$names_a, " != ", iter_filt$names_b),
                           names_a = iter_filt$names_a, names_b = iter_filt$names_b, metric = metric, t_statistic = NA, p.value = NA)
      }
    }
  } else{
    
    # Calculate means for final dataframe
    
    x_mean <- mean(x, na.rm = TRUE)
    y_mean <- mean(y, na.rm = TRUE)
    
    if(hypothesis == "null"){
      if(by_set){
        t_test <- resampled_ttest(x = x, y = y, n = n_resamples, n1 = train_test_sizes[1], n2 = train_test_sizes[1])
        
        outs <- data.frame(hypothesis = paste0(iter_filt, " != own null"),
                           feature_set = iter_filt, metric = metric, set_mean = x_mean, null_mean = y_mean,
                           t_statistic = t_test$statistic, p.value = t_test$p.value)
      } else{
        t_test <- resampled_ttest(x = x, y = y, n = n_resamples, n1 = train_test_sizes[1], n2 = train_test_sizes[1])
        
        outs <- data.frame(hypothesis = paste0(iter_filt, " != own null"),
                           names = iter_filt, feature_set = gsub("_.*", "\\1", iter_filt), 
                           original_names = gsub("^[^_]*_", "", iter_filt), metric = metric,
                           feature_mean = x_mean, null_mean = y_mean,
                           t_statistic = t_test$statistic, p.value = t_test$p.value)
      }
    } else{
      if(by_set){
        t_test <- resampled_ttest(x = x, y = y, n = n_resamples, n1 = train_test_sizes[1], n2 = train_test_sizes[1])
        
        outs <- data.frame(hypothesis = paste0(iter_filt$method_a, " != ", iter_filt$method_b),
                           feature_set_a = iter_filt$method_a, feature_set_b = iter_filt$method_b, metric = metric, 
                           set_a_mean = x_mean, set_b_mean = y_mean,
                           t_statistic = t_test$statistic, p.value = t_test$p.value)
      } else{
        t_test <- resampled_ttest(x = x, y = y, n = n_resamples, n1 = train_test_sizes[1], n2 = train_test_sizes[1])
        
        outs <- data.frame(hypothesis = paste0(iter_filt$names_a, " != ", iter_filt$names_b),
                           names_a = iter_filt$names_a, names_b = iter_filt$names_b, metric = metric, 
                           names_a_mean = x_mean, names_b_mean = y_mean, 
                           t_statistic = t_test$statistic, p.value = t_test$p.value)
      }
    }
  }
  
  return(outs)
}
