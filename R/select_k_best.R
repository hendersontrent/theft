#' Perform fast and simple univariate feature selection based on an output vector using analysis of variance or correlation
#' 
#' @importFrom stats aov cor.test
#' @importFrom dplyr %>% select rename left_join mutate filter pull slice_max n_distinct
#' @importFrom tidyr drop_na pivot_wider pivot_longer
#' @param data the \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param k \code{integer} denoting the number of features to retain. Defaults to half the length of the unique features available in \code{data}
#' @param outputs \code{data.frame} containing output data if it was not included in the \code{group} column initially when running \code{calculate_features}. Should have 2 columns where the first is an ID variable that can be joined to the \code{id} column of \code{data} and the second the output of interest (i.e., a 'y' variable). If a character or factor, classification is assumed, and if numeric, regression is assumed. Defaults to \code{NULL} which assumes the \code{group} variable exists in \code{data}
#' @return object of class \code{feature_calculations} that contains the summary statistics for each feature that was retained
#' @author Trent Henderson
#' @export
#' 

select_k_best <- function(data, k = floor(length(unique(data[[1]]$names)) / 2), outputs = NULL){
  
  stopifnot(inherits(data, "feature_calculations") == TRUE)
  
  # Join outputs to data if not null
  
  if(!is.null(outputs)){
    
    if(ncol(outputs) != 2){
      message("More than 2 columns found in outputs. Assuming column 1 is ID column to match to data argument and column 2 is the output values.")
    }
    
    outputs <- outputs %>%
      dplyr::select(c(1,2)) %>%
      dplyr::rename(id = 1,
                    group = 2)
    
    if(class(outputs$group) == "numeric"){
      message("Numeric output variable detected. Assuming task is regression and using correlation p-value as selection metric.")
      prob_type <- "regression"
    } else if(class(outputs$y) %in% c("character", "factor")){
      message("Character or factor output variable detected. Assuming task is classification and using ANOVA p-value as selection metric.")
      prob_type <- "classification"
    } else{
      stop("y variable should be a character/factor or numeric.")
    }
    
    tmp <- data[[1]] %>%
      dplyr::left_join(outputs, by = c("id" = "id"))
    
  } else{
    
    tmp <- data[[1]]
    
    if(class(tmp$group) == "numeric"){
      message("Numeric group variable detected. Assuming task is regression and using correlation p-value as selection metric.")
      prob_type <- "regression"
    } else if(class(tmp$group) %in% c("character", "factor")){
      message("Character or factor group variable detected. Assuming task is classification and using ANOVA p-value as selection metric.")
      prob_type <- "classification"
    } else{
      stop("group variable should be a character/factor or numeric.")
    }
  }
  
  tmp <- tmp %>%
    dplyr::mutate(feature = paste0(method, "_", names))  
  
  if(class(tmp$group) == "character"){
    tmp <- tmp %>%
      dplyr::mutate(group = as.factor(group))
  }
  
  # Drop features that are all NaNs or constants
  
  tmp <- tmp %>%
    dplyr::select(c(id, group, feature, values)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "feature", values_from = "values") %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::select(mywhere(~dplyr::n_distinct(.) > 1))
  
  tmp <- tmp %>%
    tidyr::pivot_longer(cols = 3:ncol(tmp), names_to = "feature", values_to = "values")
  
  #----------------- Do feature selection ----------------
  
  # Compute statistics for each time-series feature
  
  feature_stats <- vector(mode = "list", length = length(unique(tmp$feature)))
  
  if(prob_type == "classification"){
    
    for(i in unique(tmp$feature)){
      
      feat_i <- tmp %>%
        dplyr::filter(feature == i)
      
      fit <- try(summary(stats::aov(values ~ group, data = feat_i)))
      
      if("try-error" %in% class(fit)){
        feature_stats[[match(i, unique(tmp$feature))]] <- data.frame(feature = i, statistic = NA)
      } else{
        feature_stats[[match(i, unique(tmp$feature))]] <- data.frame(feature = i, statistic = fit[[1]]$`Pr(>F)`[1])
      }
    }
    
  } else{
    
    for(i in unique(tmp$feature)){
      
      feat_i <- tmp %>%
        dplyr::filter(feature == i)
      
      fit <- try(abs(stats::cor.test(feat_i$values, feat_i$group)))
      
      if(class(fit) == "try-error"){
        feature_stats[[match(i, unique(tmp$feature))]] <- data.frame(feature = i, statistic = NA)
      } else{
        feature_stats[[match(i, unique(tmp$feature))]] <- data.frame(feature = i, statistic = fit$p.value)
      }
    }
  }
  
  feature_stats <- do.call("rbind", feature_stats)
  
  # Filter features based on statistics
  
  feature_stats <- feature_stats %>% 
    tidyr::drop_na() %>%
    dplyr::slice_min(statistic, n = k) %>%
    dplyr::pull(feature)
  
  # Final returns
  
  outs <- data[[1]] %>%
    dplyr::mutate(feature = paste0(method, "_", names)) %>%
    dplyr::filter(feature %in% feature_stats) %>%
    dplyr::select(-c(feature))
  
  outs <- structure(list(outs), class = "feature_calculations")
  return(outs)
}
