#--------------- Helper functions ----------------

#--------------
# Model fitting
#--------------

fit_single_feature_models <- function(data, test_method, use_balanced_accuracy, use_k_fold, num_folds, use_empirical_null, null_testing_method, num_permutations, feature, pb, seed){
  
  # Print {purrr} iteration progress updates in the console
  
  pb$tick()$print()
  
  set.seed(seed)
  
  tmp <- clean_by_feature(data = data, x = feature) %>%
    dplyr::select(-c(id))
  
  if(use_k_fold){
    
    # Train main model
    
    if (use_balanced_accuracy) {
      fitControl <- caret::trainControl(method = "cv",
                                        number = num_folds,
                                        summaryFunction = calculate_balanced_accuracy,
                                        classProbs = TRUE)
    } else {
      fitControl <- caret::trainControl(method = "cv",
                                        number = num_folds,
                                        classProbs = TRUE)
    }
    
    mod <- caret::train(group ~ ., 
                        data = tmp, 
                        method = test_method, 
                        trControl = fitControl,
                        preProcess = c("center", "scale"))
    
    # Get main predictions
    
    mainOuts <- extract_prediction_accuracy(mod = mod, use_balanced_accuracy = use_balanced_accuracy) %>%
      dplyr::mutate(category = "Main")
    
  } else{
    
    if (use_balanced_accuracy) {
      fitControl <- caret::trainControl(method = "none",
                                        summaryFunction = calculate_balanced_accuracy,
                                        classProbs = TRUE)
    } else {
      fitControl <- caret::trainControl(method = "none",
                                        classProbs = TRUE)
    }
    
    mod <- caret::train(group ~ ., 
                        data = tmp, 
                        method = test_method, 
                        trControl = fitControl,
                        preProcess = c("center", "scale"))
    
    # Get main predictions and account for some times when factor level error from {caret appears}
    # NOTE: Should this also go in the multivariate version or only when a case arises?
    
    u <- dplyr::union(predict(mod, newdata = tmp), tmp$group)
    mytable <- table(factor(predict(mod, newdata = tmp), u), factor(tmp$group, u))
    cm <- t(as.matrix(caret::confusionMatrix(mytable)$table)) # Transpose as {caret} has reversed format
    
    if(use_balanced_accuracy){
      
      recall <- 1:nrow(cm) %>%
        purrr::map(~ calculate_recall(cm, x = .x)) %>%
        unlist()
      
      balanced_accuracy <- sum(recall) / length(recall)
    }
    
    # Calculate accuracy
    
    accuracy <- sum(diag(cm)) / sum(cm)
    
    if(use_balanced_accuracy){
      mainOuts <- data.frame(accuracy = accuracy, 
                             balanced_accuracy = balanced_accuracy)
    } else{
      mainOuts <- data.frame(accuracy = accuracy)
    }
    
    mainOuts <- mainOuts%>%
      dplyr::mutate(category = "Main")
  }
  
  if(use_empirical_null){
    
    if(null_testing_method == "NullModelFits"){
      
      # Run procedure
      
      nullOuts <- 1:num_permutations %>%
        purrr::map_df( ~ fit_empirical_null_models(data = tmp, 
                                                   s = .x,
                                                   test_method = test_method,
                                                   theControl = fitControl,
                                                   pb = NULL,
                                                   univariable = TRUE,
                                                   use_balanced_accuracy = use_balanced_accuracy)) %>%
        dplyr::mutate(category = "Null")
      
      finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
      
    } else{
      finalOuts <- mainOuts
    }
    
  } else{
    finalOuts <- mainOuts
  }
  
  finalOuts <- finalOuts %>%
    dplyr::mutate(feature = names(tmp[2]))
  
  return(finalOuts)
}

#--------------------------
# Calculation of statistics
# for empirical nulls
#--------------------------

calculate_against_null_vector <- function(nulls, main_matrix, main_matrix_balanced = NULL, x, p_value_method, use_balanced_accuracy){
  
  if(use_balanced_accuracy) {
    
    true_val_acc <- main_matrix %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    true_val_bal_acc <- main_matrix_balanced %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    stopifnot(length(true_val_acc) == 1)
    stopifnot(length(true_val_bal_acc) == 1)
    
    # Null models
    
    nulls_acc <- nulls$accuracy
    nulls_bal_acc <- nulls$balanced_accuracy
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls_acc) == 0){
      p_value_acc <- NA
      message("Insufficient variance to calculate p-value, returning NA.")
      
    } else{
      
      if(p_value_method == "empirical"){
        
        # Use ECDF to calculate p-value
        
        fn <- stats::ecdf(nulls_acc)
        p_value_acc <- 1 - fn(true_val_acc)
        
      } else{
        
        # Calculate p-value from Gaussian with null distribution parameters
        
        p_value_acc <- stats::pnorm(true_val_acc, mean = mean(nulls_acc), sd = stats::sd(nulls_acc), lower.tail = FALSE)
      }
    }
    
    if(stats::sd(nulls_bal_acc) == 0){
      p_value_bal_acc <- NA
      message("Insufficient variance to calculate p-value, returning NA.")
      
    } else{
      
      if(p_value_method == "empirical"){
        
        # Use ECDF to calculate p-value
        
        fn_bal_acc <- stats::ecdf(nulls_bal_acc)
        p_value_bal_acc <- 1 - fn_bal_acc(true_val_bal_acc)
        
      } else{
        
        # Calculate p-value from Gaussian with null distribution parameters
        
        p_value_bal_acc <- stats::pnorm(true_val_bal_acc, mean = mean(nulls_bal_acc), sd = stats::sd(nulls_bal_acc), lower.tail = FALSE)
      }
    }
    
    tmp_outputs <- data.frame(feature = names(main_matrix)[x],
                              accuracy = true_val_acc,
                              p_value_accuracy = p_value_acc,
                              balanced_accuracy = true_val_bal_acc,
                              p_value_balanced_accuracy = p_value_bal_acc)
    
  } else{
    
    true_val_acc <- main_matrix %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    stopifnot(length(true_val_acc) == 1)
    nulls_acc <- nulls
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls_acc) == 0){
      p_value_acc <- NA
      message("Insufficient variance to calculate p-value, returning NA.")
      
    } else{
      
      if(p_value_method == "empirical"){
        
        # Use ECDF to calculate p-value
        
        fn <- stats::ecdf(nulls_acc)
        p_value_acc <- 1 - fn(true_val_acc)
        
      } else{
        
        # Calculate p-value from Gaussian with null distribution parameters
        
        p_value_acc <- stats::pnorm(true_val_acc, mean = mean(nulls_acc), sd = stats::sd(nulls_acc), lower.tail = FALSE)
      }
    }
    
    tmp_outputs <- data.frame(feature = names(main_matrix)[x],
                              accuracy = true_val_acc,
                              p_value_accuracy = p_value_acc)
  }
  
  return(tmp_outputs)
}

# Unpooled

calculate_unpooled_null <- function(main_matrix, main_matrix_balanced = NULL, x, p_value_method, use_balanced_accuracy = FALSE){
  
  if(use_balanced_accuracy) {
    
    true_val_acc <- main_matrix %>%
      dplyr::filter(.data$category == "Main") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    true_val_bal_acc <- main_matrix_balanced %>%
      dplyr::filter(.data$category == "Main") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    stopifnot(length(true_val_acc) == 1)
    stopifnot(length(true_val_bal_acc) == 1)
    
    # Null models
    
    nulls_acc <- main_matrix %>%
      dplyr::filter(.data$category == "Null") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    nulls_bal_acc <- main_matrix_balanced %>%
      dplyr::filter(.data$category == "Null") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls_acc) == 0){
      p_value_acc <- NA
      message("Insufficient variance to calculate p-value, returning NA.")
      
    } else{
      
      if(p_value_method == "empirical"){
        
        # Use ECDF to calculate p-value
        
        fn <- stats::ecdf(nulls_acc)
        p_value_acc <- 1 - fn(true_val_acc)
        
      } else{
        
        # Calculate p-value from Gaussian with null distribution parameters
        
        p_value_acc <- stats::pnorm(true_val_acc, mean = mean(nulls_acc), sd = stats::sd(nulls_acc), lower.tail = FALSE)
      }
    }
    
    if(stats::sd(nulls_bal_acc) == 0){
      p_value_bal_acc <- NA
      message("Insufficient variance to calculate p-value, returning NA.")
      
    } else{
      
      if(p_value_method == "empirical"){
        
        # Use ECDF to calculate p-value
        
        fn_bal_acc <- stats::ecdf(nulls_bal_acc)
        p_value_bal_acc <- 1 - fn_bal_acc(true_val_bal_acc)
        
      } else{
        
        # Calculate p-value from Gaussian with null distribution parameters
        
        p_value_bal_acc <- stats::pnorm(true_val_bal_acc, mean = mean(nulls_bal_acc), sd = stats::sd(nulls_bal_acc), lower.tail = FALSE)
      }
    }
    
    tmp_outputs <- data.frame(feature = names(main_matrix)[x],
                              accuracy = true_val_acc,
                              p_value_accuracy = p_value_acc,
                              balanced_accuracy = true_val_bal_acc,
                              p_value_balanced_accuracy = p_value_bal_acc)
    
  } else{
    
    true_val_acc <- main_matrix %>%
      dplyr::filter(.data$category == "Main") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    stopifnot(length(true_val_acc) == 1)
    
    nulls_acc <- main_matrix %>%
      dplyr::filter(.data$category == "Null") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls_acc) == 0){
      p_value_acc <- NA
      message("Insufficient variance to calculate p-value, returning NA.")
      
    } else{
      
      if(p_value_method == "empirical"){
        
        # Use ECDF to calculate p-value
        
        fn <- stats::ecdf(nulls_acc)
        p_value_acc <- 1 - fn(true_val_acc)
        
      } else{
        
        # Calculate p-value from Gaussian with null distribution parameters
        
        p_value_acc <- stats::pnorm(true_val_acc, mean = mean(nulls_acc), sd = stats::sd(nulls_acc), lower.tail = FALSE)
      }
    }
    
    tmp_outputs <- data.frame(feature = names(main_matrix)[x],
                              accuracy = true_val_acc,
                              p_value_accuracy = p_value_acc)
  }
  
  return(tmp_outputs)
}

#------------------------
# Binomial GLM extraction
#------------------------

gather_binomial_info <- function(data, x){
  
  tmp <- clean_by_feature(data = data, x = x)
  mod <- stats::glm(formula = stats::formula(paste0("group ~ ", colnames(tmp[3]))), data = tmp, family = stats::binomial())
  
  tmp <- data.frame(feature = as.character(mod$terms[[3]]),
                    statistic_value = as.numeric(summary(mod)$coefficients[,3][2]),
                    p_value = as.numeric(summary(mod)$coefficients[,4][2]))
  
  return(tmp)
}

#------------------------------
# t-test and wilcox comparisons
#------------------------------

mean_diff_calculator <- function(data, x, method){
  
  tmp <- clean_by_feature(data = data, x = x)
  
  if(method == "t-test"){
    results <- stats::t.test(formula = stats::formula(paste0(colnames(tmp[3]), " ~ group")), data = tmp)
  } else{
    results <- stats::wilcox.test(formula = stats::formula(paste0(colnames(tmp[3]), " ~ group")), data = tmp)
  }
  
  results <- data.frame(feature = results$data.name, 
                        statistic_value = results$statistic, 
                        p_value = results$p.value)
  return(results)
}

#--------------------
# Cleaning by feature
#--------------------

clean_by_feature <- function(data, x){
  
  themethod <- names(data[x])
  
  tmp_cleaner <- data %>%
    dplyr::select(c(.data$id, .data$group, dplyr::all_of(x)))
  
  ncols <- ncol(tmp_cleaner)
  
  # Delete features that are all NaNs and features with constant values
  
  tmp_cleaner <- tmp_cleaner %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::select(mywhere(~dplyr::n_distinct(.) > 1))
  
  if(ncol(tmp_cleaner) < ncols){
    message(paste0("Dropped ", ncols - ncol(tmp_cleaner), "/", ncol(tmp_cleaner), " features from ", themethod, " due to containing NAs or only a constant."))
  }
  
  # Check NAs
  
  nrows <- nrow(tmp_cleaner)
  
  tmp_cleaner <- tmp_cleaner %>%
    tidyr::drop_na()
  
  if(nrow(tmp_cleaner) < nrows){
    message(paste0("Dropped ", nrows - nrow(tmp_cleaner), " unique IDs due to NA values."))
  }
  
  return(tmp_cleaner)
}

#-------------- Main exported function ---------------

#' Fit a classifier to feature matrix to extract top performers
#' @importFrom rlang .data
#' @import dplyr
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom stats glm binomial sd wilcox.test t.test ecdf pnorm formula predict
#' @importFrom purrr map map_df possibly
#' @importFrom janitor clean_names
#' @importFrom caret preProcess train confusionMatrix
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to \code{"id"}
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to \code{"group"}
#' @param test_method the algorithm to use for quantifying class separation. Defaults to \code{"gaussprRadial"}. Should be either \code{"t-test"}, \code{"wilcox"}, or \code{"binomial logistic"} for two-class problems to obtain exact statistics, or a valid \code{caret} classification model for everything else 
#' @param use_balanced_accuracy a Boolean specifying whether to use balanced accuracy as the summary metric for caret model training. Defaults to \code{FALSE}
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates if a \code{caret} model is specified for \code{test_method}. Defaults to \code{ FALSE}
#' @param num_folds an integer specifying the number of k-folds to perform if \code{use_k_fold} is set to \code{TRUE}. Defaults to \code{10}
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values if a \code{caret} model is specified for \code{test_method}. Defaults to \code{FALSE}
#' @param null_testing_method a string specifying the type of statistical method to use to calculate p-values. Defaults to \code{model free shuffles}
#' @param p_value_method a string specifying the method of calculating p-values. Defaults to \code{"empirical"}
#' @param num_permutations an integer specifying the number of class label shuffles to perform if \code{use_empirical_null} is \code{TRUE}. Defaults to \code{50}
#' @param pool_empirical_null a Boolean specifying whether to use the pooled empirical null distribution of all features or each features' individual empirical null distribution if a \code{caret} model is specified for \code{test_method} use_empirical_null is \code{TRUE}. Defaults to \code{FALSE}
#' @param seed fixed number for R's random number generator to ensure reproducibility
#' @return an object of class dataframe containing results
#' @author Trent Henderson
#' @export
#' 

fit_single_feature_classifier <- function(data, test_method = "gaussprRadial", use_balanced_accuracy = FALSE,
                                          use_k_fold = FALSE, num_folds = 10, 
                                          use_empirical_null = FALSE, null_testing_method = c("ModelFreeShuffles", "NullModelFits"),
                                          p_value_method = c("empirical", "gaussian"), num_permutations = 50,
                                          pool_empirical_null = FALSE, seed = 123){
  
  #------------------------------------------------------------------------
  #----------------- NOTE: ALL ARG CHECKS ARE PERFORMED IN ----------------
  #----------------- theft::compute_top_features AS THIS IS ---------------
  #----------------- JUST A HELPER FUNCTION!!!!!!--------------------------
  #------------------------------------------------------------------------
  
  num_classes <- length(unique(data$group)) # Get number of classes in the data
  
  #------------- Preprocess data --------------
  
  # Widening for model matrix
  
  data_id <- data %>%
    dplyr::mutate(names = paste0(.data$method, "_", .data$names)) %>%
    dplyr::select(-c(.data$method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    janitor::clean_names()
  
  #------------- Fit classifiers -------------
  
  #---------------------
  # Set up useful method 
  # information
  #---------------------
  
  if(test_method == "t-test"){
    
    classifier_name <- "Welch Two Sample t-test"
    statistic_name <- "t-test statistic"
    
  } else if(test_method == "wilcox") {
    
    classifier_name <- "Mann-Whitney-Wilcoxon Test"
    statistic_name <- "Mann-Whitney-Wilcoxon Test statistic"
    
  } else if(test_method == "BinomialLogistic") {
    
    classifier_name <- "Binomial logistic regression"
    statistic_name <- "Binomial logistic coefficient z-test"
    
  } else{
    
    classifier_name <- test_method
    statistic_name <- ifelse(use_balanced_accuracy, "Mean classification accuracy and balanced classification accuracy", "Mean classification accuracy")
  }
  
  #-----------------------------
  # Iterate through each feature 
  # and fit appropriate model
  #-----------------------------
  
  if(test_method == "t-test"){
    
    mean_diff_calculator_safe <- purrr::possibly(mean_diff_calculator, otherwise = NULL)
    
    output <- 3:ncol(data_id) %>%
      purrr::map_df(~ mean_diff_calculator_safe(data = data_id, x = .x, method = test_method)) %>%
      dplyr::distinct() %>%
      dplyr::mutate(feature = gsub(" .*", "\\1", .data$feature),
                    classifier_name = classifier_name,
                    statistic_name = statistic_name)
    
    return(output)
    
  } else if (test_method == "wilcox"){
    
    mean_diff_calculator_safe <- purrr::possibly(mean_diff_calculator, otherwise = NULL)
    
    output <- 3:ncol(data_id) %>%
      purrr::map_df(~ mean_diff_calculator_safe(data = data_id, x = .x, method = test_method)) %>%
      dplyr::distinct() %>%
      dplyr::mutate(feature = gsub(" .*", "\\1", .data$feature),
                    classifier_name = classifier_name,
                    statistic_name = statistic_name)
    
    return(output)
    
  } else if (test_method == "BinomialLogistic"){
    
    gather_binomial_info_safe <- purrr::possibly(gather_binomial_info, otherwise = NULL)
    
    data_id <- data_id %>%
      dplyr::mutate(group = as.factor(.data$group))
    
    output <- 3:ncol(data_id) %>%
      purrr::map_df(~ gather_binomial_info_safe(data_id, .x)) %>%
      dplyr::mutate(classifier_name = classifier_name,
                    statistic_name = statistic_name)
    
    return(output)
    
  } else {
    
    # Set up progress bar for {purrr::map} iterations
    
    pb <- dplyr::progress_estimated(length(3:ncol(data_id)))
    
    # Very important coffee console message
    
    if(use_empirical_null & null_testing_method == "ModelFreeShuffles"){
      message("This will take a while. Great reason to go grab a coffee and relax ^_^")
    }
    
    # Compute accuracies for each feature
    
    fit_single_feature_models_safe <- purrr::possibly(fit_single_feature_models, otherwise = NULL)
    
    output <- 3:ncol(data_id) %>%
      purrr::map(~ fit_single_feature_models_safe(data = data_id, 
                                                  test_method = test_method,
                                                  use_balanced_accuracy = use_balanced_accuracy,
                                                  use_k_fold = use_k_fold,
                                                  num_folds = num_folds,
                                                  use_empirical_null = use_empirical_null,
                                                  null_testing_method = null_testing_method,
                                                  num_permutations = num_permutations,
                                                  feature = .x,
                                                  pb = pb,
                                                  seed = seed))
    
    output <- output[!sapply(output, is.null)]
    output <- do.call(rbind, output)
    
    if(nrow(output) < (ncol(data_id) - 2)){
      difference <- (ncol(data_id) - 2) - nrow(output)
      message(paste0(difference, " features failed due to NAs or other errors."))
    }
    
    # Run nulls if random shuffles are to be used
    
    if(null_testing_method == "ModelFreeShuffles"){
      
      # Run random shuffles procedure
      
      x_prep <- data_id %>%
        dplyr::select(c(.data$id, .data$group)) %>%
        dplyr::distinct() %>%
        dplyr::pull(.data$group)
      
      nullOuts <- simulate_null_acc(x = x_prep, num_permutations = num_permutations, use_balanced_accuracy) %>%
        dplyr::mutate(category = "Null",
                      feature = "ModelFreeShuffles")
      
      if(use_k_fold){
        nullOuts <- nullOuts %>%
          dplyr::mutate(accuracy_sd = NA)
        
        if(use_balanced_accuracy){
          nullOuts <- nullOuts %>%
            dplyr::mutate(balanced_accuracy_sd = NA)
        }
      }
      
      output <- dplyr::bind_rows(output, nullOuts)
    }
    
    # Compute statistics for each feature against empirical null distribution
    
    if(use_empirical_null){
      
      if(pool_empirical_null){
        
        # Set up vector of null accuracies across all features
        
        if(use_balanced_accuracy){
          
          nulls <- output %>%
            dplyr::filter(.data$category == "Null") %>%
            dplyr::select(c(.data$accuracy, .data$balanced_accuracy))
          
          # Widen main results matrix
          
          main_matrix <- output %>%
            dplyr::select(c(.data$category, .data$feature, .data$accuracy)) %>%
            dplyr::filter(.data$category == "Main") %>%
            dplyr::group_by(.data$feature) %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
            dplyr::select(-c(.data$id))
          
          main_matrix_balanced <- output %>%
            dplyr::select(c(.data$category, .data$feature, .data$balanced_accuracy)) %>%
            dplyr::filter(.data$category == "Main") %>%
            dplyr::group_by(.data$feature) %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "balanced_accuracy") %>%
            dplyr::select(-c(.data$id))
          
          # Calculate p-values for each feature
          
          feature_statistics <- 2:ncol(main_matrix) %>%
            purrr::map_df(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = main_matrix_balanced,
                                                          x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
          
        } else{
          
          nulls <- output %>%
            dplyr::filter(.data$category == "Null") %>%
            dplyr::select(c(.data$accuracy))
          
          # Widen main results matrix
          
          main_matrix <- output %>%
            dplyr::select(c(.data$category, .data$feature, .data$accuracy)) %>%
            dplyr::filter(.data$category == "Main") %>%
            dplyr::group_by(.data$feature) %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
            dplyr::select(-c(.data$id))
          
          # Calculate p-values for each feature
          
          feature_statistics <- 2:ncol(main_matrix) %>%
            purrr::map_df(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = NULL,
                                                          x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
        }
        
      } else{
        
        if(null_testing_method == "NullModelFits"){
          
          if(use_balanced_accuracy){
            
            main_matrix <- output %>%
              dplyr::select(c(.data$category, .data$feature, .data$accuracy)) %>%
              dplyr::group_by(.data$feature) %>%
              dplyr::mutate(id = dplyr::row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(.data$id))
            
            main_matrix_balanced <- output %>%
              dplyr::select(c(.data$category, .data$feature, .data$balanced_accuracy)) %>%
              dplyr::group_by(.data$feature) %>%
              dplyr::mutate(id = dplyr::row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "balanced_accuracy") %>%
              dplyr::select(-c(.data$id))
            
            # Calculate p-values for each feature
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map_df(~ calculate_unpooled_null(main_matrix = main_matrix, main_matrix_balanced = main_matrix_balanced, 
                                                      x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
            
          } else{
            
            main_matrix <- output %>%
              dplyr::select(c(.data$category, .data$feature, .data$accuracy)) %>%
              dplyr::group_by(.data$feature) %>%
              dplyr::mutate(id = dplyr::row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(.data$id))
            
            # Calculate p-values for each feature
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map_df(~ calculate_unpooled_null(main_matrix = main_matrix, main_matrix_balanced = NULL, 
                                                      x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
          }
          
        } else{
          
          # Set up vector of null accuracies across all features
          
          if(use_balanced_accuracy){
            
            nulls <- output %>%
              dplyr::filter(.data$category == "Null") %>%
              dplyr::select(c(.data$accuracy, .data$balanced_accuracy))
            
            main_matrix <- output %>%
              dplyr::select(c(.data$category, .data$feature, .data$accuracy)) %>%
              dplyr::filter(.data$category == "Main") %>%
              dplyr::group_by(.data$feature) %>%
              dplyr::mutate(id = dplyr::row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(.data$id))
            
            main_matrix_balanced <- output %>%
              dplyr::select(c(.data$category, .data$feature, .data$balanced_accuracy)) %>%
              dplyr::filter(.data$category == "Main") %>%
              dplyr::group_by(.data$feature) %>%
              dplyr::mutate(id = dplyr::row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "balanced_accuracy") %>%
              dplyr::select(-c(.data$id))
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map_df(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = main_matrix_balanced, 
                                                            x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
            
          } else{
            
            nulls <- output %>%
              dplyr::filter(.data$category == "Null") %>%
              dplyr::pull(.data$accuracy)
            
            main_matrix <- output %>%
              dplyr::select(c(.data$category, .data$feature, .data$accuracy)) %>%
              dplyr::filter(.data$category == "Main") %>%
              dplyr::group_by(.data$feature) %>%
              dplyr::mutate(id = dplyr::row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(.data$id))
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map_df(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = NULL, 
                                                            x = .x, p_value_method = p_value_method,
                                                            use_balanced_accuracy = use_balanced_accuracy))
          }
        }
      }
      
      # Bind together
      
      feature_statistics <- feature_statistics %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
    } else{
      
      feature_statistics <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
    }
    return(feature_statistics)
  }
}
