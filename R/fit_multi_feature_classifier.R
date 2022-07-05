#--------------- Helper functions ----------------

#-----------------------
# Classification metrics
#-----------------------

# Recall (for use in computing balanced classification accuracy)

calculate_recall <- function(matrix, x){
  tp <- as.numeric(matrix[x, x])
  fn <- sum(matrix[x, -x])
  
  # Add a catch for when 0s occupy the entire row in the confusion matrix
  # NOTE: Is this the correct way to handle? Seems consistent with {caret}'s default matrix
  
  if(tp + fn == 0){
    recall <- 0
  } else{
    recall <- tp / (tp + fn)
  }
  return(recall)
}

# Four MECE parts of the confusion matrix (TP, FP, TN, FN)

calculate_cm_stats <- function(matrix, x){
  tp <- as.numeric(matrix[x, x])
  fp <- sum(matrix[-x, x])
  tn <- sum(matrix[-x, -x])
  fn <- sum(matrix[x, -x])
  mymat <- matrix(c(tp, fp, tn, fn), nrow = 1, ncol = 4)
  return(mymat)
}

#----------------
# Random shuffles
#----------------

calculate_accuracy <- function(x, seed, use_balanced_accuracy, pb){
  
  # Print {purrr} iteration progress updates in the console
  
  pb$tick()$print()
  
  # Randomly shuffle class labels and generate confusion matrix
  
  set.seed(seed)
  y <- sample(x, replace = FALSE)
  u <- dplyr::union(y, x)
  mytable <- table(factor(y, u), factor(x, u))
  cm <- t(as.matrix(caret::confusionMatrix(mytable)$table)) # Transpose as {caret} has reversed format
  
  if(use_balanced_accuracy){
    
    # Calculate balanced accuracy from confusion matrix as the average of class recalls as per https://arxiv.org/pdf/2008.05756.pdf
    
    recall <- 1:nrow(cm) %>%
      purrr::map(~ calculate_recall(cm, x = .x)) %>%
      unlist()
    
    balanced_accuracy <- sum(recall) / length(recall)
  }
  
  # Calculate accuracy
  
  accuracy <- sum(diag(cm)) / sum(cm)
  
  # Return results
  
  if(use_balanced_accuracy){
    out <- data.frame(accuracy = accuracy, 
                      balanced_accuracy = balanced_accuracy)
  } else{
    out <- data.frame(accuracy = accuracy)
  }
  return(out)
}

simulate_null_acc <- function(x, num_permutations = 10000, use_balanced_accuracy){
  
  # Run function over num_permutations
  
  pb <- dplyr::progress_estimated(num_permutations)
  
  outs <- 1:num_permutations %>%
    purrr::map_df(~ calculate_accuracy(x, seed = .x, use_balanced_accuracy = use_balanced_accuracy, pb = pb))

  return(outs)
}

#--------------
# Model fitting
#--------------

# Function for returning accuracies over the train procedure

extract_prediction_accuracy <- function(mod, use_balanced_accuracy = FALSE) {
  
  results <- as.data.frame(mod$results)
  
  if (use_balanced_accuracy) {
    
    results <- results %>%
      dplyr::select(c(.data$Accuracy, .data$AccuracySD, .data$Balanced_Accuracy, .data$Balanced_AccuracySD)) %>%
      janitor::clean_names() %>%
      dplyr::slice_max(.data$balanced_accuracy, n = 1) # Catches cases where multiple results are returned by {caret} in `mod`
    
  } else {
    
    results <- results %>%
      dplyr::select(c(.data$Accuracy, .data$AccuracySD)) %>%
      janitor::clean_names() %>%
      dplyr::slice_max(.data$accuracy, n = 1) # Catches cases where multiple results are returned by {caret} in `mod`
  }
  
  return(results)
}

# Function for iterating over random shuffle permutations of class labels

fit_empirical_null_models <- function(data, s, test_method, theControl, pb = NULL, univariable = FALSE, use_balanced_accuracy = FALSE){
  
  # Print {purrr} iteration progress updates in the console
  
  if(!is.null(pb)){
    pb$tick()$print()
  } else{
  }
  
  # Null shuffles and computations
  
  y <- data %>% dplyr::pull(.data$group)
  y <- as.character(y)
  
  set.seed(s)
  shuffles <- sample(y, replace = FALSE)
  
  shuffledtest <- data %>%
    dplyr::mutate(group = shuffles) %>%
    dplyr::mutate(group = as.factor(.data$group))
  
  if(univariable){
    processes <- c("center", "scale")
  } else{
    processes <- c("center", "scale", "nzv")
  }
  
  modNull <- caret::train(group ~ .,
                          data = shuffledtest,
                          method = test_method,
                          trControl = theControl,
                          preProcess = processes)
  
  if(theControl$method == "none"){
    
    u <- dplyr::union(predict(modNull, newdata = shuffledtest), shuffledtest$group)
    mytable <- table(factor(stats::predict(modNull, newdata = shuffledtest), u), factor(shuffledtest$group, u))
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
      null_models <- data.frame(accuracy = accuracy, 
                                balanced_accuracy = balanced_accuracy)
    } else{
      null_models <- data.frame(accuracy = accuracy)
    }
    
    null_models <- null_models%>%
      dplyr::mutate(category = "Null")
    
  } else{
    null_models <- extract_prediction_accuracy(mod = modNull, use_balanced_accuracy = use_balanced_accuracy)
  }
  
  return(null_models)
}

#-------------------------------------
# Calculate balanced accuracy in caret
#-------------------------------------

calculate_balanced_accuracy <- function(data, lev = NULL, model = NULL) {
  
  # Calculate balanced accuracy from confusion matrix as the average of class recalls as per https://arxiv.org/pdf/2008.05756.pdf
  
  cm <- t(as.matrix(caret::confusionMatrix(data$pred, data$obs)$table))
  
  recall <- 1:nrow(cm) %>%
    purrr::map(~ calculate_recall(cm, x = .x)) %>%
    unlist()
  
  balanced_accuracy <- sum(recall) / length(recall)
  
  # Calculate accuracy
  
  accuracy <- sum(diag(cm)) / sum(cm)
  
  # Return results
  
  out <- c(accuracy, balanced_accuracy)
  names(out) <- c("Accuracy", "Balanced_Accuracy")
  return(out)
}

#--------------
# Model fitting
#--------------

fit_multi_feature_models <- function(data, test_method, use_balanced_accuracy, use_k_fold, num_folds, use_empirical_null, null_testing_method, num_permutations, set = NULL, seed){
  
  # Set up input matrices
  
  if(!is.null(set)){
    
    message(paste0("\nCalculating models for ", set))
    
    tmp <- data %>%
      dplyr::filter(.data$method == set) %>%
      dplyr::select(-c(.data$method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
    
  } else{
    
    tmp <- data %>%
      dplyr::select(-c(.data$method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(.data$id))
  }
  
  # Fit models
  
  set.seed(seed)
  
  if(use_k_fold){
    
    # Train model
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
                        preProcess = c("center", "scale", "nzv"))
    
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
                        preProcess = c("center", "scale", "nzv"))
    
    # Get main predictions
    
    u <- dplyr::union(predict(mod, newdata = tmp), tmp$group)
    mytable <- table(factor(stats::predict(mod, newdata = tmp), u), factor(tmp$group, u))
    cm <- t(as.matrix(caret::confusionMatrix(mytable)$table))
    
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
      
      # Set up progress bar for {purrr::map} iterations
      
      pb <- dplyr::progress_estimated(length(1:num_permutations))
      
      # Run procedure
      
      nullOuts <- 1:num_permutations %>%
        purrr::map_df( ~ fit_empirical_null_models(data = tmp,
                                                s = .x,
                                                test_method = test_method,
                                                theControl = fitControl,
                                                pb = pb,
                                                univariable = FALSE,
                                                use_balanced_accuracy = use_balanced_accuracy)) %>%
        dplyr::mutate(category = "Null")
      
      finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
      
    } else{
      finalOuts <- mainOuts
    }
    
  } else{
    finalOuts <- mainOuts
  }
  
  if(!is.null(set)){
    finalOuts <- finalOuts %>%
      dplyr::mutate(method = set,
                    num_features_used = (ncol(tmp) - 1))
  }
  
  return(finalOuts)
}

#--------------------
# p-value calculation
#--------------------

calculate_multivariable_statistics <- function(data, set = NULL, p_value_method, use_balanced_accuracy = FALSE){
  
  # Wrangle vectors
  
  if(!is.null(set)){
    vals <- data %>%
      dplyr::filter(.data$method %in% c(set, "ModelFreeShuffles"))
  } else{
    vals <- data
  }
  
  #------- Balanced accuracy -------
  
  # Main models
  
  if (use_balanced_accuracy) {
    main <- dplyr::filter(vals, .data$category == "Main")
    true_val_acc <- main$accuracy
    true_val_bal_acc <- main$balanced_accuracy
    
    stopifnot(length(true_val_acc) == 1)
    stopifnot(length(true_val_bal_acc) == 1)
    
    # Null models
    
    nulls <- dplyr::filter(vals, .data$category == "Null")
    nulls_acc <- nulls$accuracy
    nulls_bal_acc <- nulls$balanced_accuracy
    
    #-------------------
    # Calculate p-values
    #-------------------
    
    # Empirical p-value
    
    if (p_value_method == "empirical") {
      
      # Catch cases when SD = 0 and then use ECDF to calculate p-values
      
      if(stats::sd(nulls_acc) == 0){
        p_value_acc <- NA
        message("Insufficient variance to calculate p-value, returning NA.")
      } else{
        fn_acc <- stats::ecdf(nulls_acc)
        p_value_acc <- 1 - fn_acc(true_val_acc)
      }
      
      if(stats::sd(nulls_bal_acc) == 0){
        p_value_bal_acc <- NA
        message("Insufficient variance to calculate p-value, returning NA.")
      } else{
        fn_bal_acc <- stats::ecdf(nulls_bal_acc)
        p_value_bal_acc <- 1 - fn_bal_acc(true_val_bal_acc)
      }
      
    } else {
      
      # Catch cases when SD = 0 and then use Gaussian PDF to calculate p-values
      
      if(stats::sd(nulls_acc) == 0){
        p_value_acc <- NA
        message("Insufficient variance to calculate p-value, returning NA.")
      } else{
        p_value_acc <- stats::pnorm(true_val_acc, 
                                    mean = mean(nulls_acc),
                                    sd = stats::sd(nulls_acc),
                                    lower.tail = FALSE)
      }
      
      if(stats::sd(nulls_bal_acc) == 0){
        p_value_bal_acc <- NA
        message("Insufficient variance to calculate p-value, returning NA.")
      } else{
        p_value_bal_acc <- stats::pnorm(true_val_bal_acc, 
                                        mean = mean(nulls_bal_acc),
                                        sd = stats::sd(nulls_bal_acc),
                                        lower.tail = FALSE)
      }
    }
    
    tmp_outputs <- data.frame(accuracy = true_val_acc,
                              p_value_accuracy = p_value_acc,
                              balanced_accuracy = true_val_bal_acc,
                              p_value_balanced_accuracy = p_value_bal_acc)
    
    if(!is.null(set)){
      tmp_outputs <- tmp_outputs %>%
        dplyr::mutate(method = set) %>%
        dplyr::select(c(.data$method, .data$accuracy, .data$p_value_accuracy, .data$balanced_accuracy, .data$p_value_balanced_accuracy))
    }
    
  } else {
    
    true_val <- vals %>%
      dplyr::filter(.data$category == "Main") %>%
      dplyr::pull(.data$accuracy)
    
    stopifnot(length(true_val) == 1)
    
    nulls <- vals %>%
      dplyr::filter(.data$category == "Null") %>%
      dplyr::pull(.data$accuracy)
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls) == 0){
      p_value <- NA
      message("Insufficient variance to calculate p-value, returning NA.")
      
      tmp_outputs <- data.frame(accuracy = true_val,
                                p_value_accuracy = p_value)
      
      if(!is.null(set)){
        tmp_outputs <- tmp_outputs %>%
          dplyr::mutate(method = set) %>%
          dplyr::select(c(.data$method, .data$accuracy, .data$p_value_accuracy))
      }
      
    } else{
      
      if(p_value_method == "empirical"){
        
        # Use ECDF to calculate p-value
        
        fn <- stats::ecdf(nulls)
        p_value <- 1 - fn(true_val)
        
      } else{
        
        # Calculate p-value from Gaussian with null distribution parameters
        
        p_value <- stats::pnorm(true_val, mean = mean(nulls), sd = stats::sd(nulls), lower.tail = FALSE)
      }
      
      tmp_outputs <- data.frame(accuracy = true_val,
                                p_value_accuracy = p_value)
      
      if(!is.null(set)){
        tmp_outputs <- tmp_outputs %>%
          dplyr::mutate(method = set) %>%
          dplyr::select(c(.data$method, .data$accuracy, .data$p_value_accuracy))
      } 
    }
  }
  return(tmp_outputs)
}

#---------------
# 'where' helper
#---------------

# NOTE: This is from {tidyselect} but due to import limitations for CRAN (and it not being namespaced) it's rebuilt here
# See https://github.com/r-lib/tidyselect/blob/main/R/helpers-where.R for implementation

mywhere <- function(fn) {
  predicate <- rlang::as_function(fn)
  
  function(x, ...) {
    out <- predicate(x, ...)
    
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    
    out
  }
}

#------------------------------
# Pre-processing by feature set
#------------------------------

clean_by_set <- function(data, themethod = NULL){
  
  if(is.null(themethod)){
    tmp_cleaner <- data
    themethod <- "matrix of all features"
  } else{
    tmp_cleaner <- data %>%
      dplyr::filter(.data$method == themethod)
  }
  
  # Widening for model matrix
  
  tmp_cleaner <- tmp_cleaner %>%
    dplyr::mutate(names = paste0(.data$method, "_", .data$names)) %>%
    dplyr::select(-c(.data$method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")
  
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
  
  # Clean up column (feature) names so models fit properly (mainly an issue with SVM formula) and re-join set labels
  # and prep factor levels as names for {caret} if the 3 base two-class options aren't being used
  
  tmp_cleaner <- tmp_cleaner %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = 3:ncol(tmp_cleaner), names_to = "names", values_to = "values") %>%
    dplyr::mutate(method = gsub("_.*", "\\1", .data$names),
                  method = dplyr::case_when(
                    .data$method == "tsfel" ~ "TSFEL",
                    .data$method == "kats"  ~ "Kats",
                    TRUE                    ~ method)) %>%
    dplyr::mutate(group = as.factor(.data$group)) %>%
    dplyr::mutate(group = as.integer(.data$group),
                  group = paste0("Group_", .data$group),
                  group = make.names(.data$group),
                  group = as.factor(.data$group))
  
  return(tmp_cleaner)
}

#---------------- Main function ----------------

#' Fit a classifier to feature matrix using all features or all features by set
#' @importFrom rlang .data as_function is_bool abort
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr drop_na pivot_wider pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom stats sd reorder ecdf pnorm
#' @importFrom purrr map map_df
#' @importFrom janitor clean_names
#' @importFrom caret preProcess train confusionMatrix
#' @param data the dataframe containing the raw feature data as calculated by \code{theft::calculate_features}
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to \code{"id"}
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to \code{"group"}
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to \code{FALSE}
#' @param test_method the algorithm to use for quantifying class separation. Defaults to \code{"gaussprRadial"}
#' @param use_balanced_accuracy a Boolean specifying whether to use balanced accuracy as the summary metric for caret model training. Defaults to \code{FALSE}
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to \code{TRUE}
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if \code{use_k_fold} is set to \code{TRUE}. Defaults to \code{10}
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values. Defaults to \code{FALSE}
#' @param null_testing_method a string specifying the type of statistical method to use to calculate p-values. Defaults to \code{model free shuffles}
#' @param p_value_method a string specifying the method of calculating p-values. Defaults to \code{"empirical"}
#' @param num_permutations an integer specifying the number of class label shuffles to perform if \code{use_empirical_null} is \code{TRUE}. Defaults to \code{100}
#' @param seed fixed number for R's random number generator to ensure reproducibility
#' @return an object of class list containing dataframe summaries of the classification models and a \code{ggplot} object if \code{by_set} is \code{TRUE}
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
#' fit_multi_feature_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   by_set = FALSE,
#'   test_method = "gaussprRadial",
#'   use_balanced_accuracy = FALSE,
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   use_empirical_null = TRUE,
#'   null_testing_method = "ModelFreeShuffles",
#'   p_value_method = "gaussian",
#'   num_permutations = 50,
#'   seed = 123)
#' }
#'

fit_multi_feature_classifier <- function(data, id_var = "id", group_var = "group",
                                         by_set = FALSE, test_method = "gaussprRadial",
                                         use_balanced_accuracy = FALSE, use_k_fold = TRUE, num_folds = 10,
                                         use_empirical_null = FALSE, null_testing_method = c("ModelFreeShuffles", "NullModelFits"),
                                         p_value_method = c("empirical", "gaussian"), num_permutations = 100, seed = 123){
  
  #---------- Check arguments ------------
  
  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  expected_cols_3 <- "method"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')
  
  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features(). Please consider running this first and then passing the resultant dataframe to this function.")
  }
  
  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features(). Please consider running this first and then passing the resultant dataframe to this function.")
  }
  
  if(expected_cols_3 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features(). Please consider running this first and then passing the resultant dataframe to this function.")
  }
  
  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }
  
  if(!is.null(id_var) && !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  # Null testing options
  
  theoptions <- c("ModelFreeShuffles", "NullModelFits")
  
  if(null_testing_method == "model free shuffles"){
    message("'model free shuffles' is deprecated, please use 'ModelFreeShuffles' instead.")
    null_testing_method <- "ModelFreeShuffles"
  }
  
  if(null_testing_method == "null model fits"){
    message("'null model fits' is deprecated, please use 'NullModelFits' instead.")
    null_testing_method <- "NullModelFits"
  }
  
  if(is.null(null_testing_method) || missing(null_testing_method)){
    null_testing_method <- "ModelFreeShuffles"
    message("No argument supplied to null_testing_method. Using 'ModelFreeShuffles' as default.")
  }
  
  if(length(null_testing_method) != 1){
    stop("null_testing_method should be a single string of either 'ModelFreeShuffles' or 'NullModelFits'.")
  }
  
  if(null_testing_method %ni% theoptions){
    stop("null_testing_method should be a single string of either 'ModelFreeShuffles' or 'NullModelFits'.")
  }
  
  if(null_testing_method == "ModelFreeShuffles" && num_permutations < 1000){
    message("Null testing method 'ModelFreeShuffles' is fast. Consider running more permutations for more reliable results. N = 10000 is recommended.")
  }
  
  # p-value options
  
  theoptions_p <- c("empirical", "gaussian")
  
  if(is.null(p_value_method) || missing(p_value_method)){
    p_value_method <- "gaussian"
    message("No argument supplied to p_value_method Using 'gaussian' as default.")
  }
  
  if(length(p_value_method) != 1){
    stop("p_value_method should be a single string of either 'empirical' or 'gaussian'.")
  }
  
  if(p_value_method %ni% theoptions_p){
    stop("p_value_method should be a single string of either 'empirical' or 'gaussian'.")
  }
  
  # Seed
  
  if(is.null(seed) || missing(seed)){
    seed <- 123
    message("No argument supplied to seed, using 123 as default.")
  }
  
  #------------- Renaming columns -------------
  
  if (is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(.data$id, .data$group, .data$method, .data$names, .data$values))
  }
  
  num_classes <- length(unique(data_id$group)) # Get number of classes in the data
  
  if(num_classes < 2){
    stop("Your data has less than two unique classes. At least two are required to performed classification analysis.")
  }
  
  # Set defaults for classification method
  
  if((missing(test_method) || is.null(test_method))){
    test_method <- "gaussprRadial"
    message("test_method is NULL or missing, fitting 'gaussprRadial' by default.")
  }
  
  if(length(test_method) != 1){
    stop("test_method should be a single string specification of a classification model available in the `caret` package. 'svmLinear' or 'gaussprRadial' are recommended as starting points.")
  }
  
  # Splits and shuffles
  
  if(use_k_fold == TRUE && !is.numeric(num_folds)){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }
  
  if(use_empirical_null == TRUE && !is.numeric(num_permutations)){
    stop("num_permutations should be a postive integer. A minimum of 50 permutations is recommended.")
  }
  
  if(use_empirical_null == TRUE && num_permutations < 3){
    stop("num_permutations should be a positive integer >= 3 for empirical null calculations. A minimum of 50 permutations is recommended.")
  }
  
  if(use_k_fold == TRUE && num_folds < 1){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }
  
  #------------- Preprocess data --------------
  
  # NOTE: This performs NA checking and filtering by feature set to maximise features and IDs for each if `by_set = TRUE`
  
  if(by_set){
    
    message("Assessing feature values and unique IDs for NAs by individual set.")
    
    data_id <- unique(data_id$method) %>%
      purrr::map_df(~ clean_by_set(data = data_id, themethod = .x))
    
    # Create reference set for all feature aggregation option
    
    data_id_all <- clean_by_set(data = data_id, themethod = NULL)
    
  } else{
    message("Assessing feature values and unique IDs for NAs using matrix of all features.")
    data_id <- clean_by_set(data = data_id, themethod = NULL)
  }
  
  #------------- Fit models -------------------
  
  #---------------------
  # Set up useful method
  # information
  #---------------------
  
  classifier_name <- test_method
  statistic_name <- ifelse(use_balanced_accuracy, "Mean classification accuracy and balanced classification accuracy", "Mean classification accuracy")
  
  # Very important coffee console message
  
  if(null_testing_method == "ModelFreeShuffles" & num_permutations > 50){
    message("This will take a while. Great reason to go grab a coffee and relax ^_^")
  }
  
  if(by_set){
    
    sets <- unique(data_id$method)
    
    # Compute accuracies for each feature set
    
    output <- sets %>%
      purrr::map_df(~ fit_multi_feature_models(data = data_id,
                                               test_method = test_method,
                                               use_balanced_accuracy = use_balanced_accuracy,
                                               use_k_fold = use_k_fold,
                                               num_folds = num_folds,
                                               use_empirical_null = use_empirical_null,
                                               null_testing_method = null_testing_method,
                                               num_permutations = num_permutations,
                                               set = .x,
                                               seed = seed))
    
    output_all <- fit_multi_feature_models(data = data_id_all,
                                           test_method = test_method,
                                           use_balanced_accuracy = use_balanced_accuracy,
                                           use_k_fold = use_k_fold,
                                           num_folds = num_folds,
                                           use_empirical_null = use_empirical_null,
                                           null_testing_method = null_testing_method,
                                           num_permutations = num_permutations,
                                           set = NULL,
                                           seed = seed) %>%
      dplyr::mutate(method = "All features") %>%
      dplyr::mutate(num_features_used = length(unique(data_id_all$names)))
    
    output <- dplyr::bind_rows(output, output_all)
    
  } else{
    
    output <- fit_multi_feature_models(data = data_id,
                                       test_method = test_method,
                                       use_balanced_accuracy = use_balanced_accuracy,
                                       use_k_fold = use_k_fold,
                                       num_folds = num_folds,
                                       use_empirical_null = use_empirical_null,
                                       null_testing_method = null_testing_method,
                                       num_permutations = num_permutations,
                                       set = NULL,
                                       seed = seed)
  }
  
  # Run nulls if random shuffles are to be used
  
  if(null_testing_method == "ModelFreeShuffles"){
    
    # Run random shuffles procedure
    
    x_prep <- data_id %>%
      dplyr::select(c(.data$id, .data$group)) %>%
      dplyr::distinct() %>%
      dplyr::pull(.data$group)
    
    nullOuts <- simulate_null_acc(x = x_prep, num_permutations = num_permutations, use_balanced_accuracy = use_balanced_accuracy) %>%
      dplyr::mutate(category = "Null",
                    method = "ModelFreeShuffles",
                    num_features_used = NA)
    
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
  
  #--------------- Evaluate results ---------------
  
  if(by_set){
    
    # Get chance probability
    
    chance <- round((1 / length(unique(data_id$group)) * 100), digits = 2)
    
    # Define colour palette
    
    mypal <- c("catch22" = "#1B9E77",
               "feasts" = "#D95F02",
               "Kats" = "#7570B3",
               "tsfeatures" = "#E7298A",
               "TSFEL" = "#66A61E",
               "tsfresh" = "#E6AB02",
               "All features" = "grey50")
    
    #--------------
    # Draw bar plot
    #--------------
    
    # Wrangle data into tidy format to facet
    
    if(use_balanced_accuracy){
      
      if(use_k_fold){
       
        means <- output %>%
          dplyr::filter(.data$category == "Main") %>%
          dplyr::mutate(method = paste0(.data$method, " (", .data$num_features_used, ")")) %>%
          dplyr::select(c(.data$accuracy, .data$balanced_accuracy, .data$category, .data$method, .data$num_features_used)) %>%
          tidyr::pivot_longer(cols = c("accuracy", "balanced_accuracy"), names_to = "names", values_to = "statistic")
        
        sds <- output %>%
          dplyr::filter(.data$category == "Main") %>% 
          dplyr::mutate(method = paste0(.data$method, " (", .data$num_features_used, ")")) %>%
          dplyr::select(c(.data$accuracy_sd, .data$balanced_accuracy_sd, .data$category, .data$method, .data$num_features_used)) %>%
          tidyr::pivot_longer(cols = c("accuracy_sd", "balanced_accuracy_sd"), names_to = "names", values_to = "statistic_sd") %>%
          dplyr::mutate(names = gsub("_sd", "\\1", .data$names))
        
        stopifnot(nrow(means) == nrow(sds))
        
        accuracies <- means %>%
          dplyr::inner_join(sds, by = c("method" = "method", "category" = "category", "num_features_used" = "num_features_used", "names" = "names")) %>%
          dplyr::mutate(names = ifelse(.data$names == "accuracy", "Accuracy", "Balanced Accuracy"))
        
      } else{
        
        accuracies <- output %>%
          dplyr::filter(.data$category == "Main") %>%
          dplyr::mutate(method = paste0(.data$method, " (", .data$num_features_used, ")")) %>%
          dplyr::select(c(.data$accuracy, .data$balanced_accuracy, .data$category, .data$method, .data$num_features_used)) %>%
          tidyr::pivot_longer(cols = c("accuracy", "balanced_accuracy"), names_to = "names", values_to = "statistic") %>%
          dplyr::mutate(names = ifelse(.data$names == "accuracy", "Accuracy", "Balanced Accuracy"))
      }
    } else{
      
      if(use_k_fold){
       
        accuracies <- output %>%
          dplyr::filter(.data$category == "Main") %>%
          dplyr::mutate(method = paste0(.data$method, " (", .data$num_features_used, ")")) %>%
          dplyr::rename(statistic = .data$accuracy,
                        statistic_sd = .data$accuracy_sd) 
      } else{
        
        accuracies <- output %>%
          dplyr::filter(.data$category == "Main") %>%
          dplyr::mutate(method = paste0(.data$method, " (", .data$num_features_used, ")")) %>%
          dplyr::rename(statistic = .data$accuracy) 
      }
    }
    
    # Draw plot
    
    accuracies <- accuracies %>%
      dplyr::mutate(statistic = .data$statistic * 100) %>%
      dplyr::mutate(method_short = gsub(" .*", "\\1", .data$method))
    
    if(use_k_fold){
      
      accuracies <- accuracies %>%
        mutate(statistic_sd = .data$statistic_sd * 100) %>%
        dplyr::mutate(lower = .data$statistic - (1 * .data$statistic_sd),
                      upper = .data$statistic + (1 * .data$statistic_sd))
      
      FeatureSetResultsPlot <- accuracies %>%
        ggplot2::ggplot(ggplot2::aes(x = reorder(.data$method, -.data$statistic), y = .data$statistic, colour = .data$method_short)) +
        ggplot2::geom_hline(yintercept = chance, colour = "black", lty = "dashed", size = 1) +
        ggplot2::geom_point(size = 5) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), size = 1)
      
      # Expand y-axis if max (mean + (1*SD)) is > 100%
      
      if(max(accuracies$upper, na.rm = TRUE) >= 100){
        
        FeatureSetResultsPlot <- FeatureSetResultsPlot +
          ggplot2::scale_y_continuous(limits = c(0, 100),
                                      breaks = seq(from = 0, to = 120, by = 20),
                                      labels = function(x) paste0(x, "%"))
        
      } else{
        
        FeatureSetResultsPlot <- FeatureSetResultsPlot +
          ggplot2::scale_y_continuous(limits = c(0, 100),
                                      breaks = seq(from = 0, to = 100, by = 20),
                                      labels = function(x) paste0(x, "%"))
      }
      
      FeatureSetResultsPlot <- FeatureSetResultsPlot +
        ggplot2::labs(subtitle = "Number of features is indicated in parentheses. Error bars are +/- 1 times SD. Dashed line = chance")
      
    } else{
      
      FeatureSetResultsPlot <- accuracies %>%
        ggplot2::ggplot(ggplot2::aes(x = stats::reorder(.data$method, -.data$statistic), colour = .data$method_short)) +
        ggplot2::geom_hline(yintercept = chance, colour = "black", lty = "dashed", size = 1) +
        ggplot2::geom_point(size = 5) +
        ggplot2::labs(subtitle = "Number of features is indicated in parentheses. Dashed line = chance") +
        ggplot2::scale_y_continuous(limits = c(0, 100),
                                    breaks = seq(from = 0, to = 100, by = 20),
                                    labels = function(x) paste0(x, "%"))
    }
    
    FeatureSetResultsPlot <- FeatureSetResultsPlot +
      ggplot2::labs(title = "Classification accuracy by feature set",
                    y = "Classification accuracy (%)",
                    x = "Feature set",
                    fill = NULL,
                    colour = NULL) +
      ggplot2::scale_colour_manual(values = mypal) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    
    if(use_balanced_accuracy){
      
      # Some visual niceties based on number of sets in the analysis
      
      if(length(unique(accuracies$method)) > 2){
        FeatureSetResultsPlot <- FeatureSetResultsPlot +
          ggplot2::facet_wrap(~names, dir = "v")
      } else{
        FeatureSetResultsPlot <- FeatureSetResultsPlot +
          ggplot2::facet_wrap(~names)
      }
    }
    
    #-----------------
    # Compute p values
    #-----------------
    
    if(use_empirical_null){
      
      TestStatistics <- sets %>%
        purrr::map_df(~ calculate_multivariable_statistics(data = output, set = .x, p_value_method = p_value_method,
                                                        use_balanced_accuracy = use_balanced_accuracy)) %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
      
      output <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
      
      myList <- list(FeatureSetResultsPlot, TestStatistics, output)
      names(myList) <- c("FeatureSetResultsPlot", "TestStatistics", "RawClassificationResults")
      
    } else{
      
      output <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name) %>%
        dplyr::select(-c(.data$category))
      
      myList <- list(FeatureSetResultsPlot, output)
      names(myList) <- c("FeatureSetResultsPlot", "RawClassificationResults")
    }
  } else{
    
    if(use_empirical_null){
      
      TestStatistics <- calculate_multivariable_statistics(data = output, set = NULL, p_value_method = p_value_method,
                                                           use_balanced_accuracy = use_balanced_accuracy) %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
      
      output <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
      
      myList <- list(TestStatistics, output)
      names(myList) <- c("TestStatistics", "RawClassificationResults")
      
    } else{
      
      output <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name) %>%
        dplyr::select(-c(.data$category))
      
      myList <- list(output)
      names(myList) <- c("RawClassificationResults")
    }
  }
  return(myList)
}
