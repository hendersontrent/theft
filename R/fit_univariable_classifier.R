#--------------- Helper functions ----------------

#--------------
# Model fitting
#--------------

fit_univariable_models <- function(data, test_method, use_balanced_accuracy, use_k_fold, num_folds, use_empirical_null, null_testing_method, num_permutations, feature, pb){
  
  # Print {purrr} iteration progress updates in the console
  
  pb$tick()$print()
  
  tmp <- data %>%
    dplyr::select(c(group, dplyr::all_of(feature)))
  
  set.seed(123)
  
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
    cm <- as.matrix(caret::confusionMatrix(mytable)$table)
    
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
    
    if(null_testing_method == "null model fits"){
      
      # Run procedure
      
      nullOuts <- 1:num_permutations %>%
        purrr::map( ~ fit_empirical_null_models(data = tmp, 
                                                s = .x,
                                                test_method = test_method,
                                                theControl = fitControl,
                                                pb = NULL,
                                                univariable = TRUE))
      
      nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
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
      print("Insufficient variance to calculate p-value, returning NA.")
      
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
      print("Insufficient variance to calculate p-value, returning NA.")
      
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
                              p_value_balanced_accurary = p_value_bal_acc)
    
  } else{
    
    true_val_acc <- main_matrix %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    stopifnot(length(true_val_acc) == 1)
    nulls_acc <- nulls
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls_acc) == 0){
      p_value_acc <- NA
      print("Insufficient variance to calculate p-value, returning NA.")
      
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
      dplyr::filter(category == "Main") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    true_val_bal_acc <- main_matrix_balanced %>%
      dplyr::filter(category == "Main") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    stopifnot(length(true_val_acc) == 1)
    stopifnot(length(true_val_bal_acc) == 1)
    
    # Null models
    
    nulls_acc <- main_matrix %>%
      dplyr::filter(category == "Null") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    nulls_bal_acc <- main_matrix_balanced %>%
      dplyr::filter(category == "Null") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls_acc) == 0){
      p_value_acc <- NA
      print("Insufficient variance to calculate p-value, returning NA.")
      
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
      print("Insufficient variance to calculate p-value, returning NA.")
      
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
                              p_value_balanced_accurary = p_value_bal_acc)
    
  } else{
    
    true_val_acc <- main_matrix %>%
      dplyr::filter(category == "Main") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    stopifnot(length(true_val_acc) == 1)
    
    nulls_acc <- main_matrix %>%
      dplyr::filter(category == "Null") %>%
      dplyr::select(dplyr::all_of(x)) %>%
      dplyr::pull()
    
    # Catch cases when SD = 0
    
    if(stats::sd(nulls_acc) == 0){
      p_value_acc <- NA
      print("Insufficient variance to calculate p-value, returning NA.")
      
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
# helper
#------------------------

gather_binomial_info <- function(data, x){
  
  tmp <- data.frame(feature = as.character(data[[x]]$terms[[3]]),
                    statistic_value = as.numeric(summary(data[[x]])$coefficients[,3][2]),
                    p_value = as.numeric(summary(data[[x]])$coefficients[,4][2]))
  
  return(tmp)
}

#-------------- Main exported function ---------------

#' Fit a classifier to feature matrix to extract top performers
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom data.table rbindlist
#' @importFrom stats glm binomial sd wilcox.test t.test ecdf pnorm
#' @importFrom purrr map possibly
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
#' @param return_raw_estimates a Boolean (for testing purposes only -- will break \code{compute_top_features}!!) specifying whether to return the raw main and null model results
#' @return an object of class dataframe containing results
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22")
#'   
#' fit_univariable_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   test_method = "linear svm",
#'   use_balanced_accuracy = FALSE,
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   use_empirical_null = TRUE,
#'   null_testing_method = "model free shuffles",
#'   p_value_method = "empirical",
#'   num_permutations = 50,
#'   pool_empirical_null = FALSE,
#'   return_raw_estimates = FALSE) 
#' }
#' 

fit_univariable_classifier <- function(data, id_var = "id", group_var = "group",
                                       test_method = "gaussprRadial", use_balanced_accuracy = FALSE,
                                       use_k_fold = FALSE, num_folds = 10, 
                                       use_empirical_null = FALSE, null_testing_method = c("model free shuffles", "null model fits"),
                                       p_value_method = c("empirical", "gaussian"), num_permutations = 50,
                                       pool_empirical_null = FALSE, return_raw_estimates = FALSE){
  
  #---------- Check arguments ------------
  
  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  expected_cols_3 <- "method"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')
  
  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by calculate_features(). Please consider running this first and then passing the resultant dataframe in to this function.")
  }
  
  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by calculate_features(). Please consider running this first and then passing the resultant dataframe in to this function.")
  }
  
  if(expected_cols_3 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by calculate_features(). Please consider running this first and then passing the resultant dataframe in to this function.")
  }
  
  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }
  
  if(!is.null(id_var) && !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  # Null testing options
  
  theoptions <- c("model free shuffles", "null model fits")
  
  if(is.null(null_testing_method) || missing(null_testing_method)){
    null_testing_method <- "model free shuffles"
    message("No argument supplied to null_testing_method. Using 'model free shuffles' as default.")
  }
  
  if(length(null_testing_method) != 1){
    stop("null_testing_method should be a single string of either 'model free shuffles' or 'null model fits'.")
  }
  
  if(null_testing_method %ni% theoptions){
    stop("null_testing_method should be a single string of either 'model free shuffles' or 'null model fits'.")
  }
  
  if(null_testing_method == "model free shuffles" && pool_empirical_null){
    stop("'model free shuffles' and pooled empirical null are incompatible (pooled null combines each feature's null into a grand null and features don'tt get a null if 'model free shuffles' is used). Please respecify.")
  }
  
  if(null_testing_method == "model free shuffles" && num_permutations < 1000){
    message("Null testing method 'model free shuffles' is very fast. Consider running more permutations for more reliable results. N = 10000 is recommended.")
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
  
  #------------- Renaming columns -------------
  
  if (is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(id, group, method, names, values))
  }
  
  num_classes <- length(unique(data_id$group)) # Get number of classes in the data
  
  if(num_classes == 1){
    stop("Your data only has one class label. At least two are required to performed analysis.")
  }
  
  if(((missing(test_method) || is.null(test_method))) && num_classes == 2){
    test_method <- "t-test"
    message("test_method is NULL or missing. Running t-test for 2-class problem.")
  }
  
  if(((missing(test_method) || is.null(test_method))) && num_classes > 2){
    test_method <- "gaussprRadial"
    message("test_method is NULL or missing, fitting 'gaussprRadial' by default.")
  }
  
  if(test_method %in% c("t-test", "wilcox", "binomial logistic") && num_classes > 2){
    stop("t-test, Mann-Whitney-Wilcoxon Test and binomial logistic regression can only be run for 2-class problems.")
  }
  
  # Splits and shuffles
  
  if(use_k_fold == TRUE && !is.numeric(num_folds)){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }
  
  if(use_empirical_null == TRUE && !is.numeric(num_permutations)){
    stop("num_permutations should be a postive integer. A minimum of 50 shuffles is recommended.")
  }
  
  if(use_empirical_null == TRUE && num_permutations < 3){
    stop("num_permutations should be a positive integer >= 3 for empirical null calculations. A minimum of 50 shuffles is recommended.")
  }
  
  if(use_k_fold == TRUE && num_folds < 1){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }
  
  #------------- Preprocess data --------------
  
  # Widening for model matrix
  
  data_id <- data_id %>%
    dplyr::mutate(names = paste0(method, "_", names)) %>%
    dplyr::select(-c(method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")
  
  ncols <- ncol(data_id)
  
  # Delete features that are all NaNs and features with constant values
  
  data_id <- data_id %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::select(where(~dplyr::n_distinct(.) > 1))
  
  if(ncol(data_id) < ncols){
    message(paste0("Dropped ", ncols - ncol(data_id), " features due to containing all NAs or only a constant."))
  }
  
  # Check NAs
  
  nrows <- nrow(data_id)
  
  data_id <- data_id %>%
    tidyr::drop_na()
  
  if(nrow(data_id) < nrows){
    message(paste0("Dropped ", nrows - nrow(data_id), " unique IDs due to NA values."))
  }
  
  # Clean up column (feature) names so models fit properly (mainly an issue with SVM formula)
  
  data_id <- data_id %>%
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
    
  } else if(test_method == "binomial logistic") {
    
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
    
    output <- 3:ncol(data_id) %>%
      purrr::map(~ stats::t.test(formula = formula(paste0(colnames(data_id[.x]), " ~ group")), data = data_id))
    
    output <- data.table::rbindlist(output, use.names = TRUE) %>%
      dplyr::select(c(data.name, statistic, p.value)) %>%
      dplyr::distinct() %>%
      dplyr::rename(feature = data.name) %>%
      dplyr::mutate(feature = gsub(" .*", "\\1", feature),
                    classifier_name = classifier_name,
                    statistic_name = statistic_name) %>%
      dplyr::rename(statistic_value = 2,
                    p_value = 3)
    
    return(output)
    
  } else if (test_method == "wilcox"){
    
    output <- 3:ncol(data_id) %>%
      purrr::map(~ stats::wilcox.test(formula = formula(paste0(colnames(data_id[.x]), " ~ group")), data = data_id))
    
    output <- data.table::rbindlist(output, use.names = TRUE) %>%
      dplyr::select(c(data.name, statistic, p.value)) %>%
      dplyr::rename(feature = data.name) %>%
      dplyr::mutate(feature = gsub(" .*", "\\1", feature),
                    classifier_name = classifier_name,
                    statistic_name = statistic_name) %>%
      dplyr::rename(statistic_value = 2,
                    p_value = 3)
    
    return(output)
    
  } else if (test_method == "binomial logistic"){
    
    output <- 3:ncol(data_id) %>%
      purrr::map(~ stats::glm(formula = formula(paste0("group ~ ", colnames(data_id[.x]))), data = data_id, family = stats::binomial()))
    
    output <- 1:length(output) %>%
      purrr::map(~ gather_binomial_info(output, .x))
    
    output <- data.table::rbindlist(output, use.names = TRUE) %>%
      dplyr::mutate(classifier_name = classifier_name,
                    statistic_name = statistic_name)
    
    return(output)
    
  } else {
    
    # Set up progress bar for {purrr::map} iterations
    
    pb <- dplyr::progress_estimated(length(3:ncol(data_id)))
    
    # Very important coffee console message
    
    if(use_empirical_null & null_testing_method == "null model fits"){
      message("This will take a while. Great reason to go grab a coffee and relax ^_^")
    }
    
    # Compute accuracies for each feature
    
    fit_univariable_models_safe <- purrr::possibly(fit_univariable_models, otherwise = NULL)
    
    output <- 3:ncol(data_id) %>%
      purrr::map(~ fit_univariable_models_safe(data = data_id, 
                                               test_method = test_method,
                                               use_balanced_accuracy = use_balanced_accuracy,
                                               use_k_fold = use_k_fold,
                                               num_folds = num_folds,
                                               use_empirical_null = use_empirical_null,
                                               null_testing_method = null_testing_method,
                                               num_permutations = num_permutations,
                                               feature = .x,
                                               pb = pb))
    
    output <- output[!sapply(output, is.null)]
    output <- data.table::rbindlist(output, use.names = TRUE)
    
    # Run nulls if random shuffles are to be used
    
    if(null_testing_method == "model free shuffles"){
      
      # Run random shuffles procedure
      
      nullOuts <- simulate_null_acc(x = data_id$group, num_permutations = num_permutations, use_balanced_accuracy) %>%
        dplyr::mutate(category = "Null",
                      feature = "model free shuffles")
      
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
    
    if(return_raw_estimates){
      return(output)
    }
    
    # Compute statistics for each feature against empirical null distribution
    
    if(use_empirical_null){
      
      if(pool_empirical_null){
        
        # Set up vector of null accuracies across all features
        
        if(use_balanced_accuracy){
          
          nulls <- output %>%
            dplyr::filter(category == "Null") %>%
            dplyr::select(c(accuracy, balanced_accuracy))
          
          # Widen main results matrix
          
          main_matrix <- output %>%
            dplyr::select(c(category, feature, accuracy)) %>%
            dplyr::filter(category == "Main") %>%
            dplyr::group_by(feature) %>%
            dplyr::mutate(id = row_number()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
            dplyr::select(-c(id))
          
          main_matrix_balanced <- output %>%
            dplyr::select(c(category, feature, balanced_accuracy)) %>%
            dplyr::filter(category == "Main") %>%
            dplyr::group_by(feature) %>%
            dplyr::mutate(id = row_number()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "balanced_accuracy") %>%
            dplyr::select(-c(id))
          
          # Calculate p-values for each feature
          
          feature_statistics <- 2:ncol(main_matrix) %>%
            purrr::map(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = main_matrix_balanced,
                                                       x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
          
        } else{
          
          nulls <- output %>%
            dplyr::filter(category == "Null") %>%
            dplyr::select(c(accuracy))
          
          # Widen main results matrix
          
          main_matrix <- output %>%
            dplyr::select(c(category, feature, accuracy)) %>%
            dplyr::filter(category == "Main") %>%
            dplyr::group_by(feature) %>%
            dplyr::mutate(id = row_number()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
            dplyr::select(-c(id))
          
          # Calculate p-values for each feature
          
          feature_statistics <- 2:ncol(main_matrix) %>%
            purrr::map(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = NULL,
                                                       x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
        }
        
      } else{
        
        if(null_testing_method == "null model fits"){
          
          if(use_balanced_accuracy){
            
            main_matrix <- output %>%
              dplyr::select(c(category, feature, accuracy)) %>%
              dplyr::group_by(feature) %>%
              dplyr::mutate(id = row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(id))
            
            main_matrix_balanced <- output %>%
              dplyr::select(c(category, feature, balanced_accuracy)) %>%
              dplyr::group_by(feature) %>%
              dplyr::mutate(id = row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "balanced_accuracy") %>%
              dplyr::select(-c(id))
            
            # Calculate p-values for each feature
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map(~ calculate_unpooled_null(main_matrix = main_matrix, main_matrix_balanced = main_matrix_balanced, 
                                                   x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
            
          } else{
            
            main_matrix <- output %>%
              dplyr::select(c(category, feature, accuracy)) %>%
              dplyr::group_by(feature) %>%
              dplyr::mutate(id = row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(id))
            
            # Calculate p-values for each feature
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map(~ calculate_unpooled_null(main_matrix = main_matrix, main_matrix_balanced = NULL, 
                                                   x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
          }
          
        } else{
          
          # Set up vector of null accuracies across all features
          
          if(use_balanced_accuracy){
            
            nulls <- output %>%
              dplyr::filter(category == "Null") %>%
              dplyr::select(c(accuracy, balanced_accuracy))
            
            main_matrix <- output %>%
              dplyr::select(c(category, feature, accuracy)) %>%
              dplyr::filter(category == "Main") %>%
              dplyr::group_by(feature) %>%
              dplyr::mutate(id = row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(id))
            
            main_matrix_balanced <- output %>%
              dplyr::select(c(category, feature, balanced_accuracy)) %>%
              dplyr::filter(category == "Main") %>%
              dplyr::group_by(feature) %>%
              dplyr::mutate(id = row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "balanced_accuracy") %>%
              dplyr::select(-c(id))
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = main_matrix_balanced, 
                                                         x = .x, p_value_method = p_value_method, use_balanced_accuracy = use_balanced_accuracy))
            
          } else{
            
            nulls <- output %>%
              dplyr::filter(category == "Null") %>%
              dplyr::pull(accuracy)
            
            main_matrix <- output %>%
              dplyr::select(c(category, feature, accuracy)) %>%
              dplyr::filter(category == "Main") %>%
              dplyr::group_by(feature) %>%
              dplyr::mutate(id = row_number()) %>%
              dplyr::ungroup() %>%
              tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "accuracy") %>%
              dplyr::select(-c(id))
            
            feature_statistics <- 2:ncol(main_matrix) %>%
              purrr::map(~ calculate_against_null_vector(nulls = nulls, main_matrix = main_matrix, main_matrix_balanced = NULL, 
                                                         x = .x, p_value_method = p_value_method,
                                                         use_balanced_accuracy = use_balanced_accuracy))
          }
        }
      }
      
      # Bind together
      
      feature_statistics <- data.table::rbindlist(feature_statistics, use.names = TRUE) %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
    } else{
      
      feature_statistics <- output %>%
        dplyr::rename(statistic_value = statistic) %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)
    }
    return(feature_statistics)
  }
}
