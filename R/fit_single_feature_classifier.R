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
#' @param return_raw_estimates a Boolean (for testing purposes only -- will break \code{compute_top_features}!!) specifying whether to return the raw main and null model results
#' @return an object of class dataframe containing results
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
#' # Mimic machinery of theft::compute_top_features
#' # which calls fit_single_feature_classifier and
#' # does these operations prior
#'   
#' featMat$group <- make.names(featMat$group)
#' featMat$group <- as.factor(featMat$group)
#' featMat$values <- as.numeric(featMat$values)
#'   
#' fit_single_feature_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   test_method = "gaussprRadial",
#'   use_balanced_accuracy = FALSE,
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   use_empirical_null = TRUE,
#'   null_testing_method = "ModelFreeShuffles",
#'   p_value_method = "gaussian",
#'   num_permutations = 50,
#'   pool_empirical_null = FALSE,
#'   seed = 123,
#'   return_raw_estimates = FALSE) 
#' }
#' 

fit_single_feature_classifier <- function(data, id_var = "id", group_var = "group",
                                          test_method = "gaussprRadial", use_balanced_accuracy = FALSE,
                                          use_k_fold = FALSE, num_folds = 10, 
                                          use_empirical_null = FALSE, null_testing_method = c("ModelFreeShuffles", "NullModelFits"),
                                          p_value_method = c("empirical", "gaussian"), num_permutations = 50,
                                          pool_empirical_null = FALSE, seed = 123, return_raw_estimates = FALSE){
  
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
  
  # Upstream correction for deprecated 'binomial logistic' specification
  
  if(length(test_method) == 1 && test_method == "binomial logistic"){
    test_method <- "BinomialLogistic"
    message("'binomial logistic' is deprecated. Please specify 'BinomialLogistic' instead. Performing this conversion automatically.")
  }
  
  # Null testing options
  
  if(length(null_testing_method) != 1 && test_method %ni% c("t-test", "wilcox", "BinomialLogistic")){
    stop("null_testing_method should be a single string of either 'ModelFreeShuffles' or 'NullModelFits'.")
  }
  
  if((is.null(null_testing_method) || missing(null_testing_method)) && test_method %ni% c("t-test", "wilcox", "BinomialLogistic")){
    null_testing_method <- "ModelFreeShuffles"
    message("No argument supplied to null_testing_method. Using 'ModelFreeShuffles' as default.")
  }
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method == "model free shuffles"){
    message("'model free shuffles' is deprecated, please use 'ModelFreeShuffles' instead.")
    null_testing_method <- "ModelFreeShuffles"
  }
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method == "null model fits"){
    message("'null model fits' is deprecated, please use 'NullModelFits' instead.")
    null_testing_method <- "NullModelFits"
  }
  
  theoptions <- c("ModelFreeShuffles", "NullModelFits")
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method %ni% theoptions){
    stop("null_testing_method should be a single string of either 'ModelFreeShuffles' or 'NullModelFits'.")
  }
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method == "ModelFreeShuffles" && num_permutations < 1000){
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
  
  if(num_classes == 1){
    stop("Your data only has one class label. At least two are required to performed analysis.")
  }
  
  if(num_classes == 2 && test_method %ni% c("t-test", "wilcox", "BinomialLogistic")){
    message("Your data has two classes. Setting test_method to one of 't-test', 'wilcox', or 'BinomialLogistic' is recommended.")
  }
  
  if(((missing(test_method) || is.null(test_method))) && num_classes == 2){
    test_method <- "t-test"
    message("test_method is NULL or missing. Running t-test as default for 2-class problem.")
  }
  
  if(((missing(test_method) || is.null(test_method))) && num_classes > 2){
    test_method <- "gaussprRadial"
    message("test_method is NULL or missing, fitting 'gaussprRadial' by default.")
  }
  
  if(test_method %in% c("t-test", "wilcox", "BinomialLogistic") && num_classes > 2){
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
    
    if(return_raw_estimates){
      return(output)
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
