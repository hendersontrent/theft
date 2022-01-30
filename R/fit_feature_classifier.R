#--------------- Helper functions ----------------

fit_empirical_null_univariate_models <- function(mod, testdata, s){
  
  # Print out updates for every 10 shuffles so the user gets a time guesstimate that isn't burdensome
  
  if (s %% 10 == 0) {
    print(paste0("Calculating shuffle ", s))
  }
  
  # Null shuffles and computations
  
  y <- testdata %>% dplyr::pull(group)
  y <- as.character(y)
  
  set.seed(s)
  shuffles <- sample(y, replace = FALSE)
  
  shuffledtest <- testdata %>%
    dplyr::mutate(group = shuffles,
                  group = as.factor(group))
  
  null_models <- extract_prediction_accuracy(mod = mod, testData = shuffledtest)
  return(null_models)
}

#--------------
# Model fitting
#--------------

fit_univariate_models <- function(data, test_method, use_k_fold, num_folds, use_empirical_null, num_shuffles, split_prop, feature){
  
  tmp <- data %>%
      dplyr::select(c(group, dplyr::all_of(feature)))
  
  # Make a train-test split
  
  set.seed(123)
  
  trainIndex <- caret::createDataPartition(tmp$group, 
                                           p = split_prop, 
                                           list = FALSE, 
                                           times = 1)
  
  dataTrain <- tmp[ trainIndex,]
  dataTest  <- tmp[-trainIndex,]
  
  if(use_k_fold){
    
    # Train model
    
    fitControl <- caret::trainControl(method = "cv",
                                      number = num_folds,
                                      classProbs = TRUE)
    
    mod <- caret::train(group ~ ., 
                        data = dataTrain, 
                        method = test_method, 
                        trControl = fitControl,
                        preProcess = c("center", "scale"))
    
  } else{
    
    mod <- caret::train(group ~ ., 
                        data = dataTrain, 
                        method = test_method, 
                        preProcess = c("center", "scale"))
  }
  
  # Get main predictions
  
  mainOuts <- extract_prediction_accuracy(mod = mod, testData = dataTest) %>%
    dplyr::mutate(category = "Main")
  
  if(use_empirical_null){
    
    nullOuts <- 1:num_shuffles %>%
      purrr::map( ~ fit_empirical_null_univariate_models(mod = mod, 
                                                         testdata = dataTest, 
                                                         s = .x))
    
    nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
      dplyr::mutate(category = "Null")
    
    finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
    
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

# Pooled

calculate_pooled_null <- function(null_vector, main_matrix, x){
  
  # Filter data matrix to feature of interest
  
  statistic_value <- main_matrix %>%
    dplyr::select(dplyr::all_of(x)) %>%
    dplyr::rename(statistic = 1) %>%
    dplyr::pull(statistic)
  
  # Compute p-value against empirical null samples
  
  nulls_above_main <- null_vector[null_vector >= statistic_value]
  p_value <- length(nulls_above_main) / length(null_vector)
  
  tmp_outputs <- data.frame(feature = names(main_matrix)[x],
                            statistic_value = statistic_value,
                            p_value = p_value)
  
  return(tmp_outputs)
}

# Unpooled

calculate_unpooled_null <- function(.data, x){
  
  # Filter data matrix to feature of interest
  
  tmp <- .data %>%
    dplyr::select(category, dplyr::all_of(x)) %>%
    dplyr::rename(statistic = 2)
  
  # Compute mean for main model
  
  statistic_value <- tmp %>%
    dplyr::filter(category == "Main") %>%
    dplyr::pull(statistic)
  
  # Compute p-value against empirical null samples
  
  nulls <- tmp %>%
    dplyr::filter(category == "Null")
  
  nulls_above_main <- nulls %>%
    dplyr::filter(statistic >= statistic_value)
  
  p_value <- nrow(nulls_above_main) / nrow(nulls)
  
  tmp_outputs <- data.frame(feature = names(.data)[x],
                            statistic_value = statistic_value,
                            p_value = p_value)
  
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
#' @importFrom tidyr drop_na pivot_wider crossing
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats glm binomial sd wilcox.test t.test
#' @importFrom purrr map
#' @importFrom janitor clean_names
#' @importFrom caret createDataPartition preProcess train
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param test_method the algorithm to use for quantifying class separation
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values if linear svm or rbf svm is selected. Defaults to FALSE
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to FALSE
#' @param num_folds an integer specifying the number of k-folds to perform if data has more than two classes. Defaults to 10
#' @param split_prop a double between 0 and 1 specifying the proportion of input data that should go into the training set (therefore 1 - p goes into the test set). Defaults to 0.8
#' @param num_shuffles an integer specifying the number of class label shuffles to perform if linear svm or rbf svm is selected. Defaults to 50
#' @param pool_empirical_null a Boolean specifying whether to use the pooled empirical null distribution of all features or each features' individual empirical null distribution if linear svm or rbf svm is selected and use_empirical_null is TRUE. Defaults to FALSE
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
#' fit_feature_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   test_method = "linear svm",
#'   use_empirical_null = TRUE,
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   split_prop = 0.8,
#'   num_shuffles = 5,
#'   pool_empirical_null = FALSE) 
#' }
#' 

fit_feature_classifier <- function(data, id_var = "id", group_var = "group",
                                   test_method = c("t-test", "wilcox", "binomial logistic", "linear svm", "rbf svm"),
                                   use_empirical_null = FALSE, use_k_fold = FALSE,
                                   num_folds = 10, split_prop = 0.8, num_shuffles = 50,
                                   pool_empirical_null = FALSE){
  
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
  
  # Set defaults for classification method
  
  methods <- c("t-test", "wilcox", "binomial logistic", "linear svm", "rbf svm")
  
  if(test_method %ni% methods){
    stop("test_method should be a single string specification of 't-test', 'wilcox', 'binomial logistic', 'linear svm', or 'rbf svm'.")
  }
  
  if(length(test_method) != 1){
    stop("test_method should be a single string specification of 't-test', 'wilcox', 'binomial logistic', 'linear svm', or 'rbf svm'.")
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
  
  if(num_classes == 2){
    message("Your data has two classes. Setting test_method to one of 't-test', 'wilcox', or 'binomial logistic' is recommended.")
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
  
  if(use_empirical_null == TRUE && !is.numeric(num_shuffles)){
    stop("num_shuffles should be a postive integer. A minimum of 50 shuffles is recommended.")
  }
  
  if(use_empirical_null == TRUE && num_shuffles < 3){
    stop("num_shuffles should be a positive integer >= 3 for empirical null calculations. A minimum of 50 shuffles is recommended.")
  }
  
  if(use_k_fold == TRUE && num_folds < 1){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }
  
  if(!is.numeric(split_prop)){
    stop("split_prop should be a scalar between 0 and 1 specifying the proportion of input data that should go into the training set.")
  }
  
  if(split_prop < 0 || split_prop > 1){
    stop("split_prop should be a scalar between 0 and 1 specifying the proportion of input data that should go into the training set.")
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
    message(paste0("Dropped ", ncols - ncol(data_id), " features due to containing NAs or only a constant."))
  }
  
  # Check NAs
  
  nrows <- nrow(data_id)
  
  data_id <- data_id %>%
    dplyr::filter(!is.na(group)) %>%
    dplyr::mutate(group = as.factor(group))
  
  if(nrow(data_id) < nrows){
    message(paste0("Dropped ", nrows - nrow(data_id), " time series due to NaN values in the 'group' variable."))
  }
  
  # Clean up column (feature) names so models fit properly (mainly an issue with SVM formula)
  
  data_id <- data_id %>%
    janitor::clean_names()
  
  #------------- Fit classifiers -------------
  
  #--------------------------
  # Set up column information 
  # for iteration
  #--------------------------
  
  message("Performing calculations... This may take a while depending on the number of features and classes in your dataset.")
  
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
    statistic_name <- "Classification accuracy"
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
    
    # Compute accuracies for each permutation and feature
    
    output <- 3:ncol(data_id) %>%
      purrr::map(~ fit_univariate_models(data = data_id, 
                                         test_method = test_method, 
                                         use_k_fold = use_k_fold,
                                         num_folds = num_folds,
                                         use_empirical_null = use_empirical_null,
                                         split_prop = split_prop,
                                         num_shuffles = num_shuffles,
                                         feature = .x))
    
    output <- data.table::rbindlist(output, use.names = TRUE)
    
    # Compute statistics for each feature against empirical null distribution
    
    if(use_empirical_null){
      
      if(pool_empirical_null){
        
        # Set up vector of null accuracies across all features
        
        null_vector <- output %>%
          dplyr::filter(category == "Null") %>%
          dplyr::pull(statistic)
        
        # Widen main results matrix
        
        main_matrix <- output %>%
          dplyr::filter(category == "Main") %>%
          dplyr::group_by(feature) %>%
          dplyr::mutate(id = row_number()) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "statistic") %>%
          dplyr::select(-c(id))
        
        # Calculate p-values for each feature
        
        feature_statistics <- 2:ncol(main_matrix) %>%
          purrr::map(~ calculate_pooled_null(null_vector = null_vector, main_matrix = main_matrix, x = .x))
        
      } else{
        
        # Widen data matrix
        
        main_matrix <- output %>%
          dplyr::group_by(feature) %>%
          dplyr::mutate(id = row_number()) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "statistic") %>%
          dplyr::select(-c(id))
        
        # Calculate p-values for each feature
        
        feature_statistics <- 2:ncol(main_matrix) %>%
          purrr::map(~ calculate_unpooled_null(.data = main_matrix, x = .x))
        
      }
      
      # Bind together
      
      feature_statistics <- data.table::rbindlist(feature_statistics, use.names = TRUE) %>%
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
