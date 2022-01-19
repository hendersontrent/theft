#--------------- Helper functions ----------------

#-------------
# Model matrix
#-------------

prepare_univariate_model_matrices <- function(.data, seed){
  
  # Train-test split
  
  set.seed(seed)
  bound <- floor((nrow(.data)/4)*3)
  .data <- .data[sample(nrow(.data)), ]
  train <- .data[1:bound, ]
  test <- .data[(bound + 1):nrow(.data), ]
  
  # Get train mean and SD for normalisation
  
  train_scales <- train %>%
    tidyr::pivot_longer(cols = 3:ncol(train), names_to = "names", values_to = "values") %>%
    dplyr::group_by(names) %>%
    dplyr::summarise(mean = mean(values, na.rm = TRUE),
                     sd = stats::sd(values, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # Normalise train and test sets and widen model matrices
  
  train <- widener(train, train_scales)
  test <- widener(test, train_scales)
  
  # Delete columns that were problematic for the train set so they can be removed from test set
  
  removals <- sapply(train, function(x) sum(is.na(x)))
  removals <- removals[removals > 0]
  train <- train %>% dplyr::select(!dplyr::all_of(removals))
  test <- test %>% dplyr::select(!dplyr::all_of(removals))
  myMatrix <- list(train, test)
  return(myMatrix)
}

#-------------------------------
# General purpose fit-by-feature 
# procedure to iterate over
#-------------------------------

fit_single_feature_model <- function(traindata, testdata, kernel, x){
  
  myformula <- formula(paste0("group ~ ", names(traindata[x])))
  mod <- e1071::svm(myformula, data = traindata, kernel = kernel, cross = 10, probability = TRUE)
  
  # Get outputs for main model
  
  cm <- as.data.frame(table(testdata$group, predict(mod, newdata = testdata))) %>%
    dplyr::mutate(flag = ifelse(Var1 == Var2, "Same", "Different"))
  
  same_total <- cm %>%
    dplyr::filter(flag == "Same") %>%
    dplyr::summarise(Freq = sum(Freq)) %>%
    dplyr::pull()
  
  all_total <- cm %>%
    dplyr::summarise(Freq = sum(Freq)) %>%
    dplyr::pull()
  
  statistic <- same_total / all_total
  
  tmp_feature <- data.frame(feature = as.character(names(traindata[x])),
                            statistic = statistic)
  
  return(tmp_feature)
}

#-------------------
# Null model fitting
#-------------------

fit_empirical_null_models <- function(traindata, testdata, test_method, s){
  
  y <- traindata %>% dplyr::pull(group)
  y <- as.character(y)
  
  set.seed(s)
  shuffles <- sample(y, replace = FALSE)
  
  shuffledtrain <- traindata %>%
    dplyr::mutate(group = shuffles,
                  group = as.factor(group))
  
  # Main procedure
  
  if(test_method == "linear svm"){
    kernel <- "linear"
  } else{
    kernel <- "radial"
  }
  
  null_models <- 2:ncol(shuffledtrain) %>%
    purrr::map(~ fit_single_feature_model(traindata = shuffledtrain, testdata = testdata, kernel = kernel, x = .x))
  
  null_models <- data.table::rbindlist(null_models, use.names = TRUE)
  return(null_models)
}

#----------------------
# Overall model fitting
#----------------------

fit_univariate_models <- function(data, test_method, num_shuffles, seed){
  
  inputData <- prepare_univariate_model_matrices(data, seed = seed)
  trainset <- as.data.frame(inputData[1])
  testset <- as.data.frame(inputData[2])
  removals <- sapply(trainset, function(y) sum(length(which(is.na(y)))))
  removals <- data.frame(removals) %>% tibble::rownames_to_column(var = "names") %>% dplyr::filter(removals >= 1) %>% dplyr::pull(names)
  mytrainset <- trainset %>% dplyr::select(-dplyr::all_of(removals))
  mytestset <- testset %>% dplyr::select(-dplyr::all_of(removals))
  
  # Main procedure
  
  if(test_method == "linear svm"){
    kernel <- "linear"
  } else{
    kernel <- "radial"
  }
  
  mainOuts <- 2:ncol(mytrainset) %>%
    purrr::map(~ fit_single_feature_model(traindata = mytrainset, testdata = mytestset, kernel = kernel, x = .x))
  
  mainOuts <- data.table::rbindlist(mainOuts, use.names = TRUE) %>%
    dplyr::mutate(category = "Main")
  
  # Get outputs for empirical null
  
  nullOuts <- 1:num_shuffles %>%
    purrr::map(~ fit_empirical_null_models(traindata = mytrainset, 
                                           testdata = mytestset, 
                                           test_method = test_method, 
                                           s = .x))
  
  nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
    dplyr::mutate(category = "Null")
  
  # Bind together and return
  
  finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
  return(finalOuts)
}

#-------------- Main exported function ---------------

#' Fit a classifier to feature matrix to extract top performers
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom purrr map
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats glm binomial sd wilcox.test t.test
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param test_method the algorithm to use for quantifying class separation
#' @param num_splits an integer specifying the number of 75/25 train-test splits to perform if linear svm or rbf svm is selected. Defaults to 10
#' @param num_shuffles an integer specifying the number of class label shuffles to perform if linear svm or rbf svm is selected. Defaults to 5
#' @param pool_empirical_null a Boolean specifying whether to use the pooled empirical null distribution of all features or each features' individual empirical null distribution if linear svm or rbf svm is selected. Defaults to FALSE
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
#'   num_splits = 10,
#'   num_shuffles = 5,
#'   pool_empirical_null = FALSE) 
#' }
#' 

fit_feature_classifier2 <- function(data, id_var = "id", group_var = "group",
                                    test_method = c("t-test", "wilcox", "binomial logistic", "linear svm", "rbf svm"),
                                    num_splits = 10, num_shuffles = 5,
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
                    group = dplyr::all_of(group_var))
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
    test_method <- "linear svm"
    message("test_method is NULL or missing. Running linear svm for multiclass problem.")
  }
  
  if(test_method %in% c("t-test", "wilcox", "binomial logistic") && num_classes > 2){
    stop("t-test, Mann-Whitney-Wilcoxon Test and binomial logistic regression can only be run for 2-class problems.")
  }
  
  # Splits and shuffles
  
  if(test_method %in% c("linear svm", "rbf svm") && (!is.numeric(num_splits) || !is.numeric(num_shuffles))){
    stop("num_splits and num_shuffles should both be integers >= 1.")
  }
  
  if(test_method %in% c("linear svm", "rbf svm") && (num_splits < 1 || num_shuffles < 1)){
    stop("num_splits and num_shuffles should both be integers >= 1.")
  }
  
  if(test_method %in% c("linear svm", "rbf svm") && (num_splits == 1 && pool_empirical_null == FALSE && num_shuffles < 3)){
    stop("If pool_empirical_null = FALSE and num_splits == 1, num_shuffles should be an integer >= 3 so each feature has a distribution to compute statistics on.")
  }
  
  if(test_method %in% c("linear svm", "rbf svm") && (num_shuffles == 1 && pool_empirical_null == FALSE && num_splits < 3)){
    stop("If pool_empirical_null = FALSE and num_shuffles == 1, num_splits should be an integer >= 3 so each feature has a distribution to compute statistics on.")
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
  
  #------------- Fit classifiers -------------
  
  #--------------------------
  # Set up column information 
  # for iteration
  #--------------------------
  
  features <- seq(from = 3, to = ncol(data_id))
  feature_names <- colnames(data_id)
  feature_names <- feature_names[!feature_names %in% c("id", "group")] # Remove ID and group columns
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
    
  } else if(test_method == "linear svm") {
    
    classifier_name <- "Linear SVM"
    statistic_name <- "Mann-Whitney-Wilcoxon Test statistic of classification accuracies"
    
  } else if(test_method == "rbf svm") {
    
    classifier_name <- "RBF SVM"
    statistic_name <- "Mann-Whitney-Wilcoxon Test statistic of classification accuracies"
    
  } else if(test_method == "binomial logistic") {
    
    classifier_name <- "Binomial logistic regression"
    statistic_name <- "Binomial logistic coefficient z-test"
  }
  
  #-----------------------------
  # Iterate through each feature 
  # and fit appropriate model
  #-----------------------------
  
  if(test_method %in% c("t-test", "wilcox", "binomial logistic")){
    
    # Define a vector of formulas to iterate over
    
    if(test_method %in% c("t-test", "wilcox")){
      formulas <- paste(names(data_id)[3:(ncol(data_id))], "~ group")
    } else{
      formulas <- paste("group ~ ", names(data_id)[3:(ncol(data_id))])
    }
    
    output <- as.data.frame(t(sapply(formulas, function(f) {
      
      # Run selected method for each formula
      
      if(test_method == "t-test"){
        mod <- stats::t.test(as.formula(f), data = data_id)
      } else if(test_method == "wilcox") {
        mod <- stats::wilcox.test(as.formula(f), data = data_id)
      } else{
        mod <- stats::glm(as.formula(f), data = data_id, family = stats::binomial())
      }
      
      # Extract statistics
      
      if(test_method %in% c("t-test", "wilcox")){
        c(mod$statistic, p.value = mod$p.value)
        
      } else{
        
        statistic_value <- as.numeric(summary(mod)$coefficients[,3][2])
        p_value <- as.numeric(summary(mod)$coefficients[,4][2])
        c(statistic_value, p.value = p_value)
      }
    }
    )
    )
    ) %>%
      tibble::rownames_to_column(var = "feature") %>%
      dplyr::mutate(feature = gsub(" .*", "\\1", feature),
                    classifier_name = classifier_name,
                    statistic_name = statistic_name) %>%
      dplyr::rename(statistic_value = 2,
                    p_value = 3)
    
  } else if (test_method == "linear svm" || test_method == "rbf svm"){
    
    message("Fitting SVM with 10-fold cross-validation (CV) for every split and shuffle permutation specified. This may take a while.")
    
    output <- 1:num_splits %>%
      purrr::map(~ fit_univariate_models(data = data_id, 
                                         test_method = test_method, 
                                         num_shuffles = num_shuffles, 
                                         seed = .x))
    
    output <- data.table::rbindlist(output, use.names = TRUE)
  }
  return(output)
}
