#--------------- Helper functions ----------------

#-------------
# Model matrix
#-------------

prepare_univariate_model_matrices <- function(.data, use_k_fold, num_folds){
  
  if(use_k_fold){
    
    # Fix seed for reproducibility and set up fold indexes
    
    set.seed(123)
    yourdata <- .data[sample(nrow(.data)), ]
    folds <- cut(seq(1, nrow(yourdata)), breaks = num_folds, labels = FALSE)
    
    # Create datasets for 10 fold cross validation
    
    storage <- list()
    
    for(i in 1:num_folds){
      
      # Segement data by fold
      
      testIndexes <- which(folds == i, arr.ind = TRUE)
      testData <- yourdata[testIndexes, ]
      trainData <- yourdata[-testIndexes, ]
      
      # Scale data
      
      train_scales <- trainData %>%
        tidyr::pivot_longer(cols = 3:ncol(trainData), names_to = "names", values_to = "values") %>%
        dplyr::group_by(names) %>%
        dplyr::summarise(mean = mean(values, na.rm = TRUE),
                         sd = stats::sd(values, na.rm = TRUE)) %>%
        dplyr::ungroup()
      
      # Normalise train and test sets and widen model matrices
      
      trainData <- widener(trainData, train_scales)
      testData <- widener(testData, train_scales)
      
      ncols <- ncol(trainData)
      
      # Delete columns that were problematic for the train set so they can be removed from test set
      
      trainData <- trainData[ , colSums(is.na(trainData)) < nrow(trainData)]
      
      if(ncols < ncol(trainData)){
        print(paste0("Removed ", (ncol(trainData) - ncols), " features due to containing NA values after splitting and normalising."))
      }
      
      testData <- testData %>% dplyr::select(dplyr::all_of(colnames(trainData)))
      mylist <- list(trainData, testData)
      names(mylist) <- c("trainData", "testData")
      storage[[i]] <- mylist
    }
    
  } else{
    
    storage <- .data %>%
      tidyr::pivot_longer(cols = 3:ncol(.data), names_to = "names", values_to = "values") %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = (values - mean(values, na.rm = TRUE)) / sd(values, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(id)) %>%
      dplyr::mutate(group = as.factor(group))
    
    ncols <- ncol(storage)
    storage <- storage[ , colSums(is.na(storage)) < nrow(storage)]
    
    if(ncols < ncol(storage)){
      print(paste0("Removed ", (ncol(storage) - ncols), " features due to containing NA values after normalising."))
    }
  }
  return(storage)
}

#-------------------------------
# General purpose fit-by-feature 
# procedure to iterate over
#-------------------------------

fit_single_feature_model <- function(traindata, testdata, kernel, x){
  
  myformula <- formula(paste0("group ~ ", names(traindata[x])))
  mod <- e1071::svm(myformula, data = traindata, kernel = kernel, scale = FALSE, probability = TRUE)
  
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

fit_empirical_null_models <- function(maindata, testdata, kernel, x = NULL, s){
  
  # Print out updates for every 10 shuffles so the user gets a time guesstimate that isn't burdensome
  
  if (s %% 10 == 0) {
    print(paste0("Calculating models for ", s))
  }
  
  # Null shuffles and computations
  
  y <- maindata %>% dplyr::pull(group)
  y <- as.character(y)
  
  set.seed(s)
  shuffles <- sample(y, replace = FALSE)
  
  shuffledtrain <- maindata %>%
    dplyr::mutate(group = shuffles,
                  group = as.factor(group))
  
  fit_single_feature_model_safe <- purrr::possibly(fit_single_feature_model, otherwise = NA_character_)
  
  null_models <- 2:ncol(shuffledtrain) %>%
    purrr::map(~ fit_single_feature_model_safe(traindata = shuffledtrain, testdata = testdata, kernel = kernel, x = .x))

  # Remove any pure NA entries that mark errors
  
  null_models <- base::Filter(Negate(anyNA), null_models)
  
  null_models <- data.table::rbindlist(null_models, use.names = TRUE) %>%
    dplyr::mutate(shuffle = s)
  
  if(!is.null(x)){
    null_models <- null_models %>%
      dplyr::mutate(fold = x)
  } else{
    
  }
  
  return(null_models)
}

#----------------------
# Overall model fitting
#----------------------

fit_univariate_models <- function(data, test_method, use_k_fold, num_folds, use_empirical_null, num_shuffles){
  
  inputData <- prepare_univariate_model_matrices(.data = data, use_k_fold = use_k_fold, num_folds)
  
  # Main procedure
  
  if(test_method == "linear svm"){
    kernel <- "linear"
  } else{
    kernel <- "radial"
  }
  
  fit_single_feature_model_safe <- purrr::possibly(fit_single_feature_model, otherwise = NA_character_)
  fit_empirical_null_models_safe <- purrr::possibly(fit_empirical_null_models, otherwise = NA_character_)
  
  if(use_k_fold){
    
    combinations <- tidyr::crossing(1:num_folds, 2:ncol(inputData[[1]]$trainData))
    future::plan(future::multisession, workers = (future::availableCores() - 1))
    
    mainOuts <- furrr::future_pmap(list(combinations$`1:num_folds`, combinations$`2:ncol(inputData[[1]]$trainData)`), ~  
                              fit_single_feature_model_safe(traindata = inputData[[.x]]$trainData, testdata = inputData[[.x]]$testData, kernel = kernel, x = .y),
                            .options = furrr::furrr_options(seed = TRUE))
    
    future::plan(future::sequential)
    
    # Remove any pure NA entries that mark errors
    
    mainOuts <- base::Filter(Negate(anyNA), mainOuts)
    
    mainOuts <- data.table::rbindlist(mainOuts, use.names = TRUE) %>%
      dplyr::mutate(category = "Main")
    
    # Get outputs for empirical null
    
    if(use_empirical_null){
      
      combinationsNULL <- tidyr::crossing(1:num_folds, 1:num_shuffles)
      
      nullOuts <- purrr::pmap(list(combinationsNULL$`1:num_folds`, combinationsNULL$`1:num_shuffles`), ~  
                                fit_empirical_null_models_safe(maindata = inputData[[.x]]$trainData, 
                                                               testdata = inputData[[.x]]$testData, 
                                                               kernel = kernel, x = .x, s = .y))
      
      # Remove any pure NA entries that mark errors
      
      nullOuts <- base::Filter(Negate(anyNA), nullOuts)
      
      message("Averaging empirical null classification accuracies for each shuffle over folds.")
      
      nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
        dplyr::group_by(feature, shuffle) %>%
        dplyr::summarise(statistic = mean(statistic, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(category = "Null") %>%
        dplyr::select(-c(shuffle))
      
      # Bind together and return
      
      finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
      
    } else{
      finalOuts <- mainOuts
    }
    
  } else{
    
    future::plan(future::multisession, workers = (future::availableCores() - 1))
    
    mainOuts <- 2:ncol(inputData) %>%
      furrr::future_map(~ fit_single_feature_model_safe(traindata = inputData, testdata = inputData, kernel = kernel, x = .x),
                        .options = furrr::furrr_options(seed = TRUE))
    
    future::plan(future::sequential)
    
    # Remove any pure NA entries that mark errors
    
    mainOuts <- base::Filter(Negate(anyNA), mainOuts)
    
    mainOuts <- data.table::rbindlist(mainOuts, use.names = TRUE) %>%
      dplyr::mutate(category = "Main")
    
    # Get outputs for empirical null
    
    if(use_empirical_null){
      nullOuts <- 1:num_shuffles %>%
        purrr::map(~ fit_empirical_null_models_safe(maindata = inputData, 
                                                    testdata = inputData, 
                                                    kernel = kernel,
                                                    x = NULL,
                                                    s = .x))
      
      # Remove any pure NA entries that mark errors
      
      nullOuts <- base::Filter(Negate(anyNA), nullOuts)
      
      nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
        dplyr::mutate(category = "Null") %>%
        dplyr::select(-c(shuffle))
      
      # Bind together and return
      
      finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
    } else{
      finalOuts <- mainOuts
    }
  }
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
    dplyr::summarise(statistic = mean(statistic, na.rm = TRUE)) %>%
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
    dplyr::summarise(statistic = mean(statistic, na.rm = TRUE)) %>%
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

#-------------- Main exported function ---------------

#' Fit a classifier to feature matrix to extract top performers
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr drop_na pivot_wider crossing
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats glm binomial sd wilcox.test t.test
#' @importFrom purrr possibly map pmap
#' @importFrom future plan availableCores sequential multisession
#' @importFrom furrr future_map future_pmap
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param test_method the algorithm to use for quantifying class separation
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values if linear svm or rbf svm is selected. Defaults to FALSE
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to FALSE
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if linear svm or rbf svm is selected and use_k_fold is set to TRUE. Defaults to 0 but if TRUE, 10 is recommended
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
#'   num_shuffles = 5,
#'   pool_empirical_null = FALSE) 
#' }
#' 

fit_feature_classifier <- function(data, id_var = "id", group_var = "group",
                                   test_method = c("t-test", "wilcox", "binomial logistic", "linear svm", "rbf svm"),
                                   use_empirical_null = FALSE, use_k_fold = FALSE,
                                   num_folds = 0, num_shuffles = 50,
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
  
  if(test_method %in% c("linear svm", "rbf svm") && (!is.numeric(num_folds) || !is.numeric(num_shuffles))){
    stop("num_folds and num_shuffles should both be integers.")
  }
  
  if(test_method %in% c("linear svm", "rbf svm") && use_empirical_null == TRUE && num_shuffles < 3){
    stop("num_shuffles should be an integer >= 3 for empirical null calculations. A minimum of 50 shuffles is recommended.")
  }
  
  if(test_method %in% c("linear svm", "rbf svm") && use_k_fold == TRUE && num_folds < 2){
    stop("num_folds should be an integer >= 2. 10 folds is recommended.")
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
    statistic_name <- "Classification accuracy"
    
  } else if(test_method == "rbf svm") {
    
    classifier_name <- "RBF SVM"
    statistic_name <- "Classification accuracy"
    
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
      
      if(test_method == "t-test") {
        
        mod <- stats::t.test(as.formula(f), data = data_id)
        c(mod$statistic, p.value = mod$p.value)
        
      } else if(test_method == "wilcox") {
        
        mod <- stats::wilcox.test(as.formula(f), data = data_id)
        c(mod$statistic, p.value = mod$p.value)
        
      } else {
        
        mod <- stats::glm(as.formula(f), data = data_id, family = stats::binomial())
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
    
    return(output)
    
  } else if (test_method == "linear svm" || test_method == "rbf svm"){
    
    # Compute accuracies for each permutation and feature
    
    output <- fit_univariate_models(data = data_id, 
                                    test_method = test_method, 
                                    use_k_fold = use_k_fold,
                                    num_folds = num_folds,
                                    use_empirical_null = use_empirical_null,
                                    num_shuffles = num_shuffles)
    
    # Compute statistics for each feature against empirical null distribution
    
    if(use_empirical_null){
      
      if(pool_empirical_null){
        
        calculate_pooled_null_safe <- purrr::possibly(calculate_pooled_null, otherwise = NA_character_)
        
        # Set up vector of null accuracies across all features
        
        null_vector <- output %>%
          dplyr::filter(category == "Null") %>%
          dplyr::select(-c(feature)) %>%
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
        
        future::plan(future::multisession, workers = (future::availableCores() - 1))
        
        feature_statistics <- 2:ncol(main_matrix) %>%
          furrr::future_map(~ calculate_pooled_null_safe(null_vector = null_vector, main_matrix = main_matrix, x = .x),
                            .options = furrr::furrr_options(seed = TRUE))
        
        future::plan(future::sequential)
        
      } else{
        
        calculate_unpooled_null_safe <- purrr::possibly(calculate_unpooled_null, otherwise = NA_character_)
        
        # Widen data matrix
        
        output <- output %>%
          dplyr::group_by(feature) %>%
          dplyr::mutate(id = row_number()) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_wider(id_cols = c("id", "category"), names_from = "feature", values_from = "statistic") %>%
          dplyr::select(-c(id))
        
        # Calculate p-values for each feature
        
        future::plan(future::multisession, workers = (future::availableCores() - 1))
        
        feature_statistics <- 2:ncol(output) %>%
          furrr::future_map(~ calculate_unpooled_null_safe(.data = output, x = .x),
                            .options = furrr::furrr_options(seed = TRUE))
        
        future::plan(future::sequential)
        
      }
      
      # Remove any pure NA entries that mark errors
      
      feature_statistics <- base::Filter(Negate(anyNA), feature_statistics)
      
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
