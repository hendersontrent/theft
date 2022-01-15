#--------------- Helper functions ----------------

#-------------
# Data widener
#-------------

scale_univariate_feature <- function(data, train_mean, train_sd){
  
  tmpWide <- data %>%
    dplyr::mutate(values = (values - train_mean) / train_sd) %>%
    dplyr::mutate(group = as.factor(group))
  
  return(tmpWide)
}

#-------------
# Model matrix
#-------------

prepare_univariate_model_matrices <- function(data, seed){
  
  mydata2 <- data
  set.seed(seed)
  bound <- floor((nrow(mydata2)/4)*3)
  mydata2 <- mydata2[sample(nrow(mydata2)), ]
  train <- mydata2[1:bound, ]
  test <- mydata2[(bound + 1):nrow(mydata2), ]
  train_mean <- mean(train$values, na.rm = TRUE)
  train_sd <- stats::sd(train$values, na.rm = TRUE)
  train <- scale_univariate_feature(train, train_mean, train_sd)
  test <- scale_univariate_feature(test, train_mean, train_sd)
  myMatrix <- list(train, test)
  return(myMatrix)
}

#--------------
# Model fitting
#--------------

fit_univariate_models <- function(traindata, testdata, test_method){
  
  # Main procedure
  
  if(test_method == "linear svm"){
    mod <- e1071::svm(group ~., data = traindata, kernel = "linear", cross = 10, probability = TRUE)
  } else{
    mod <- e1071::svm(group ~., data = traindata, kernel = "radial", cross = 10, probability = TRUE)
  }
  
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
  statistic_name <- "Classification accuracy"
  
  # Empirical null
  
  y <- traindata %>% dplyr::pull(group)
  y <- as.character(y)
  shuffles <- sample(y, replace = FALSE)
  
  shuffledtrain <- traindata %>%
    dplyr::mutate(group = shuffles,
                  group = as.factor(group))
  
  # Fit classifier
  
  if(test_method == "linear svm"){
    modNULL <- e1071::svm(group ~., data = shuffledtrain, kernel = "linear", cross = 10, probability = TRUE)
  } else{
    modNULL <- e1071::svm(group ~., data = shuffledtrain, kernel = "radial", cross = 10, probability = TRUE)
  }
  
  # Get outputs for model
  
  cmNULL <- as.data.frame(table(testdata$group, predict(modNULL, newdata = testdata))) %>%
    dplyr::mutate(flag = ifelse(Var1 == Var2, "Same", "Different"))
  
  same_totalNULL <- cmNULL %>%
    dplyr::filter(flag == "Same") %>%
    dplyr::summarise(Freq = sum(Freq)) %>%
    dplyr::pull()
  
  all_totalNULL <- cmNULL %>%
    dplyr::summarise(Freq = sum(Freq)) %>%
    dplyr::pull()
  
  statisticNULL <- same_totalNULL / all_totalNULL
  
  outputs <- data.frame(category = c("Main", "Null"),
                        statistic = c(statistic, statisticNULL))
  
  return(outputs)
}

#-------------- Main exported function ---------------

#' Fit a classifier to feature matrix to extract top performers
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats glm binomial sd wilcox.test
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param test_method the algorithm to use for quantifying class separation
#' @param num_splits an integer specifying the number of 75/25 train-test splits to perform if linear svm or rbf svm is selected. Defaults to 10
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
#'   pool_empirical_null = FALSE) 
#' }
#' 

fit_feature_classifier <- function(data, id_var = "id", group_var = "group",
                                   test_method = c("t-test", "wilcox", "binomial logistic", "linear svm", "rbf svm"),
                                   num_splits = 10, pool_empirical_null = FALSE){
  
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
  
  # Splits
  
  if(test_method %in% c("linear svm", "rbf svm") && !is.numeric(num_splits)){
    stop("num_splits should be an integer >=2 specifying the number of train-test splits to perform.")
  }
  
  if(test_method %in% c("linear svm", "rbf svm") && num_splits < 2){
    stop("num_splits should be an integer >=2 specifying the number of train-test splits to perform.")
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
    dplyr::filter(!is.na(group))
  
  if(nrow(data_id) < nrows){
    message(paste0("Dropped ", nrows - nrow(data_id), " time series due to NaN values in the 'group' variable."))
  }
  
  #------------- Fit classifiers -------------
  
  # Loop over features and fit appropriate model
  
  features <- seq(from = 3, to = ncol(data_id))
  feature_names <- colnames(data_id)
  feature_names <- feature_names[!feature_names %in% c("id", "group")] # Remove ID and group columns
  results <- list()
  message("Performing calculations... This may take a while depending on the number of features and classes in your dataset.")
  
  for(f in features){
    
    if(test_method == "t-test"){
      
      # Filter dataset
      
      tmp <- data_id %>%
        dplyr::select(c(group, dplyr::all_of(f)))
      
      # Perform calculations between the two groups
      
      class_names <- (unique(tmp$group))
      x <- tmp %>% dplyr::filter(group == class_names[1]) %>% dplyr::pull(2)
      y <- tmp %>% dplyr::filter(group == class_names[2]) %>% dplyr::pull(2)
      mod <- t.test(x, y)
      
      # Extract statistics
      
      classifier_name <- "Welch Two Sample t-test"
      statistic_name <- "t-test statistic"
      statistic_value <- mod$statistic
      p_value <- mod$p.value
      
    } else if(test_method == "wilcox"){
      
      # Filter dataset
      
      tmp <- data_id %>%
        dplyr::select(c(group, dplyr::all_of(f))) %>%
        dplyr::rename(values = 2)
      
      # Perform calculations between the two groups
      
      mod <- stats::wilcox.test(values ~ group, data = tmp)
      
      # Extract statistics
      
      classifier_name <- "Mann-Whitney-Wilcoxon Test"
      statistic_name <- "Mann-Whitney-Wilcoxon Test statistic"
      statistic_value <- mod$statistic
      p_value <- mod$p.value
    
    } else if (test_method == "linear svm" || test_method == "rbf svm"){
      
      #---------------
      # Main procedure
      #---------------
      
      message(paste0("Fitting classifier: ", match(f, features), "/", length(features), "."))
      
      tmp <- data_id %>% 
        dplyr::select(c(group, dplyr::all_of(f))) %>%
        dplyr::rename(values = 2)
      
      storage <- list()
      
      for(n in 1:num_splits){
        
        message(paste0("Performing computations for split ", n, "/", num_splits))
        inputData <- prepare_univariate_model_matrices(data = tmp, seed = n)
        trainset <- as.data.frame(inputData[1])
        testset <- as.data.frame(inputData[2])
        modelOutputs <- fit_univariate_models(traindata = trainset, testdata = testset, test_method = test_method)
        storage[[n]] <- modelOutputs
      }
      
      prelimResults <- data.table::rbindlist(storage, use.names = TRUE)
      
      if(test_method == "linear svm"){
        classifier_name <- "Linear SVM"
      } else{
        classifier_name <- "RBF SVM"
      }
      
      statistic_name <- "Mann-Whitney-Wilcoxon Test statistic"
      
      if(pool_empirical_null){
        
        featResults <- prelimResults %>%
          dplyr::mutate(classifier_name = classifier_name,
                        feature = feature_names[f - 2])
        
      } else{
        
        mod <- stats::wilcox.test(statistic ~ category, data = prelimResults)
        statistic_value <- as.numeric(mod$statistic)
        p_value <- mod$p.value
      }
    } else{
      
      # Filter dataset
      
      tmp <- data_id %>%
        dplyr::select(c(group, dplyr::all_of(f))) %>%
        dplyr::rename(values = 2) %>%
        dplyr::mutate(group = as.factor(group))
      
      # Perform calculations between the two groups
      
      mod <- stats::glm(group ~ values, data = tmp, family = stats::binomial())
      
      # Extract statistics
      
      classifier_name <- "Binomial logistic regression"
      statistic_name <- "Binomial logistic coefficient z-test"
      statistic_value <- as.numeric(summary(mod)$coefficients[,3][2])
      p_value <- as.numeric(summary(mod)$coefficients[,4][2])
    }
    
    # Put results into dataframe
    
    if(pool_empirical_null){
      
    } else{
      featResults <- data.frame(feature = feature_names[f - 2],
                                classifier_name = classifier_name,
                                sig_statistic_name = statistic_name,
                                sig_statistic_value = statistic_value,
                                p_value = p_value)
    }
    
    results[[f]] <- featResults
  }
  
  allOutputs <- data.table::rbindlist(results, use.names = TRUE)
  
  if(pool_empirical_null){
    
    # Compute statistics for each feature
    
    pooled_storage <- list()
    
    nulls <- allOutputs %>%
      dplyr::filter(category == "Null")
    
    for(f in features){
      
      tmp_pool <- allOutputs %>%
        dplyr::filter(feature == feature_names[f - 2]) %>%
        dplyr::filter(category == "Main")
      
      tmp_pool <- dplyr::bind_rows(tmp_pool, nulls) %>%
        dplyr::mutate(category = factor(category, levels = c("Null", "Main")))
      
      mod <- stats::wilcox.test(statistic ~ category, data = tmp_pool)
      statistic_value <- as.numeric(mod$statistic)
      p_value <- mod$p.value
      
      pooled_outputs <- data.frame(feature = feature_names[f - 2],
                                   classifier_name = classifier_name,
                                   sig_statistic_name = statistic_name,
                                   sig_statistic_value = statistic_value,
                                   p_value = p_value)
      
      pooled_storage[[f]] <- pooled_outputs
    }
    
    pooled_outputs <- data.table::rbindlist(pooled_storage)
    
    return(pooled_outputs)
    
  } else{
    return(allOutputs)
  }
}
