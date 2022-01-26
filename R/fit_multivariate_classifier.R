#--------------- Helper functions ----------------

#-------------
# Data widener
#-------------

widener <- function(data, scaledata){
  
  tmpWide <- data %>%
    tidyr::pivot_longer(cols = 3:ncol(data), names_to = "names", values_to = "values") %>%
    dplyr::inner_join(scaledata, by = c("names" = "names")) %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = (values - mean) / sd) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(mean, sd)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    dplyr::mutate(group = as.factor(group))
  
  return(tmpWide)
}

#-------------
# Model matrix
#-------------

prepare_multivariate_model_matrices <- function(.data, use_k_fold, num_folds){
  
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
    storage <- trainData[ , colSums(is.na(storage)) < nrow(storage)]
    
    if(ncols < ncol(storage)){
      print(paste0("Removed ", (ncol(storage) - ncols), " features due to containing NA values after normalising."))
    }
  }
  return(storage)
}

#---------------------
# Main model procedure
#---------------------

fit_multivariate_feature_model <- function(traindata, testdata, kernel){
  
  mod <- e1071::svm(group ~ ., data = traindata, kernel = kernel, scale = FALSE, probability = TRUE)
  
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
  
  tmp_feature <- data.frame(statistic = statistic)
  
  return(tmp_feature)
}

fit_empirical_null_multivariate_models <- function(maindata, testdata, kernel, x = NULL, s){
  
  # Print out updates for every 10 shuffles so the user gets a time guesstimate that isn't burdensome
  
  if (s %% 10 == 0) {
    print(paste0("Calculating shuffle ", s))
  }
  
  # Null shuffles and computations
  
  y <- maindata %>% dplyr::pull(group)
  y <- as.character(y)
  
  set.seed(s)
  shuffles <- sample(y, replace = FALSE)
  
  shuffledtrain <- maindata %>%
    dplyr::mutate(group = shuffles,
                  group = as.factor(group))
  
  null_models <- fit_multivariate_feature_model(traindata = shuffledtrain, testdata = testdata, kernel = kernel) %>%
    dplyr::mutate(shuffle = s)
  
  if(!is.null(x)){
    null_models <- null_models %>%
      dplyr::mutate(fold = x)
  } else{
    
  }
  return(null_models)
}

#--------------
# Model fitting
#--------------

fit_multivariate_models <- function(data, test_method, use_k_fold, num_folds, use_empirical_null, num_shuffles, set = NULL){
  
  if(!is.null(set)){
    
    message(paste0("Calculating models for ", set))
    
    tmp <- data %>%
      dplyr::filter(method == set) %>%
      dplyr::select(-c(method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")
    
  } else{
    
    tmp <- data %>%
      dplyr::select(-c(method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")
  }
  
  inputData <- prepare_multivariate_model_matrices(.data = tmp, use_k_fold = use_k_fold, num_folds = num_folds)
  
  # Main procedure
  
  if(test_method == "linear svm"){
    kernel <- "linear"
  } else{
    kernel <- "radial"
  }
  
  fit_multivariate_feature_model_safe <- purrr::possibly(fit_multivariate_feature_model, otherwise = NA_character_)
  fit_empirical_null_multivariate_models_safe <- purrr::possibly(fit_empirical_null_multivariate_models, otherwise = NA_character_)
  
  if(use_k_fold){
    
    mainOuts <- 1:num_folds %>%
      purrr::map(~ fit_multivariate_feature_model_safe(traindata = inputData[[.x]]$trainData, 
                                                       testdata = inputData[[.x]]$testData, 
                                                       kernel = kernel))
    
    # Remove any pure NA entries that mark errors
    
    mainOuts <- base::Filter(Negate(anyNA), mainOuts)
    
    mainOuts <- data.table::rbindlist(mainOuts, use.names = TRUE) %>%
      dplyr::mutate(category = "Main")
    
    # Get outputs for empirical null
    
    if(use_empirical_null){
      
      combinationsNULL <- tidyr::crossing(1:num_folds, 1:num_shuffles)
      future::plan(future::multisession, workers = (future::availableCores() - 1))
      
      nullOuts <- furrr::future_pmap(list(combinationsNULL$`1:num_folds`, combinationsNULL$`1:num_shuffles`), ~  
                                fit_empirical_null_multivariate_models_safe(maindata = inputData[[.x]]$trainData, 
                                                                            testdata = inputData[[.x]]$testData, 
                                                                            kernel = kernel, 
                                                                            x = .x,
                                                                            s = .y), 
                                .options = furrr::furrr_options(seed = TRUE))
      
      future::plan(future::sequential)
      
      # Remove any pure NA entries that mark errors
      
      nullOuts <- base::Filter(Negate(anyNA), nullOuts)
      
      message("Averaging empirical null classification accuracies for each shuffle over folds.")
      
      nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
        dplyr::group_by(shuffle) %>%
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
    
    mainOuts <- fit_multivariate_feature_model(traindata = inputData, 
                                               testdata = inputData, 
                                               kernel = kernel) %>%
      dplyr::mutate(category = "Main")
    
    # Get outputs for empirical null
    
    if(use_empirical_null){
      
      nullOuts <- 1:num_shuffles %>%
        purrr::map(~ fit_empirical_null_multivariate_models_safe(maindata = inputData, 
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
  
  if(!is.null(set)){
    finalOuts <- finalOuts %>%
      dplyr::mutate(method = set)
  } else{
  }
  
  return(finalOuts)
}

#--------------------
# p-value calculation
#--------------------

calculate_multivariate_statistics <- function(data, set = NULL){
  
  if(!is.null(set)){
    tmp <- data %>%
      dplyr::filter(method == set)
  } else{
    tmp <- data
  }
  
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
  
  tmp_outputs <- data.frame(statistic_value = statistic_value,
                            p_value = p_value)
  
  if(!is.null(set)){
    tmp_outputs <- tmp_outputs %>%
      dplyr::mutate(method = set) %>%
      dplyr::select(c(method, statistic_value, p_value))
  } else{
    
  }
  
  return(tmp_outputs)
}

#---------------- Main function ----------------

#' Fit a classifier to feature matrix using all features or all features by set
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na pivot_wider crossing
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats sd reorder
#' @importFrom purrr possibly map
#' @importFrom future plan availableCores sequential multisession
#' @importFrom furrr future_map future_pmap
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to FALSE
#' @param test_method the algorithm to use for quantifying class separation
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values if linear svm or rbf svm is selected. Defaults to FALSE
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to FALSE
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if use_k_fold is set to TRUE. Defaults to 10
#' @param num_shuffles an integer specifying the number of class label shuffles to perform. Defaults to 50
#' @return an object of class list containing summaries of the classification models
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
#' fit_multivariate_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   by_set = FALSE,
#'   test_method = "linear svm",
#'   use_empirical_null = TRUE,
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   num_shuffles = 50) 
#' }
#' 

fit_multivariate_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE, test_method = c("linear svm", "rbf svm"),
                                        use_empirical_null = FALSE, use_k_fold = FALSE,
                                        num_folds = 0, num_shuffles = 50){
  
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
  
  # Set defaults for classification method
  
  if(((missing(test_method) || is.null(test_method))) && num_classes > 1){
    test_method <- "linear svm"
    message("test_method is NULL or missing, fitting 'linear svm' by default.")
  }
  
  methods <- c("linear svm", "rbf svm")
  
  if(test_method %ni% methods){
    stop("test_method should be a single string specification of 'linear svm' or 'rbf svm'.")
  }
  
  if(length(test_method) != 1){
    stop("test_method should be a single string specification of 'linear svm' or 'rbf svm'.")
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
  
  if(use_k_fold == TRUE && num_folds < 2){
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
  
  # Re-join method (set) labels
  
  data_id <- data_id %>%
    tidyr::pivot_longer(cols = 3:ncol(data_id), names_to = "names", values_to = "values") %>%
    dplyr::mutate(method = gsub("_.*", "\\1", names))
  
  #------------- Fit models -------------------
  
  #---------------------
  # Set up useful method 
  # information
  #---------------------
  
  if(test_method == "linear svm") {
    
    classifier_name <- "Linear SVM"
    statistic_name <- "Classification accuracy"
    
  } else {
    
    classifier_name <- "RBF SVM"
    statistic_name <- "Classification accuracy"
  }
  
  if(by_set){
    
    sets <- unique(data_id$method)
    
    # Compute accuracies for each feature set
    
    output <- sets %>%
      purrr::map(~ fit_multivariate_models(data = data_id, 
                                           test_method = test_method, 
                                           use_k_fold = use_k_fold, 
                                           num_folds = num_folds, 
                                           use_empirical_null = use_empirical_null, 
                                           num_shuffles = num_shuffles, 
                                           set = .x))
    
    output <- data.table::rbindlist(output, use.names = TRUE)
    
  } else{
    
    output <- fit_multivariate_models(data = data_id, 
                                      test_method = test_method, 
                                      use_k_fold = use_k_fold, 
                                      num_folds = num_folds, 
                                      use_empirical_null = use_empirical_null, 
                                      num_shuffles = num_shuffles,
                                      set = NULL)
  }
  
  #--------------- Evaluate results ---------------
  
  if(by_set){
      
    #---------- Draw bar plot ---------
      
    # Get final number of features used by set
      
    feat_nums <- data %>% 
      dplyr::select(c(method, names)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(method) %>%
      dplyr::summarise(num_feats = dplyr::n()) %>%
      dplyr::ungroup()
      
    # Draw plot
      
    FeatureSetResultsPlot <- output %>%
      dplyr::filter(category == "Main") %>%
      dplyr::mutate(statistic = statistic * 100) %>%
      dplyr::group_by(method) %>%
      dplyr::summarise(average = mean(statistic, na.rm = TRUE),
                       lower = average - 2 * stats::sd(statistic, na.rm = TRUE),
                       upper = average + 2 * stats::sd(statistic, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    if(max(FeatureSetResultsPlot$upper) > 100){
      message("Truncating two times standard deviation upper bound to 100% accuracy for plot.")
    }
    
    FeatureSetResultsPlot <- FeatureSetResultsPlot %>%
      dplyr::mutate(upper = ifelse(upper > 100, 100, upper)) %>% # Truncate for plot to get bar to appear in limits
      dplyr::inner_join(feat_nums, by = c("method" = "method")) %>%
      dplyr::mutate(method = paste0(method, " (", num_feats, ")")) %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(method, -average))) +
      ggplot2::geom_bar(ggplot2::aes(y = average, fill = method), stat = "identity") +
      ggplot2::geom_point(ggplot2::aes(y = average), colour = "black") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), colour = "black") +
      ggplot2::labs(title = "Classification accuracy by feature set",
                    subtitle = "Error bars represent mean +- two times the standard deviation.\nNumber of features in each set retained for analysis is indicated in parentheses.",
                    x = "Feature set",
                    y = "Classification accuracy (%)",
                    fill = NULL) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(limits = c(0, 100),
                                  breaks = seq(from = 0, to = 100, by = 20),
                                  labels = function(x) paste0(x, "%")) +
      ggplot2::scale_fill_brewer(palette = "Dark2") +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      
    #---------- Compute p values ------
      
    if(use_empirical_null){
      
      calculate_multivariate_statistics_safe <- purrr::possibly(calculate_multivariate_statistics, otherwise = NA_character_)
        
      TestStatistics <- sets %>%
        purrr::map(~ calculate_multivariate_statistics_safe(data = output, set = .x))
      
      # Remove any pure NA entries that mark errors
      
      TestStatistics <- base::Filter(Negate(anyNA), TestStatistics)
      
      TestStatistics <- data.table::rbindlist(TestStatistics, use.names = TRUE) %>%
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
          dplyr::select(-c(category))
        
        myList <- list(FeatureSetResultsPlot, output)
        names(myList) <- c("FeatureSetResultsPlot", "RawClassificationResults")
    }
  } else{
      
    if(use_empirical_null){
      
      TestStatistics <- calculate_multivariate_statistics(data = output, set = NULL) %>%
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
        dplyr::select(-c(category))
      
      myList <- list(output)
      names(myList) <- c("RawClassificationResults") 
    }
  }
  return(myList)
}
