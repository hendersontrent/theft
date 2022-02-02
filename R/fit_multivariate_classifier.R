#--------------- Helper functions ----------------

#---------------------
# Main model procedure
#---------------------

calculate_recall <- function(matrix, x){
  
  tp <- as.numeric(matrix[x, x])
  fn <- sum(matrix[x, -x])
  recall_num <- tp / (tp + fn)
  return(recall_num)
}

extract_prediction_accuracy <- function(mod, testData, use_balanced_accuracy){
  
  cm <- table(testData$group, predict(mod, newdata = testData))
  
  if(use_balanced_accuracy){
    
    # Calculate recall for each class and use to calculate balanced accuracy as per https://neptune.ai/blog/balanced-accuracy
    
    cm <- as.matrix(cm)
    
    recall <- 1:nrow(cm) %>%
      purrr::map(~ calculate_recall(cm, x = .x)) %>%
      unlist()
    
    statistic <- sum(recall) / length(recall)
    
  } else{
    
    cm <- as.data.frame(cm) %>%
      dplyr::mutate(flag = ifelse(Var1 == Var2, "Same", "Different"))
    
    same_total <- cm %>%
      dplyr::filter(flag == "Same") %>%
      dplyr::summarise(Freq = sum(Freq)) %>%
      dplyr::pull()
    
    all_total <- cm %>%
      dplyr::summarise(Freq = sum(Freq)) %>%
      dplyr::pull()
    
    statistic <- same_total / all_total
  }
  
  tmp_feature <- data.frame(statistic = statistic)
  
  return(tmp_feature)
}

fit_empirical_null_multivariate_models <- function(mod, testdata, s, use_balanced_accuracy){
  
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
    dplyr::mutate(group = shuffles)
  
  null_models <- extract_prediction_accuracy(mod = mod, testData = shuffledtest, use_balanced_accuracy = use_balanced_accuracy)
  return(null_models)
}

#--------------
# Model fitting
#--------------

fit_multivariate_models <- function(data, test_method, use_k_fold, num_folds, use_empirical_null, num_shuffles, split_prop, set = NULL,
                                    use_balanced_accuracy){
  
  if(!is.null(set)){
    
    message(paste0("Calculating models for ", set))
    
    tmp <- data %>%
      dplyr::filter(method == set) %>%
      dplyr::select(-c(method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(id))
    
  } else{
    
    tmp <- data %>%
      dplyr::select(-c(method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(id))
  }
  
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
                        preProcess = c("center", "scale", "nzv"))
    
  } else{
    
    mod <- caret::train(group ~ ., 
                        data = dataTrain, 
                        method = test_method, 
                        preProcess = c("center", "scale", "nzv"))
  }
  
  # Get main predictions
  
  mainOuts <- extract_prediction_accuracy(mod = mod, testData = dataTest, use_balanced_accuracy = use_balanced_accuracy) %>%
    dplyr::mutate(category = "Main")
    
  if(use_empirical_null){
      
    nullOuts <- 1:num_shuffles %>%
      purrr::map( ~ fit_empirical_null_multivariate_models(mod = mod, 
                                                           testdata = dataTest, 
                                                           s = .x,
                                                           use_balanced_accuracy = use_balanced_accuracy))
      
    nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
      dplyr::mutate(category = "Null")
      
    finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
      
    } else{
      finalOuts <- mainOuts
    }
  
  if(!is.null(set)){
    finalOuts <- finalOuts %>%
      dplyr::mutate(method = set,
                    num_features_used = (ncol(dataTrain) - 1))
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
#' @importFrom data.table rbindlist
#' @importFrom stats sd reorder
#' @importFrom purrr map
#' @importFrom janitor clean_names
#' @importFrom caret createDataPartition preProcess train
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to FALSE
#' @param test_method the algorithm to use for quantifying class separation
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values. Defaults to FALSE
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to FALSE
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if use_k_fold is set to TRUE.  Defaults to 10
#' @param split_prop a double between 0 and 1 specifying the proportion of input data that should go into the training set (therefore 1 - p goes into the test set). Defaults to 0.8
#' @param num_shuffles an integer specifying the number of class label shuffles to perform. Defaults to 50
#' @param use_balanced_accuracy a Boolean specifying whether to use balanced accuracy as the performance metric instead of overall accuracy
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
#'   test_method = "gaussprRadial",
#'   use_empirical_null = TRUE,
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   split_prop = 0.8,
#'   num_shuffles = 50,
#'   use_balanced_accuracy = FALSE) 
#' }
#' 

fit_multivariate_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE, test_method = "gaussprRadial",
                                        use_empirical_null = FALSE, use_k_fold = FALSE,
                                        num_folds = 10, split_prop = 0.8, num_shuffles = 50,
                                        use_balanced_accuracy = FALSE){
  
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
  
  if(num_classes < 2){
    stop("Your data has less than two unique classes. At least two are required to performed classification analysis.")
  }
  
  # Set defaults for classification method
  
  if(((missing(test_method) || is.null(test_method)))){
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
    dplyr::filter(!is.na(group))
  
  if(nrow(data_id) < nrows){
    message(paste0("Dropped ", nrows - nrow(data_id), " time series due to NaN values in the 'group' variable."))
  }
  
  # Clean up column (feature) names so models fit properly (mainly an issue with SVM formula) and re-join set labels
  # and prep factor levels as names for {caret} if the 3 base two-class options aren't being used
  
  data_id <- data_id %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = 3:ncol(data_id), names_to = "names", values_to = "values") %>%
    dplyr::mutate(method = gsub("_.*", "\\1", names)) %>%
    dplyr::mutate(group = make.names(group),
                  group = as.factor(group))
  
  #------------- Fit models -------------------
  
  #---------------------
  # Set up useful method 
  # information
  #---------------------
  
  classifier_name <- test_method
  
  if(use_balanced_accuracy){
    statistic_name <- "Balanced classification accuracy"
  } else{
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
                                           split_prop = split_prop,
                                           set = .x,
                                           use_balanced_accuracy = use_balanced_accuracy))
    
    output <- data.table::rbindlist(output, use.names = TRUE)
    
  } else{
    
    output <- fit_multivariate_models(data = data_id, 
                                      test_method = test_method, 
                                      use_k_fold = use_k_fold, 
                                      num_folds = num_folds, 
                                      use_empirical_null = use_empirical_null, 
                                      num_shuffles = num_shuffles,
                                      split_prop = split_prop,
                                      set = NULL,
                                      use_balanced_accuracy = use_balanced_accuracy)
  }
  
  #--------------- Evaluate results ---------------
  
  if(by_set){
      
    #---------- Draw bar plot ---------
    
    # Get final number of features used by set
    
    feat_nums <- data_id %>% 
      dplyr::select(c(method, names)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(method) %>%
      dplyr::summarise(num_feats = dplyr::n()) %>%
      dplyr::ungroup()
      
    # Draw plot
      
    FeatureSetResultsPlot <- output %>%
      dplyr::filter(category == "Main") %>%
      dplyr::inner_join(feat_nums, by = c("method" = "method")) %>%
      dplyr::mutate(method = paste0(method, " (", num_feats, ")")) %>%
      dplyr::mutate(statistic = statistic * 100) %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(method, -statistic))) +
      ggplot2::geom_bar(ggplot2::aes(y = statistic, fill = method), stat = "identity")
    
    if(use_balanced_accuracy){
      
      FeatureSetResultsPlot <- FeatureSetResultsPlot +
        ggplot2::labs(title = "Balanced classification accuracy by feature set",
                      y = "Balanced classification accuracy (%)")
      
    } else{
      
      FeatureSetResultsPlot <- FeatureSetResultsPlot +
        ggplot2::labs(title = "Classification accuracy by feature set",
                      y = "Classification accuracy (%)")
    }
    
    FeatureSetResultsPlot <- FeatureSetResultsPlot +
      ggplot2::labs(subtitle = "Number of features in each set used for analysis is indicated in parentheses",
                    x = "Feature set",
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
        
      TestStatistics <- sets %>%
        purrr::map(~ calculate_multivariate_statistics(data = output, set = .x))
      
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
