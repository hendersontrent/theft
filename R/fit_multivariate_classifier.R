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

prepare_model_matrices <- function(data, seed){
  
  # Pivot wider for correct train-test splits
  
  mydata2 <- data %>%
    dplyr::mutate(names = paste0(method, "_", names)) %>%
    dplyr::select(-c(method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")
  
  ncols <- ncol(mydata2)
  
  # Delete features that are all NaNs and features with constant values
  
  mydata2 <- mydata2 %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::select(where(~dplyr::n_distinct(.) > 1))
  
  if(ncol(mydata2) < ncols){
    message(paste0("Dropped ", ncols - ncol(mydata2), " features due to containing NAs or only a constant."))
  }
  
  # Check NAs
  
  nrows <- nrow(mydata2)
  
  mydata2 <- mydata2 %>%
    dplyr::filter(!is.na(group))
  
  if(nrow(mydata2) < nrows){
    message(paste0("Dropped ", nrows - nrow(mydata2), " time series due to NaN values in the 'group' variable."))
  }
  
  # Train-test split
  
  set.seed(seed)
  bound <- floor((nrow(mydata2)/4)*3)
  mydata2 <- mydata2[sample(nrow(mydata2)), ]
  train <- mydata2[1:bound, ]
  test <- mydata2[(bound + 1):nrow(mydata2), ]
  
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

#--------------
# Model fitting
#--------------

fit_multivariate_models <- function(traindata, testdata, test_method){
  
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

#---------------- Main function ----------------

#' Fit a classifier to feature matrix using all features or all features by set
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats sd reorder wilcox.test
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to FALSE
#' @param num_splits an integer specifying the number of 75/25 train-test splits to perform for error bars. Defaults to 10
#' @param test_method the algorithm to use for quantifying class separation
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
#'   num_splits = 10,
#'   test_method = "linear svm") 
#' }
#' 

fit_multivariate_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE, num_splits = 10,
                                        test_method = c("linear svm", "rbf svm")){
  
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
                    group = dplyr::all_of(group_var))
  }
  
  num_classes <- length(unique(data_id$group)) # Get number of classes in the data
  
  if(num_classes == 1){
    stop("Your data only has one class label. At least two are required to performed analysis.")
  }
  
  # Set defaults for classification method
  
  if((missing(test_method) || is.null(test_method)) && num_classes > 1){
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
  
  # Splits
  
  if(!is.numeric(num_splits)){
    stop("num_splits should be an integer >=1 specifying the number of train-test splits to perform.")
  }
  
  if(num_splits < 1){
    stop("num_splits should be an integer >=1 specifying the number of train-test splits to perform.")
  }
  
  #------------- Fit models -------------------
  
  if(by_set){
    
    sets <- unique(data_id$method)
    storage <- list()
    setStorage <- list()
    
    for(s in sets){
      
      storage2 <- list()
      setStorage2 <- list()
      
      setData <- data_id %>%
        dplyr::filter(method == s)
      
      for(n in 1:num_splits){
        
        message(paste0("Performing computations for ", s, ", split ", n, "/", num_splits))
        inputData <- prepare_model_matrices(data = setData, seed = n)
        trainset <- as.data.frame(inputData[1])
        testset <- as.data.frame(inputData[2])
        removals <- sapply(trainset, function(y) sum(length(which(is.na(y)))))
        removals <- data.frame(removals) %>% tibble::rownames_to_column(var = "names") %>% dplyr::filter(removals >= 1) %>% dplyr::pull(names)
        mytrainset <- trainset %>% dplyr::select(-dplyr::all_of(removals))
        mytestset <- testset %>% dplyr::select(-dplyr::all_of(removals))
        modelOutputs <- fit_multivariate_models(traindata = mytrainset, testdata = mytestset, test_method = test_method)
        storage2[[n]] <- modelOutputs
        setStorage2[[n]] <- data.frame(num_feats = (ncol(mytrainset) - 1))
      }
      results2 <- data.table::rbindlist(storage2, use.names = TRUE) %>%
        dplyr::mutate(method = s)
      
      setResults2 <- data.table::rbindlist(setStorage2, use.names = TRUE) %>%
        dplyr::mutate(method = s)
      
      storage[[s]] <- results2
      setStorage[[s]] <- setResults2
    }
    results <- data.table::rbindlist(storage, use.names = TRUE)
    setResults <- data.table::rbindlist(setStorage, use.names = TRUE)
    
  } else{
    
    storage <- list()
    
    for(n in 1:num_splits){
      
      message(paste0("Performing computations for split ", n, "/", num_splits))
      inputData <- prepare_model_matrices(data = data_id, seed = n)
      trainset <- as.data.frame(inputData[1])
      testset <- as.data.frame(inputData[2])
      removals <- sapply(trainset, function(y) sum(length(which(is.na(y)))))
      removals <- data.frame(removals) %>% tibble::rownames_to_column(var = "names") %>% dplyr::filter(removals >= 1) %>% dplyr::pull(names)
      mytrainset <- trainset %>% dplyr::select(-dplyr::all_of(removals))
      mytestset <- testset %>% dplyr::select(-dplyr::all_of(removals))
      modelOutputs <- fit_multivariate_models(traindata = mytrainset, testdata = mytestset, test_method = test_method)
      storage[[n]] <- modelOutputs
    }
    results <- data.table::rbindlist(storage, use.names = TRUE)
  }
  
  #--------------- Evaluate results ---------------
  
  if(by_set){
      
    #---------- Draw bar plot ---------
      
    # Get final number of features used by set
      
    feat_nums <- setResults %>% 
      dplyr::group_by(method) %>%
      dplyr::summarise(num_feats = mean(num_feats, na.rm = TRUE)) %>%
      dplyr::ungroup()
      
    # Draw plot
      
    FeatureSetResultsPlot <- results %>%
      dplyr::filter(category == "Main") %>%
      dplyr::mutate(statistic = statistic * 100) %>%
      dplyr::group_by(method) %>%
      dplyr::summarise(average = mean(statistic, na.rm = TRUE),
                       lower = average - 2 * stats::sd(statistic, na.rm = TRUE),
                       upper = average + 2 * stats::sd(statistic, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(feat_nums, by = c("method" = "method")) %>%
      dplyr::mutate(method = paste0(method, " (", num_feats, ")")) %>%
      ggplot2::ggplot(aes(x = stats::reorder(method, -average))) +
      ggplot2::geom_bar(aes(y = average, fill = method), stat = "identity") +
      ggplot2::geom_point(aes(y = average), colour = "black") +
      ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper), colour = "black") +
      ggplot2::labs(title = "Classification accuracy by feature set",
                    subtitle = "Error bars represent mean +- two times the standard deviation.\nNumber of features in each set used for analysis is indicated in parentheses.",
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
      
    message("Computing p-values between main and null models for each feature set using Mann-Whitney-Wilcoxon Test.")
    sets <- unique(results$method)
    p_val_storage <- list()
      
    for(s in sets){
      tmp_p_val <- results %>% dplyr::filter(method == s)
      p_value <- stats::wilcox.test(statistic ~ category, data = tmp_p_val)$p.value
      p_val_storage[[s]] <- data.frame(method = s, p_value = p_value)
    }
      
    FeatureSetTestStatistics <- data.table::rbindlist(p_val_storage, use.names = TRUE)
    
    # Return results
    
    myList <- list(FeatureSetResultsPlot, FeatureSetTestStatistics, results)
    names(myList) <- c("FeatureSetResultsPlot", "FeatureSetTestStatistics", "RawClassificationResults")
      
  } else{
      
    p_value <- stats::wilcox.test(statistic ~ category, data = results)$p.value
    print(paste0("p-value for comparison of main models to empirical null: ", round(p_value, digits = 7)))
    myList <- list(results)
    names(myList) <- c("RawClassificationResults")
  }
  return(myList)
}
