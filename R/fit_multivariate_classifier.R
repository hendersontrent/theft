#--------------- Helper functions ----------------

# Function for returning accuracies over the train procedure

extract_prediction_accuracy <- function(mod){
  
  results <- as.data.frame(mod$results) %>%
    dplyr::select(c(Accuracy, AccuracySD)) %>%
    dplyr::rename(statistic = Accuracy,
                  statistic_sd = AccuracySD)

  return(results)
}

# Function for iterating over random shuffle permutations of class labels

fit_empirical_null_multivariate_models <- function(data, s, test_method, theControl, pb){
  
  # Print {purrr} iteration progress updates in the console
  
  pb$tick()$print()
  
  # Null shuffles and computations
  
  y <- data %>% dplyr::pull(group)
  y <- as.character(y)
  
  set.seed(s)
  shuffles <- sample(y, replace = FALSE)
  
  shuffledtest <- data %>%
    dplyr::mutate(group = shuffles) %>%
    dplyr::mutate(group = as.factor(group))
  
  modNull <- caret::train(group ~ ., 
                          data = shuffledtest, 
                          method = test_method, 
                          trControl = theControl,
                          preProcess = c("center", "scale", "nzv"))
  
  null_models <- extract_prediction_accuracy(mod = modNull)
  return(null_models)
}

#--------------
# Model fitting
#--------------

fit_multivariate_models <- function(data, test_method, use_k_fold, num_folds, use_empirical_null, num_shuffles, set = NULL){
  
  # Set up input matrices
  
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
  
  # Fit models
  
  set.seed(123)
  
  if(use_k_fold){
    
    # Train model
    
    fitControl <- caret::trainControl(method = "cv",
                                      number = num_folds,
                                      classProbs = TRUE)
    
    mod <- caret::train(group ~ ., 
                        data = tmp, 
                        method = test_method, 
                        trControl = fitControl,
                        preProcess = c("center", "scale", "nzv"))
    
  } else{
    
    fitControl <- caret::trainControl(classProbs = TRUE)
    
    mod <- caret::train(group ~ ., 
                        data = tmp, 
                        method = test_method, 
                        trControl = fitControl,
                        preProcess = c("center", "scale", "nzv"))
  }
  
  # Get main predictions
  
  mainOuts <- extract_prediction_accuracy(mod = mod) %>%
    dplyr::mutate(category = "Main")
    
  if(use_empirical_null){
    
    # Set up progress bar
    
    pb <- progress_estimated(length(1:num_shuffles))
    
    # Run procedure
      
    nullOuts <- 1:num_shuffles %>%
      purrr::map( ~ fit_empirical_null_multivariate_models(data = tmp, 
                                                           s = .x,
                                                           test_method = test_method,
                                                           theControl = fitControl,
                                                           pb = pb))
      
    nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
      dplyr::mutate(category = "Null")
      
    finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)
    
    # Draw plot
    
    p <- nullOuts %>%
      ggplot2::ggplot(ggplot2::aes(x = statistic)) +
      ggplot2::geom_histogram(binwidth = 0.01, fill = "white", colour = "black", alpha = 0.5) +
      ggplot2::geom_vline(xintercept = mainOuts$statistic, lty = "dashed", size = 1, colour = "#1B9E77") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Classification accuracy (%)",
                    y = "Frequency",
                    subtitle = paste0("Dashed vertical line = true model; histogram = null accuracies from ", num_shuffles, " permutations"))
    
    if(!is.null(set)){
      p <- p +
        ggplot2::labs(title = paste0("True model classification accuracy against distribution of null accuracies for ", set))
      
      # get set information
      
      finalOuts <- finalOuts %>%
        dplyr::mutate(method = set,
                      num_features_used = (ncol(tmp) - 1))
    } else{
      p <- p +
        ggplot2::labs(title = "True model classification accuracy against distribution of null accuracies")
    }
    
    print(p)
      
    } else{
      finalOuts <- mainOuts
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
#' @importFrom caret preProcess train confusionMatrix
#' @param data the dataframe containing the raw feature data as calculated by \code{theft::calculate_features}
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to \code{"id"}
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to \code{"group"}
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to \code{FALSE}
#' @param test_method the algorithm to use for quantifying class separation. Defaults to \code{"gaussprRadial"}
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to \code{TRUE}
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if \code{use_k_fold} is set to \code{TRUE}. Defaults to \code{10}
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values. Defaults to \code{FALSE}
#' @param num_shuffles an integer specifying the number of class label shuffles to perform if \code{use_empirical_null} is \code{TRUE}. Defaults to \code{50}
#' @return an object of class list containing dataframe summaries of the classification models and a \code{ggplot} object if \code{by_set} is \code{TRUE}
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
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   use_empirical_null = TRUE,
#'   num_shuffles = 1000) 
#' }
#' 

fit_multivariate_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE, test_method = "gaussprRadial",
                                        use_k_fold = TRUE, num_folds = 10, 
                                        use_empirical_null = FALSE, num_shuffles = 1000){
  
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
  statistic_name <- "Mean classification accuracy"
  
  # Print some advice, as per https://faculty.washington.edu/kenrice/sisg/SISG-08-06.pdf
  
  #if(use_empirical_null){
  #  print(paste0("Computing empirical null(s) with ", num_shuffles, " permutations. This mean the smallest possible p-value is ", (1/num_shuffles), "."))
  #}
  
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
      dplyr::mutate(statistic = statistic * 100,
                    statistic_sd = statistic_sd * 100) %>%
      dplyr::mutate(lower = statistic - (2 * statistic_sd),
                    upper = statistic + (2 * statistic_sd)) %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(method, -statistic))) +
      ggplot2::geom_bar(ggplot2::aes(y = statistic, fill = method), stat = "identity") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), colour = "black") +
        ggplot2::labs(title = "Classification accuracy by feature set",
                      y = "Classification accuracy (%)",
                      subtitle = "Number of features is indicated in parentheses. Error bars are +- 2 times pointwise SD",
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
