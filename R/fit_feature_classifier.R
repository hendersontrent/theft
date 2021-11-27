#' Fit a classifier to feature matrix to extract top performers
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats glm binomial
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param test_method the algorithm to use for quantifying class separation
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
#'   group_var = "group") 
#' }
#' 

fit_feature_classifier <- function(data, id_var = "id", group_var = "group",
                                   test_method = c("t-test", "binomial logistic", "linear svm", "rbf svm")){
  
  #---------- Check arguments ------------
  
  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')
  
  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least two columns called 'names' and 'values'. These are automatically produced by feature calculations such as calculate_features(). Please consider running one of these first and then passing the resultant dataframe in to this function.")
  }
  
  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least two columns called 'names' and 'values'. These are automatically produced by feature calculations such as calculate_features(). Please consider running one of these first and then passing the resultant dataframe in to this function.")
  }
  
  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }
  
  if(!is.null(id_var) && !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  # Set defaults for classification method
  
  methods <- c("t-test", "binomial logistic", "linear svm", "rbf svm")
  
  if(test_method %ni% methods){
    stop("test_method should be a single string specification of 't-test', 'binomial logistic', 'linear svm', or 'rbf svm'.")
  }
  
  if(length(test_method) != 1){
    stop("test_method should be a single string specification of 't-test', 'binomial logistic', 'linear svm', or 'rbf svm'.")
  }
  
  num_classes <- length(unique(data$group)) # Get number of classes in the data
  
  if(num_classes == 1){
    stop("Your data only has one class label. At least two are required to performed analysis.")
  }
  
  if(missing(test_method) && num_classes == 2){
    test_method <- "t-test"
    message("test_method is NULL. Running t-test for 2-class problem.")
  }
  
  if(missing(test_method) && num_classes > 2){
    test_method <- "linear svm"
    message("test_method is NULL. Running linear svm for multiclass problem.")
  }
  
  if(test_method %in% c("t-test", "binomial logistic") && num_classes > 2){
    stop("t-test and binomial logistic regression can only be run for 2-class problems.")
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
  
  #------------- Preprocess data --------------
  
  # Widening for model matrix
  
  data_id <- data_id %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    dplyr::mutate(group = as.factor(group))
  
  #------------- Fit classifiers -------------
  
  # Loop over features and fit appropriate model
  
  features <- seq(from = 2, to = ncol(data_id))
  results <- list()
  resultsNULL <- list()
  feature_names <- colnames(data_id)
  feature_names <- feature_names[!feature_names %in% c("group")] # Remove group column name
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
      
      statistic_name <- "Welch Two Sample t-test"
      statistic <- mod$statistic
      p_value <- mod$p.value
      
    } else if (test_method == "linear svm"){
      
      #---------------
      # Main procedure
      #---------------
      
      message(paste0("Fitting classifier: ", match(f, features),"/",length(features), " with 10-fold CV and generating 10 empirical null samples."))
      
      tmp <- data_id %>%
        dplyr::select(c(group, dplyr::all_of(f)))
      
      # Fit classifier
      
      mod <- e1071::svm(group ~., data = tmp, kernel = "linear", cross = 10, probability = TRUE)
      
      # Get outputs for main model
      
      cm <- table(tmp$group, predict(mod))
      statistic <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      statistic_name <- "Classification accuracy"
      
      #---------------
      # Empirical null
      #---------------
      
      # Generate shuffled class labels
      
      nullList <- list()
      repeats <- seq(from = 100, to = 1000, by = 100)
      
      for(r in repeats){
        
        set.seed(r)
        y <- tmp %>% dplyr::pull(1)
        y <- as.character(y)
        shuffles <- sample(y, replace = FALSE)
        
        tmp2 <- tmp %>%
          dplyr::mutate(group = shuffles,
                        group = as.factor(group))
        
        # Fit classifier
        
        modNULL <- e1071::svm(group ~., data = tmp2, kernel = "linear", cross = 10, probability = TRUE)
        
        # Get outputs for main model
        
        cmNULL <- table(tmp$group, predict(modNULL))
        statisticNULL <- (cmNULL[1,1] + cmNULL[2,2]) / (cmNULL[1,1] + cmNULL[2,2] + cmNULL[1,2] + cmNULL[2,1])
        nullStorage <- data.frame(test_statistic_value = statisticNULL)
        nullList[[r]] <- nullStorage
      }
      
      # Bind empirical nulls together
      
      tmpNULLs <- data.table::rbindlist(nullList, use.names = TRUE) %>%
        dplyr::pull(1)
      
    } else if (test_method == "rbf svm"){
      
      #---------------
      # Main procedure
      #---------------
      
      message(paste0("Fitting classifier: ", match(f, features),"/",length(features), " with 10-fold CV and generating 10 empirical null samples."))
      
      tmp <- data_id %>%
        dplyr::select(c(group, dplyr::all_of(f)))
      
      # Fit classifier
      
      mod <- e1071::svm(group ~., data = tmp, kernel = "radial", cross = 10, probability = TRUE)
      
      # Get outputs for main model
      
      cm <- table(tmp$group, predict(mod))
      statistic <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      statistic_name <- "Classification accuracy"
      
      #---------------
      # Empirical null
      #---------------
      
      # Generate shuffled class labels
      
      nullList <- list()
      repeats <- seq(from = 100, to = 1000, by = 100)
      
      for(r in repeats){
        
        set.seed(r)
        y <- tmp %>% dplyr::pull(1)
        y <- as.character(y)
        shuffles <- sample(y, replace = FALSE)
        
        tmp2 <- tmp %>%
          dplyr::mutate(group = shuffles,
                        group = as.factor(group))
        
        # Fit classifier
        
        modNULL <- e1071::svm(group ~., data = tmp2, kernel = "radial", cross = 10, probability = TRUE)
        
        # Get outputs for main model
        
        cmNULL <- table(tmp$group, predict(modNULL))
        statisticNULL <- (cmNULL[1,1] + cmNULL[2,2]) / (cmNULL[1,1] + cmNULL[2,2] + cmNULL[1,2] + cmNULL[2,1])
        nullStorage <- data.frame(test_statistic_value = statisticNULL)
        nullList[[r]] <- nullStorage
      }
      
      # Bind empirical nulls together
      
      tmpNULLs <- data.table::rbindlist(nullList, use.names = TRUE) %>%
        dplyr::pull(1)
      
    } else{
      
      # Filter dataset
      
      tmp <- data_id %>%
        dplyr::select(c(group, dplyr::all_of(f))) %>%
        dplyr::rename(values = 2) %>%
        dplyr::mutate(group = as.factor(group))
      
      # Perform calculations between the two groups
      
      mod <- stats::glm(group ~ values, data = tmp, family = stats::binomial())
      
      # Extract statistics
      
      statistic_name <- "Binomial logistic coefficient z-test"
      statistic <- as.numeric(summary(mod)$coefficients[,3][2])
      p_value <- as.numeric(summary(mod)$coefficients[,4][2])
    }
    
    # Put results into dataframe
    
    if(test_method %in% c("t-test", "logistic")){
      featResults <- data.frame(feature = feature_names[f-1],
                                test_statistic_name = statistic_name,
                                test_statistic_value = statistic,
                                p_value = p_value)
    } else{
      
      featResults <- data.frame(feature = feature_names[f-1],
                                test_statistic_name = statistic_name,
                                test_statistic_value = statistic)
      
      featResultsNULL <- data.frame(test_statistic_value = tmpNULLs) %>%
        dplyr::mutate(feature = feature_names[f-1])
      
      resultsNULL[[f]] <- featResultsNULL
    }
    
    results[[f]] <- featResults
  }
  
  # Bind feature results together and return
  
  classificationResults <- data.table::rbindlist(results, use.names = TRUE)
  
  if(test_method %in% c("t-test", "logistic")){
    return(classificationResults)
    
  } else{
    
    classificationResultsNULL <- data.table::rbindlist(resultsNULL, use.names = TRUE)
    
    # Compute p-value
    
    null_dist_mean <- mean(classificationResultsNULL$test_statistic_value, na.rm = TRUE)
    null_dist_sd <- sd(classificationResultsNULL$test_statistic_value, na.rm = TRUE)
    p_value <- 
    
    classificationResults <- classificationResults %>%
      dplyr::mutate(p_value = p_value)
    
    return(classificationResults)
  }
}
