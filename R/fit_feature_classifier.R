#' Fit a classifier to feature matrix to extract top performers
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
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
                                   method = c("t-test", "lm", "linear svm", "rbf svm")){
  
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
  
  methods <- c("t-test", "lm", "linear svm", "rbf svm")
  
  if(method %ni% methods){
    stop("classification_method should be a single string specification of 't-test', 'lm', 'linear svm', or 'rbf svm'.")
  }
  
  if(length(method) != 1){
    stop("classification_method should be a single string specification of 't-test', 'lm', 'linear svm', or 'rbf svm'.")
  }
  
  num_classes <- length(unique(normed$group)) # Get number of classes in the data
  
  if(num_classes == 1){
    stop("Your data only has one class label. At least two are required to performed analysis.")
  }
  
  if(is.null(method) && num_classes == 2){
    method <- "t-test"
    message("method is NULL. Running t-test for 2-class problem.")
  }
  
  if(is.null(method) && num_classes > 2){
    method <- "linear svm"
    message("method is NULL. Running linear svm for multiclass problem.")
  }
  
  if(method == "t-test" && num_classes > 2){
    stop("t-test can only be run for 2-class problems.")
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
  
  # Normalisation
  
  normed <- data_id %>%
    dplyr::select(c(id, names, values, group)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = normalise_feature_vector(values, method = method)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na()
    
  if(nrow(normed) != nrow(data_id)){
    message("Filtered out rows containing NaNs.")
  }
  
  # Widening for model matrix
  
  normed <- normed %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    dplyr::mutate(group = as.factor(group))
  
  #------------- Fit classifiers -------------
  
  # Loop over features and fit appropriate model
  
  features <- seq(from = 2, to = ncol(normed))
  results <- list()
  feature_names <- colnames(normed)
  feature_names <- feature_names[!feature_names %in% c("group")] # Remove group column name
  set.seed(123)
  message("Performing calculations... This may take a while depending on the number of features and classes in your dataset.")
  
  for(f in features){
    
    if(method == "t-test"){
      
      # Filter dataset
      
      tmp <- normed %>%
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
      
    } else if (method == "linear svm"){
      
      message(paste0("Fitting classifier: ", match(f, features),"/",length(features)))
      
      tmp <- normed %>%
        dplyr::select(c(group, dplyr::all_of(f)))
      
      # Fit classifier
      
      mod <- e1071::svm(group ~., data = tmp, kernel = "linear", cross = 10, probability = TRUE)
      
      # Get outputs for main model
      
      cm <- table(tmp$group, predict(mod))
      statistic <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      statistic_name <- "Classification accuracy"
      
    } else if (method == "rbf svm"){
      
      message(paste0("Fitting classifier: ", match(f, features),"/",length(features)))
      
      tmp <- normed %>%
        dplyr::select(c(group, dplyr::all_of(f)))
      
      # Fit classifier
      
      mod <- e1071::svm(group ~., data = tmp, kernel = "radial", cross = 10, probability = TRUE)
      
      # Get outputs
      
      cm <- table(tmp$group, predict(mod))
      statistic <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
      statistic_name <- "Classification accuracy"
      
    } else {
      
      # Filter dataset
      
      tmp <- normed %>%
        dplyr::select(c(group, dplyr::all_of(f))) %>%
        dplyr::rename(values = 2) %>%
        dplyr::mutate(group = as.factor(group))
      
      # Perform calculations between the two groups
      
      mod <- lm(values ~ group, data = tmp)
      
      # Extract statistics
      
      statistic_name <- "Linear regression coefficient"
      statistic <- summary(mod)$coefficients[,3]
      p_value <- summary(mod)$coefficients[,4]
    }
    
    # Put results into dataframe
    
    featResults <- data.frame(feature = feature_names[f-1],
                              test_statistic_name = statistic_name,
                              test_statistic_value = statistic,
                              p_value = p_value)
    
    results[[f]] <- featResults
  }
  
  # Bind feature results together and return
  
  classificationResults <- data.table::rbindlist(results, use.names = TRUE)
  return(classificationResults)
}
