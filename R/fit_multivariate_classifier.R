#--------------- Helper function ----------------

prepare_model_matrix <- function(data){
  
  # Widening for model matrix
  
  my_matrix <- data %>%
    dplyr::mutate(names_long = paste0(method, "_", names)) %>%
    dplyr::select(-c(names, method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names_long", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    dplyr::mutate(group = as.factor(group))
  
  # Check group variable NAs
  
  nrows <- nrow(my_matrix)
  
  my_matrix <- my_matrix %>%
    dplyr::filter(!is.na(group))
  
  if(nrow(my_matrix) < nrows){
    message(paste0("Dropped ", nrows - nrow(my_matrix), " rows due to NaN values in the 'group' variable column."))
  }
  
  # Delete columns (features) with NaNs and track the number that are deleted
  
  ncols <- ncol(my_matrix)
  
  my_matrix <- my_matrix %>%
    dplyr::select_if(~ !any(is.na(.)))
  
  if(ncol(my_matrix) < ncols){
    message(paste0("Dropped ", ncols - ncol(my_matrix), " features due to NaN values."))
  }
  return(my_matrix)
}

#---------------- Main function ----------------

#' Fit a classifier to feature matrix using all features or by set
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
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to FALSE
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
#' fit_multivariate_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   by_set = FALSE,
#'   test_method = "linear svm") 
#' }
#' 

fit_multivariate_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE,
                                        test_method = c("t-test", "binomial logistic", "linear svm", "rbf svm")){
  
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
  
  if(by_set){
    
    sets <- unique(data_id$method)
    
    for(s in sets){
      
      #------------- Preprocess data -------------
      
      setData <- data_id %>%
        dplyr::filter(method == s)
      
      inputData <- prepare_model_matrix(data = setData)
      
      #------------- Fit classifiers -------------
      
    }
    
  } else{
    
    #------------- Preprocess data -------------
    
    inputData <- prepare_model_matrix(data = data_id)
    
    #------------- Fit classifiers -------------
    
    
    
  }
}
