#' Fit a classifier to feature matrix to extract top performers
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom caret train trainControl confusionMatrix
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

fit_feature_classifier <- function(data, id_var = "id", group_var = "group"){
  
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
    pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    mutate(group = as.factor(group))
  
  #------------- Fit classifier --------------
  
  # Get number of classes in the dataset to determine if binary or multiclass problem
  
  num_classes <- length(unique(normed$group))
  
  # Loop over features and fit linear SVM with 10-fold cross-validation
  
  features <- seq(from = 2, to = ncol(normed))
  results <- list()
  feature_names <- colnames(normed)
  feature_names <- feature_names[!feature_names %in% c("group")] # Remove group column name
  ctrl <- caret::trainControl(method = "repeatedcv", repeats = 5) # Specify 5 repeats of 10-fold CV
  set.seed(123)
  message("Performing 5 repeats of 10-fold CV for a linear SVM for each feature. This may take a while depending on the number of features in your dataset.")
  
  for(f in features){
    
    message(paste0("Fitting classifier: ", match(f, features),"/",length(features)))
    
    tmp <- normed %>%
      dplyr::select(c(group, f))
    
    # Fit classifier
    
    m1 <- caret::train(group ~., data = tmp, method = "svmLinear", trControl = ctrl)
    
    # Get outputs and put into dataframe
    
    outs <- caret::confusionMatrix(trainWide$group, predict(m1))
    
    accuracy <- as.data.frame(outs$overall) %>%
      tibble::rownames_to_column(var = "metric") %>%
      dplyr::filter(metric == "Accuracy") %>%
      dplyr::select(c(2)) %>%
      dplyr::pull()
    
    featResults <- data.frame(feature = feature_names[f],
                              accuracy = accuracy)
    
    results[[f]] <- featResults
  }
  
  # Bind feature results together
  
  classificationResults <- data.table::rbindlist(results, use.names = TRUE)
  return(classificationResults)
}
