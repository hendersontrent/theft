#' Return an object containing results from top-performing features on a classification task
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param normalise a Boolean of whether to normalise features before plotting. Defaults to FALSE
#' @param method a rescaling/normalising method to apply if normalise = TRUE. Defaults to 'RobustSigmoid'
#' @return an object of class list containing a dataframe of results, a feature x feature matrix plot, and a violin plot
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
#' compute_top_features(featMat,
#'   id_var = "id",
#'   names_var = "names",
#'   group_var = "group",
#'   values_var = "values",
#'   num_features = 10,
#'   normalise = FALSE,
#'   method = "RobustSigmoid") 
#' }
#' 

compute_top_features <- function(data, id_var = "id", names_var = "names", group_var = "group",
                                 values_var = "values", num_features = 10, normalise = FALSE,
                                 method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }
  
  # Check other arguments
  
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
  
  if(!is.null(group_var) && !is.character(group_var)){
    stop("group_var should be a string specifying a variable in the input data that identifies an aggregate group each observation relates to.")
  }
  
  if(!is.null(features) && !is.character(features)){
    stop("features should be a string or vector of string specifying exact feature names to filter by. If you want all features, write 'all'. This is the default.")
  }
  
  # Normalisation
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  
  if(normalise == TRUE && method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(normalise == TRUE && length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  # Default feature number
  
  if(!is.numeric(num_features)){
    stop("num_features should be a positive integer >= 2 specifying the number of features to produce analysis for.")
  }
  
  if(num_features < 2){
    stop("num_features should be a positive integer >= 2 specifying the number of features to produce analysis for.")
  }
  
  if(is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    names = dplyr::all_of(names_var),
                    group = dplyr::all_of(group_var))
  }
  
  if(num_features > length(unique(data_id$names))){
    stop("num_features should be less than or equal to the number of unique features in your data.")
  }
  
  #---------------  Computations ----------------
  
  #---------------
  # Classification
  #---------------
  
  classifierOutputs <- fit_feature_classifier(featMat)
  
  ResultsTable <- classifierOutputs %>%
    dplyr::top_n(accuracy, num_features)
  
  # Filter original data to just the top performers
  
  featMatFiltered <- featMat %>%
    dplyr::filter(names %in% c(ResultsTable$names))
  
  #---------------
  # Feature x 
  # feature plot
  #---------------
  
  FeatureFeatureCorrelations <- plot_correlation_matrix(featMatFiltered, 
                                                        is_normalised = FALSE, 
                                                        id_var = "id", 
                                                        values_var = "values",
                                                        method = method,
                                                        interactive = FALSE)
  
  #---------------
  # Violin plot
  #---------------
  
  DiscriminationPlots <- plot_feature_discrimination(featMatFiltered, 
                                                     id_var = id_var, 
                                                     group_var = group_var,
                                                     normalise = normalise,
                                                     method = method)
  
  #---------------  Returns ---------------------
  
  # Compile into one object and return
  
  myList <- list(ResultsTable, FeatureFeatureCorrelations, DiscriminationPlots)
  return(myList)
}
