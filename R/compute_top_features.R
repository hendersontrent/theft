#' Return an object containing results from top-performing features on a classification task
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom stats hclust
#' @importFrom stats dist
#' @importFrom stats cor
#' @importFrom reshape2 melt
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
                                 values_var = "values", num_features = 40, normalise = FALSE,
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
    num_features <- length(unique(data_id$names))
    message(paste0("Number of specified features exceeds number of features in your data. Automatically adjusting to ", num_features))
  }
  
  #---------------  Computations ----------------
  
  #---------------
  # Classification
  #---------------
  
  classifierOutputs <- fit_feature_classifier(data_id, id_var = "id", group_var = "group")
  
  ResultsTable <- classifierOutputs %>%
    dplyr::slice_min(p_value, n = num_features)
  
  # Filter original data to just the top performers
  
  dataFiltered <- data_id %>%
    dplyr::filter(names %in% ResultsTable$feature)
  
  #---------------
  # Feature x 
  # feature plot
  #---------------
  
  # Wrangle dataframe
  
  cor_dat <- dataFiltered %>%
    dplyr::select(c(id, names, values)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(values = normalise_feature_vector(values, method = method)) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
    dplyr::select(-c(id))
  
  # Calculate correlations
  
  result <- stats::cor(cor_dat, method = "pearson")
  
  # Wrangle into tidy format
  
  melted <- reshape2::melt(result)
  
  # Perform clustering
  
  row.order <- stats::hclust(stats::dist(result))$order # Hierarchical cluster on rows
  col.order <- stats::hclust(stats::dist(t(result)))$order # Hierarchical cluster on columns
  dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
  cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe
  
  # Draw plot
  
  FeatureFeatureCorrelationPlots <- cluster_out %>%
    ggplot2::ggplot(ggplot2::aes(x = Var1, y = Var2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::labs(title = "Pairwise correlation matrix of top features",
                  x = NULL,
                  y = NULL,
                  fill = "Pearson correlation coefficient") +
    ggplot2::scale_fill_distiller(palette = "RdBu", limits = c(-1,1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom")
  
  if(length(unique(ResultsTable$feature)) <= 22){
    FeatureFeatureCorrelationPlots <- FeatureFeatureCorrelationPlots +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  } else {
    FeatureFeatureCorrelationPlots <- FeatureFeatureCorrelationPlots +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  }
  
  #---------------
  # Violin plot
  #---------------
  
  ViolinPlots <- plot_feature_discrimination(dataFiltered, 
                                             id_var = "id", 
                                             group_var = "group",
                                             normalise = normalise,
                                             method = method)
  
  #---------------  Returns ---------------------
  
  # Compile into one object and return
  
  myList <- list(ResultsTable, FeatureFeatureCorrelationPlots, ViolinPlots)
  names(myList) <- c("ResultsTable", "FeatureFeatureCorrelationPlots", "ViolinPlots")
  return(myList)
}
