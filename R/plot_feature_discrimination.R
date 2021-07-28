#' Produce a matrix of violin plots for each feature to show class discrimination
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param features a string or vector of strings specifying which features to filter by. Defaults to "all"
#' @param normalise a Boolean of whether to normalise features before plotting. Defaults to FALSE
#' @param method a rescaling/normalising method to apply if normalise = TRUE. Defaults to 'RobustSigmoid'
#' @return an object of class ggplot containing the graphics
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
#' plot_feature_discrimination(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   features = "all") 
#' }
#' 

plot_feature_discrimination <- function(data, id_var = "id", group_var = "group", features = "all",
                                        normalise = FALSE,
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
  
  if(normalise == TRUE && method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(normalise == TRUE && length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  #------------- Assign ID variable ---------------
  
  if(is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var))
  }
  
  #------------- Normalise data -------------------
  
  if(normalise){
    normed <- data_id %>%
      dplyr::select(c(id, group, names, values)) %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = normalise_feature_vector(values, method = method)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    if(nrow(normed) != nrow(data_id)){
      message("Filtered out rows containing NaNs.")
    }
  } else{
    normed <- data_id
  }
  
  #------------- Perform filtering ----------------
  
  if(features == "all" && length(features) == 1){
    tmp <- normed
  }
  
  if(features != "all" && length(features) >= 1){
    tmp <- normed %>%
      dplyr::filter(names %in% features)
  }

  #------------- Produce plots --------------------
  
  # Define a nice colour palette
  
  available_colours <- c("#ef6ade", "#75eab6", "#2a6866", "#14bae1", "#ad0599", 
                         "#513886", "#7f73ed", "#e4b8ec", "#0b29d0", "#3986da")
  
  # Draw plot
  
  p <- tmp %>%
    dplyr::mutate(group = as.factor(group)) %>%
    ggplot2::ggplot(ggplot2::aes(x = group, y = values, colour = group)) +
    ggplot2::geom_violin() +
    ggplot2::geom_point(size = 1, alpha = 0.9, position = ggplot2::position_jitter(w = 0.05)) +
    ggplot2::labs(title = "Group discrimination by feature",
                  x = "Group",
                  y = "Value") +
    ggplot2::scale_color_manual(values = available_colours) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank())
  
  if(normalise){
    p <- p +
      ggplot2::facet_wrap(~names, ncol = 4)
  } else{
    p <- p +
      ggplot2::facet_wrap(~names, ncol = 4, scales = "free_y")
  }
  
  return(p)
}
