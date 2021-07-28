#' Produce a correlation matrix plot showing pairwise correlations of feature vectors by unique id with automatic hierarchical clustering.
#' 
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr pivot_wider
#' @importFrom reshape2 melt
#' @importFrom stats hclust
#' @importFrom stats dist
#' @importFrom stats cor
#' @importFrom plotly ggplotly
#' @importFrom plotly config
#' @param data a dataframe with at least 3 columns for 'id', 'names' and 'values'
#' @param is_normalised a Boolean as to whether the input feature values have already been scaled. Defaults to FALSE
#' @param id_var a string specifying the ID variable to compute pairwise correlations between. Defaults to "id"
#' @param names_var a string denoting the name of the variable/column that holds the feature names. Defaults to "names"
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values. Defaults to "values"
#' @param method a rescaling/normalising method to apply. Defaults to 'RobustSigmoid'
#' @param interactive a Boolean as to whether to plot an interactive plotly graphic. Defaults to FALSE
#' @return an object of class ggplot that contains the correlation matrix graphic
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
#' plot_connectivity_matrix(data = featMat, 
#'   is_normalised = FALSE, 
#'   id_var = "id", 
#'   names_var = "names", 
#'   values_var = "values",
#'   method = "RobustSigmoid",
#'   interactive = FALSE)
#' }
#'

plot_connectivity_matrix <- function(data, is_normalised = FALSE, id_var = "id", 
                                     names_var = "names", values_var = "values",
                                     method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                                     interactive = FALSE){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }
  
  #--------- Checks ---------------
  
  if(is.null(id_var) || is.null(names_var) || is.null(values_var)){
    stop("An id, names (feature name identification), and values variable must all be specified.")
  }
  
  # Method selection
  
  '%ni%' <- Negate('%in%')
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  # Dataframe length checks and tidy format wrangling
  
  data_re <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  names = dplyr::all_of(names_var),
                  values = dplyr::all_of(values_var))
  
  #------------- Normalise data -------------------
  
  if(is_normalised){
    normed <- data_re
  } else{
    normed <- data_re %>%
      dplyr::select(c(id, names, values)) %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = normalise_feature_vector(values, method = method)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    if(nrow(normed) != nrow(data_re)){
      message("Filtered out rows containing NaNs.")
    }
  }
  
  #------------- Data reshaping -------------------
  
  features <- unique(normed$names)
  
  ids_to_keep <- normed %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(counter == length(features))
  
  ids_to_keep <- ids_to_keep$id
  
  cor_dat <- normed %>%
    dplyr::filter(id %in% ids_to_keep) %>%
    tidyr::pivot_wider(id_cols = names, names_from = id, values_from = values) %>%
    dplyr::select(-c(names))
  
  #--------- Correlation ----------
  
  result <- stats::cor(cor_dat)
  
  #--------- Clustering -----------
  
  # Wrangle into tidy format
  
  melted <- reshape2::melt(result)
  
  # Perform clustering
  
  row.order <- stats::hclust(stats::dist(result))$order # Hierarchical cluster on rows
  col.order <- stats::hclust(stats::dist(t(result)))$order # Hierarchical cluster on columns
  dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
  cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe
  
  #--------- Graphic --------------
  
  if(interactive){
    p <- cluster_out %>%
      ggplot2::ggplot(ggplot2::aes(x = Var1, y = Var2,
                                   text = paste('<br><b>ID 1:</b>', Var1,
                                                '<br><b>ID 2:</b>', Var2,
                                                '<br><b>Correlation:</b>', round(value, digits = 3))))
  } else{
    p <- cluster_out %>%
      ggplot2::ggplot(ggplot2::aes(x = Var1, y = Var2)) 
  }
  
  p <- p +
    ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::labs(title = "Pairwise correlation matrix",
                  x = NULL,
                  y = NULL,
                  fill = "Correlation Coefficient") +
    ggplot2::scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom")
  
  if(nrow(cluster_out) <= 20){
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  } else {
    p <- p +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  }
  
  if(interactive){
    p <- plotly::ggplotly(p, tooltip = c("text")) %>%
      plotly::config(displayModeBar = FALSE)
  } else{
    
  }
  
  return(p)
}
