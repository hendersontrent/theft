#' Produce a correlation matrix plot showing pairwise correlations of feature vectors by unique id with automatic hierarchical clustering.
#' This function is based on functional connectivity matrices in neuroscience.
#' 
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr pivot_wider
#' @importFrom reshape2 melt
#' @importFrom stats hclust
#' @importFrom stats dist
#' @importFrom stats cor
#' @param data a dataframe with at least 3 columns for 'id', 'names' and 'values'
#' @param id_var a string specifying the ID variable to compute pairwise correlations between. Defaults to NULL
#' @param names_var a string denoting the name of the variable/column that holds the feature names
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values
#' @return an object of class ggplot that contains the correlation matrix graphic
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tsibbledata)
#' d <- tsibbledata::aus_retail %>%
#'   filter(State == "New South Wales")
#' outs <- calculate_features(data = d, id_var = "Industry", time_var = "Month", 
#'   values_var = "Turnover", feature_set = "all", tsfresh_cleanup = FALSE)
#' normed <- normalise_feature_frame(outs, names_var = "names", 
#'   values_var = "values", method = "RobustSigmoid")
#' plot_connectivity_matrix(normed, id_var = "Industry")
#' }
#'

plot_connectivity_matrix <- function(data, id_var = NULL, names_var = NULL, values_var = NULL){
  
  message("Operation assumes data has been normalised. If not, normalise feature vectors before plotting connectivity matrix - you can use theft::normalise_feature_frame for this.")
  
  #--------- Checks ---------------
  
  if(is.null(id_var) || is.null(names_var) || is.null(values_var)){
    stop("An id, names (feature name identification), and values variable must all be specified.")
  }
  
  # Dataframe length checks and tidy format wrangling
  
  data_re <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  names = dplyr::all_of(names_var),
                  values = dplyr::all_of(values_var))
  
  features <- unique(data_re$names)
  
  ids_to_keep <- data_re %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(counter == length(features))
  
  ids_to_keep <- ids_to_keep$id
  
  cor_dat <- data_re %>%
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
  
  p <- cluster_out %>%
    ggplot2::ggplot(ggplot2::aes(x = Var1, y = Var2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::labs(title = "Feature value correlations between with hierarchical clustering",
                  x = NULL,
                  y = NULL,
                  fill = "Correlation Coefficient") +
    ggplot2::scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  
  return(p)
}
