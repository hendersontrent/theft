#' Produce a correlation matrix plot showing pairwise correlations of time series with automatic hierarchical clustering
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_wider
#' @importFrom reshape2 melt
#' @importFrom stats hclust
#' @importFrom stats dist
#' @importFrom stats cor
#' @importFrom plotly ggplotly config layout
#' @param data a dataframewith at least 2 columns for \code{"id"} and \code{"values"} variables
#' @param is_normalised a Boolean as to whether the input feature values have already been scaled. Defaults to \code{FALSE}
#' @param id_var a string specifying the ID variable to compute pairwise correlations between. Defaults to \code{"id"}
#' @param time_var a string specifying the time index variable. Defaults to \code{NULL}
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values. Defaults to \code{"values"}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"RobustSigmoid"}
#' @param cor_method the correlation method to use. Defaults to \code{"pearson"}
#' @param clust_method the hierarchical clustering method to use for the pairwise correlation plot. Defaults to \code{"average"}
#' @param interactive a Boolean as to whether to plot an interactive \code{plotly} graphic. Defaults to \code{FALSE}
#' @return an object of class \code{ggplot}
#' @author Trent Henderson
#' @export
#' @examples
#' plot_ts_correlations(data = simData, 
#'   is_normalised = FALSE, 
#'   id_var = "id", 
#'   time_var = "timepoint",
#'   values_var = "values",
#'   method = "RobustSigmoid",
#'   cor_method = "pearson",
#'   clust_method = "average",
#'   interactive = FALSE)
#'

plot_ts_correlations <- function(data, is_normalised = FALSE, id_var = "id", 
                                 time_var = "timepoint", values_var = "values",
                                 method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                                 clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid"),
                                 cor_method = c("pearson", "spearman"),
                                 interactive = FALSE){
  
  # Make RobustSigmoid and pearson the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }
  
  if(missing(cor_method)){
    cor_method <- "pearson"
  } else{
    cor_method <- match.arg(cor_method)
  }
  
  #------------ Checks ---------------
  
  if(is.null(id_var) || is.null(time_var) || is.null(values_var)){
    stop("An id variable, time variable, and values variable from your dataframe must be specified.")
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
  
  # Correlation method selection
  
  the_cor_methods <- c("pearson", "spearman")
  
  if(cor_method %ni% the_cor_methods){
    stop("cor_method should be a single selection of 'pearson' or 'spearman'")
  }
  
  if(length(cor_method) > 1){
    stop("cor_method should be a single selection of 'pearson' or 'spearman'")
  }
  
  # Clustering method selection
  
  the_clust_methods <-c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid")
  
  if(clust_method %ni% the_clust_methods){
    stop("clust_method should be a single selection of 'average', 'ward.D', 'ward.D2', 'single', 'complete', 'mcquitty', 'median', or 'centroid'.")
  }
  
  if(length(clust_method) > 1){
    stop("clust_method should be a single selection of 'average', 'ward.D', 'ward.D2', 'single', 'complete', 'mcquitty', 'median', or 'centroid'.")
  }
  
  if(missing(clust_method) || is.null(clust_method)){
    clust_method <- "average"
    message("No argument supplied to clust_method Using 'average' as default.")
  }
  
  # Dataframe length checks and tidy format wrangling
  
  data_re <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  timepoint = dplyr::all_of(time_var),
                  values = dplyr::all_of(values_var))
  
  #------------- Normalise data -------------------
  
  if(is_normalised){
    normed <- data_re
  } else{
    
    normed <- data_re %>%
      dplyr::select(c(.data$id, .data$timepoint, .data$values)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(values = normalise_feature_vector(.data$values, method = method)) %>%
      tidyr::drop_na()
    
    if(nrow(normed) != nrow(data_re)){
      message("Filtered out rows containing NaNs.")
    }
  }
  
  #------------- Data reshaping -------------
  
  cor_dat <- normed %>%
    tidyr::pivot_wider(id_cols = "timepoint", names_from = "id", values_from = "values") %>%
    dplyr::select(-c(.data$timepoint)) %>%
    tidyr::drop_na()
  
  #--------- Correlation ----------
  
  # Calculate correlations and take absolute
  
  result <- abs(stats::cor(cor_dat, method = cor_method))
  
  #--------- Clustering -----------
  
  # Wrangle into tidy format
  
  melted <- reshape2::melt(result)
  
  # Perform clustering
  
  row.order <- stats::hclust(stats::dist(result, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
  col.order <- stats::hclust(stats::dist(t(result), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
  dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
  cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe
  
  #--------- Graphic --------------
  
  # Define a nice colour palette consistent with RColorBrewer in other functions
  
  mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
  
  if(interactive){
    p <- cluster_out %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2,
                                   text = paste('<br><b>ID 1:</b>', .data$Var1,
                                                '<br><b>ID 2:</b>', .data$Var2,
                                                '<br><b>Absolute correlation:</b>', round(.data$value, digits = 3))))
  } else{
    p <- cluster_out %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2)) 
  }
  
  p <- p +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value)) +
    ggplot2::labs(title = "Pairwise correlation matrix",
                  x = "Time series",
                  y = "Time series",
                  fill = "Correlation coefficient") +
    ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                               show.limits = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom")
  
  if(nrow(cluster_out) <= 20){
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    p <- p +
      ggplot2::theme(axis.text = ggplot2::element_blank())
  }
  
  if(interactive){
    p <- plotly::ggplotly(p, tooltip = c("text")) %>%
      plotly::layout(legend = list(orientation = "h", x = 0, y = -0.3)) %>%
      plotly::config(displayModeBar = FALSE)
  } else{
    
  }
  
  return(p)
}
