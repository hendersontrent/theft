#' Produce a correlation matrix plot showing pairwise correlations of feature vectors by unique id with automatic hierarchical clustering.
#' @importFrom rlang .data warn
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_wider
#' @importFrom reshape2 melt
#' @importFrom stats hclust
#' @importFrom stats dist
#' @importFrom stats cor
#' @importFrom plotly ggplotly config layout
#' @param data a dataframe with at least 3 columns for \code{'id'}, \code{'names'} and \code{'values'}
#' @param is_normalised deprecated as of 0.4.0; do not use
#' @param id_var a string specifying the ID variable to compute pairwise correlations between. Defaults to \code{"id"}
#' @param names_var a string denoting the name of the variable/column that holds the feature names. Defaults to \code{"names"}
#' @param values_var a string denoting the name of the variable/column that holds the numerical feature values. Defaults to \code{"values"}
#' @param method deprecated as of 0.4.0; do not use
#' @param cor_method the correlation method to use. Defaults to \code{"pearson"}
#' @param clust_method the hierarchical clustering method to use for the pairwise correlation plot. Defaults to \code{"average"}
#' @param interactive a Boolean as to whether to plot an interactive \code{plotly} graphic. Defaults to \code{FALSE}
#' @return an object of class \code{ggplot} that contains the correlation matrix graphic
#' @author Trent Henderson
#' @export
#' @examples
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22",
#'   seed = 123)
#'   
#' plot_feature_correlations(data = featMat, 
#'   id_var = "id", 
#'   names_var = "names", 
#'   values_var = "values",
#'   cor_method = "pearson",
#'   clust_method = "average",
#'   interactive = FALSE)
#'

plot_feature_correlations <- function(data, is_normalised = NULL, id_var = "id", 
                                    names_var = "names", values_var = "values", method = NULL,
                                    cor_method = c("pearson", "spearman"),
                                    clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid"),
                                    interactive = FALSE){
  
  if(!is.null(is_normalised) || !is.null(method)){
    rlang::warn("As of 0.4.0 'is_normalised' and 'method' are no longer arguments to plot_feature_correlations",
                .frequency = "once", .frequency_id = "plot_feature_correlations")
  }
  
  if(missing(cor_method)){
    cor_method <- "pearson"
  } else{
    cor_method <- match.arg(cor_method)
  }
  
  #------------ Checks ---------------
  
  if(is.null(id_var) || is.null(names_var) || is.null(values_var)){
    stop("An id, names (feature name identification), and values variable must all be specified.")
  }
  
  '%ni%' <- Negate('%in%')
  expected_cols_1 <- "method"
  the_cols <- colnames(data)
  
  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least one columns called 'method' containing feature set names. This is automatically produced by theft::calculate_features. Please run this first and then pass the resultant dataframe to this function.")
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
  
  data_id <- data %>%
    dplyr::rename(id = dplyr::all_of(id_var),
                  names = dplyr::all_of(names_var),
                  values = dplyr::all_of(values_var))
  
  #------------- Clean up structure --------------
  
  data_id <- data_id %>%
    dplyr::rename(feature_set = .data$method) %>% # Avoids issues with method arg later
    dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
    dplyr::select(-c(.data$feature_set))
  
  if(nrow(data_id) < nrow(data)){
    message("Filtered out rows containing NaNs.")
  }
  
  #------------- Data reshaping -------------------
  
  features <- unique(data_id$names)
  
  ids_to_keep <- data_id %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$counter == length(features))
  
  ids_to_keep <- ids_to_keep$id
  
  cor_dat <- data_id %>%
    dplyr::filter(.data$id %in% ids_to_keep) %>%
    tidyr::pivot_wider(id_cols = "names", names_from = "id", values_from = "values") %>%
    dplyr::select(-c(.data$names))
  
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
                  x = "Feature",
                  y = "Feature",
                  fill = "Absolute correlation coefficient") +
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
