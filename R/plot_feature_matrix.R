#' Produce a heatmap matrix of the calculated feature value vectors and each unique time series with automatic hierarchical clustering.
#' @importFrom rlang .data warn
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom reshape2 melt
#' @importFrom stats hclust
#' @importFrom stats dist
#' @importFrom plotly ggplotly config layout
#' @param data a dataframe with at least 2 columns called \code{"names"} and \code{"values"}
#' @param is_normalised a Boolean as to whether the input feature values have already been scaled. Defaults to \code{FALSE}
#' @param id_var a string specifying the ID variable to identify each time series. Defaults to \code{"id"}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"RobustSigmoid"}
#' @param clust_method the hierarchical clustering method to use for the pairwise correlation plot. Defaults to \code{"average"}
#' @param interactive a Boolean as to whether to plot an interactive \code{plotly} graphic. Defaults to \code{FALSE}
#' @return an object of class \code{ggplot} that contains the heatmap graphic
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
#' plot_feature_matrix(featMat, 
#'   is_normalised = FALSE,
#'   id_var = "id", 
#'   method = "RobustSigmoid",
#'   clust_method = "average",
#'   interactive = FALSE)
#'

plot_feature_matrix <- function(data, is_normalised = FALSE, id_var = "id", 
                                method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                                clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid"),
                                interactive = FALSE){
  
  rlang::warn("As of 0.3.6 plot_feature_matrix is deprecated. Please use 'plot_all_features' instead",
              .frequency = "once", .frequency_id = "plot_feature_matrix")
  
  # Set defaults
  
  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }
  
  if(missing(clust_method)){
    clust_method <- "average"
  } else{
    clust_method <- match.arg(clust_method)
  }
  
  if(missing(id_var)){
    id_var <- "id"
    message("No id_var specified. Specifying 'id' as default as returned in theft::calculate_features")
  }

  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  expected_cols_3 <- "method"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')

  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features. Please run this first and then pass the resultant dataframe to this function.")
  }
  
  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features. Please run this first and then pass the resultant dataframe to this function.")
  }
  
  if(expected_cols_3 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by theft::calculate_features. Please run this first and then pass the resultant dataframe to this function.")
  }

  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }

  if(!is.null(id_var) && !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
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

  #------------- Assign ID variable ---------------

  if (is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }

  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var))
  }

  #------------- Apply normalisation -------------
  
  if(is_normalised){
    
  } else{
    
    data_id <- data_id %>%
      dplyr::rename(feature_set = .data$method) %>% # Avoids issues with method arg later
      dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
      dplyr::select(-c(.data$feature_set)) %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(values = normalise_feature_vector(.data$values, method = method)) %>%
      dplyr::ungroup()
    
    message("Applying linear rescaling of values to make plot legend cleaner.")
    
    if(nrow(data_id) < nrow(data)){
      message("Filtered out rows containing NaNs.")
    }
  }

  #------------- Hierarchical clustering ----------

  dat <- data_id %>%
    tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
    tibble::column_to_rownames(var = "id")
  
  # Remove any columns with >50% NAs to prevent masses of rows getting dropped due to poor features
  
  dat_filtered <- dat[, which(colMeans(!is.na(dat)) > 0.5)]
  
  # Drop any remaining rows with NAs

  dat_filtered <- dat_filtered %>%
    tidyr::drop_na()

  if(nrow(dat_filtered) != nrow(dat)){
    message("Dropped rows with NAs to enable clustering.")
  }

  row.order <- stats::hclust(stats::dist(dat_filtered, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
  col.order <- stats::hclust(stats::dist(t(dat_filtered), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
  dat_new <- dat_filtered[row.order, col.order] # Re-order matrix by cluster outputs
  
  cluster_out <- reshape2::melt(as.matrix(dat_new)) %>% # Turn into dataframe
    dplyr::rename(id = .data$Var1,
                  names = .data$Var2)

  #------------- Draw graphic ---------------------
  
  # Define a nice colour palette consistent with RColorBrewer in other functions
  
  mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
  
  if(interactive){
    p <- cluster_out %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$names, y = .data$id, fill = .data$value,
                                     text = paste('<br><b>ID:</b>', .data$id,
                                                  '<br><b>Feature:</b>', .data$names,
                                                  '<br><b>Scaled Value:</b>', round(.data$value, digits = 3)))) +
        ggplot2::geom_raster() +
        ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                                   show.limits = TRUE)
    
  } else{
    p <- cluster_out %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$names, y = .data$id, fill = .data$value))  +
        ggplot2::geom_raster() +
        ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                                   show.limits = TRUE)
  }

  p <- p +
    ggplot2::labs(title = "Data matrix",
                  x = "Feature",
                  y = "Time series") +
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(fill = "Scaled value")
  
  if(length(unique(cluster_out$names)) <= 22){
    p <- p +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     axis.ticks.y = ggplot2::element_blank())
  } else {
    p <- p +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
  }
  
  if(interactive){
    p <- plotly::ggplotly(p, tooltip = c("text")) %>%
      plotly::layout(legend = list(orientation = "h", x = 0, y = -0.3)) %>%
      plotly::config(displayModeBar = FALSE)
  } else{
    p <- p +
      ggplot2::theme(legend.position = "bottom")
  }

  return(p)
}
