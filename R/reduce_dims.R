#' Project a feature matrix into a low dimensional representation using PCA or t-SNE
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr drop_na
#' @importFrom broom augment
#' @importFrom broom tidy
#' @importFrom stats prcomp
#' @import Rtsne
#' @param data the \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"z-score"}
#' @param low_dim_method the low dimensional embedding method to use. Defaults to \code{"PCA"}
#' @param perplexity the perplexity hyperparameter to use if t-SNE algorithm is selected. Defaults to \code{30}
#' @param seed fixed number for R's random number generator to ensure reproducibility
#' @return object of class \code{low_dimension}
#' @author Trent Henderson
#' @export
#' 

reduce_dims <- function(data, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                        low_dim_method = c("PCA", "t-SNE"), perplexity = 30, seed = 123){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  method <- match.arg(method)
  low_dim_method <- match.arg(low_dim_method)

  #------------- Normalise data -------------------

  normed <- data[[1]] %>%
    dplyr::rename(feature_set = .data$method) %>% # Avoids issues with method arg later
    dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normalise(.data$values, method = method)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
    dplyr::select(-c(feature_set))

  #------------- Perform low dim ----------------------
  
  # Produce matrix
  
  wide_data <- normed %>%
    tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
    tibble::column_to_rownames(var = "id") %>%
    tidyr::drop_na()
  
  if(low_dim_method == "PCA"){
    
    # PCA calculation
    
    set.seed(seed)
    
    fits <- wide_data %>%
      stats::prcomp(scale = FALSE)
    
    # Retrieve eigenvalues and tidy up variance explained for plotting
    
    eigens <- fits %>%
      broom::tidy(matrix = "eigenvalues") %>%
      dplyr::filter(.data$PC %in% c(1, 2)) %>% # Filter to just the 2 going in the plot
      dplyr::select(c(.data$PC, .data$percent)) %>%
      dplyr::mutate(percent = round(.data$percent * 100), digits = 1)
    
    eigen_pc1 <- eigens %>%
      dplyr::filter(.data$PC == 1)
    
    eigen_pc2 <- eigens %>%
      dplyr::filter(.data$PC == 2)
    
    eigen_pc1 <- paste0(eigen_pc1$percent,"%")
    eigen_pc2 <- paste0(eigen_pc2$percent,"%")
    
  } else {
    
    # t-SNE calculation
    
    set.seed(seed)
    
    fits <- Rtsne::Rtsne(as.matrix(wide_data), perplexity = perplexity, max_iter = 5000, dims = 2,
                         check_duplicates = FALSE)
    
    # Retrieve 2-dimensional embedding and add in unique IDs
    
    id_ref <- wide_data %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))
    
    fits <- data.frame(.fitted1 = fits$Y[,1],
                       .fitted2 = fits$Y[,2]) %>%
      dplyr::mutate(id = id_ref$id)
  }
  
  low_dim <- list(data[[1]], wide_data, fits)
  low_dim <- structure(low_dim, class = "low_dimension")
  return(low_dim)
}
