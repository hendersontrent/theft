#' Project a feature matrix into a low dimensional representation using PCA or t-SNE
#' 
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr drop_na
#' @importFrom broom augment tidy
#' @importFrom stats prcomp
#' @importFrom Rtsne Rtsne
#' @param data the \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param norm_method \code{character} denoting the rescaling/normalising method to apply. Can be one of \code{"z-score"}, \code{"Sigmoid"}, \code{"RobustSigmoid"}, or \code{"MinMax"}. Defaults to \code{"z-score"}
#' @param unit_int \code{Boolean} whether to rescale into unit interval \code{[0,1]} after applying normalisation method. Defaults to \code{FALSE}
#' @param low_dim_method \code{character} specifying the low dimensional embedding method to use. Can be one of \code{"PCA"} or \code{"tSNE"}. Defaults to \code{"PCA"}
#' @param na_removal \code{character} defining the way to deal with NAs produced during feature calculation. Can be one of \code{"feature"} or \code{"sample"}. \code{"feature"} removes all features that produced any NAs in any sample, keeping the number of samples the same. \code{"sample"} omits all samples that produced at least one NA. Defaults to \code{"feature"}
#' @param perplexity \code{integer} denoting the perplexity hyperparameter to use if \code{low_dim_method} is \code{"t-SNE"}. Defaults to \code{10}
#' @param seed \code{integer} to fix R's random number generator to ensure reproducibility. Defaults to \code{123}
#' @param ... arguments to be passed to either \code{stats::prcomp} or \code{Rtsne::Rtsne} depending on whether \code{"low_dim_method"} is \code{"PCA"} or \code{"t-SNE"}
#' @return object of class \code{low_dimension}
#' @author Trent Henderson
#' @export
#' 

reduce_dims <- function(data, norm_method = c("zScore", "Sigmoid", "RobustSigmoid", "MinMax"), unit_int = FALSE,
                        low_dim_method = c("PCA", "tSNE"), na_removal = c("feature","sample"), perplexity = 10, seed = 123, ...){

  stopifnot(inherits(data, "feature_calculations") == TRUE)
  norm_method <- match.arg(norm_method)
  
  if(norm_method == "t-SNE"){
    norm_method <- "tSNE" # Old version from {theft}
    rlang::warn("low_dim_method 't-SNE' was recently deprecated in favour of 'tSNE'.", .frequency = "once", .frequency_id = "reduce_dims")
  }
  
  low_dim_method <- match.arg(low_dim_method)
  na_removal <- match.arg(na_removal)
    
  #------------- Normalise data -------------------

  normed <- data[[1]] %>%
    dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$names) %>%
    dplyr::mutate(values = normalise(.data$values, norm_method = norm_method, unit_int = unit_int)) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
    dplyr::select(-c(feature_set))

  #------------- Perform low dim ----------------------
  
  # Produce matrix
    
  wide_data <- normed %>%
    tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
      tibble::column_to_rownames(var = "id")
  
  # Filter data
  
  if(na_removal == "feature"){
    wide_data <- wide_data %>%
      dplyr::select(where(~!any(is.na(.))))
  } else{
    wide_data <- wide_data %>%
      tidyr::drop_na()
  }

  # Report omitted features/samples

    n_features <- length(unique(normed$names))
    n_samples <- length(unique(normed$id))

    n_features_after <- ncol(wide_data)
    n_samples_after <- nrow(wide_data)

    n_features_omitted <- n_features - n_features_after
    n_samples_omitted <- n_samples - n_samples_after

    if (n_features_omitted > 0) {message(paste(n_features_omitted, "features omitted due to NAs", sep = " "))}

    if (n_samples_omitted > 0) {message(paste(n_samples_omitted, "samples omitted due to NAs", sep = " "))}
    
  # Perform dimension reduction
    
  if(low_dim_method == "PCA"){
    
    # PCA calculation
    
    set.seed(seed)
    
    fits <- wide_data %>%
      stats::prcomp(center = FALSE, scale. = FALSE, ...)
    
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
    
    fits <- Rtsne::Rtsne(as.matrix(wide_data), perplexity = perplexity, dims = 2,
                         check_duplicates = FALSE, ...)
    
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
