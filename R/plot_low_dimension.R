#' Produce a principal components analysis (PCA) on normalised feature values and render a bivariate plot to visualise it
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr drop_na
#' @importFrom broom augment
#' @importFrom broom tidy
#' @importFrom stats prcomp
#' @import Rtsne
#' @param data a dataframe with at least 2 columns called \code{"names"} and \code{"values"}
#' @param is_normalised a Boolean as to whether the input feature values have already been scaled. Defaults to \code{FALSE}
#' @param id_var a string specifying the ID variable to uniquely identify each time series. Defaults to \code{"id"}
#' @param group_var a string specifying the grouping variable that the data aggregates to (if one exists). Defaults to \code{NULL}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"z-score"}
#' @param low_dim_method the low dimensional embedding method to use. Defaults to \code{"PCA"}
#' @param perplexity the perplexity hyperparameter to use if t-SNE algorithm is selected. Defaults to \code{30}
#' @param plot a Boolean as to whether a plot or model fit information should be returned. Defaults to \code{TRUE}
#' @param show_covariance a Boolean as to whether covariance ellipses should be shown on the plot. Defaults to \code{FALSE}
#' @param seed fixed number for R's random number generator to ensure reproducibility
#' @return if \code{plot = TRUE}, returns an object of class \code{ggplot}, if \code{plot = FALSE} returns an object of class dataframe with PCA results
#' @author Trent Henderson
#' @export
#' @examples
#' \donttest{
#' featMat <- calculate_features(data = simData, 
#'   id_var = "id", 
#'   time_var = "timepoint", 
#'   values_var = "values", 
#'   group_var = "process", 
#'   feature_set = "catch22",
#'   seed = 123)
#'
#' plot_low_dimension(featMat, 
#'   is_normalised = FALSE, 
#'   id_var = "id", 
#'   group_var = "group", 
#'   method = "RobustSigmoid", 
#'   low_dim_method = "PCA", 
#'   plot = TRUE,
#'   show_covariance = TRUE,
#'   seed = 123)
#' }
#'

plot_low_dimension <- function(data, is_normalised = FALSE, id_var = "id", group_var = NULL, 
                               method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                               low_dim_method = c("PCA", "t-SNE"), perplexity = 30, 
                               plot = TRUE, show_covariance = FALSE, seed = 123){

  # Set defaults

  if(missing(method)){
    method <- "z-score"
    message("No method specified. Specifying 'z-score' as default")
  } else{
    method <- match.arg(method)
  }
  
  if(missing(low_dim_method)){
    low_dim_method <- "PCA"
    message("No low_dim_method specified. Specifying 'PCA' as default")
  } else{
    low_dim_method <- match.arg(low_dim_method)
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

  if(!is.null(group_var) && !is.character(group_var)){
    stop("group_var should be a string specifying a variable in the input data that identifies an aggregate group each observation relates to.")
  }

  # Method selection

  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")

  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }

  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  # Low dim method selection
  
  the_lowdims <- c("PCA", "t-SNE")
  
  if(low_dim_method %ni% the_lowdims){
    stop("low_dim_method should be a single selection of 'PCA' or 't-SNE'")
  }
  
  if(length(low_dim_method) > 1){
    stop("low_dim_method should be a single selection of 'PCA' or 't-SNE'")
  }
  
  if(low_dim_method == "t-SNE" && !is.numeric(perplexity)){
    stop("perplexity should be an integer number, typically between 2 and 100.")
  }
  
  # Seed
  
  if(is.null(seed) || missing(seed)){
    seed <- 123
    message("No argument supplied to seed, using 123 as default.")
  }

  #------------- Assign ID variable ---------------

  if(is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }

  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var))
  }

  #------------- Normalise data -------------------

  if(is_normalised){
    normed <- data_id
  } else{
    
    normed <- data_id %>%
      dplyr::rename(feature_set = .data$method) %>% # Avoids issues with method arg later
      dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
      tidyr::drop_na() %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(values = normalise_feature_vector(.data$values, method = method)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na() %>%
      dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
      dplyr::select(-c(feature_set))

    if(nrow(normed) != nrow(data_id)){
      message("Filtered out rows containing NaNs.")
    }
  }

  #------------- Perform low dim ----------------------
  
  # Produce matrix
  
  dat_filtered <- normed %>%
    tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
    tibble::column_to_rownames(var = "id") %>%
    tidyr::drop_na()
  
  if(low_dim_method == "PCA"){
    
    # PCA calculation
    
    set.seed(seed)
    
    fits <- dat_filtered %>%
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
    
  } else{
    
    # Check perplexity
    
    if(perplexity >= nrow(dat_filtered)){
      stop("perplexity must be < number of unique IDs.")
    }
    
    # t-SNE calculation
    
    set.seed(seed)
    
    tsneOut <- Rtsne::Rtsne(as.matrix(dat_filtered), perplexity = perplexity, max_iter = 5000, dims = 2,
                            check_duplicates = FALSE)
    
    # Retrieve 2-dimensional embedding and add in unique IDs
    
    id_ref <- dat_filtered %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(.data$id))
    
    fits <- data.frame(.fitted1 = tsneOut$Y[,1],
                       .fitted2 = tsneOut$Y[,2]) %>%
      dplyr::mutate(id = id_ref$id)
  }

  #------------- Output & graphic -----------------

  if(isTRUE(plot)){

    if(!is.null(group_var)){

      # Retrieve groups
      
      if(low_dim_method == "PCA"){
        fits <- fits %>%
          broom::augment(dat_filtered) %>%
          dplyr::rename(id = 1) %>%
          dplyr::mutate(id = as.factor(.data$id)) %>%
          dplyr::rename(.fitted1 = .data$.fittedPC1,
                        .fitted2 = .data$.fittedPC2)
      } else{
        fits <- fits %>%
          dplyr::mutate(id = as.factor(.data$id))
      }
      
      data_id <- as.data.frame(lapply(data_id, unlist)) # Catch weird cases where it's a list...
      
      groups <- data_id %>%
        dplyr::rename(group_id = dplyr::all_of(group_var)) %>%
        dplyr::group_by(.data$id, .data$group_id) %>%
        dplyr::summarise(counter = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(.data$counter)) %>%
        dplyr::mutate(id = as.factor(.data$id))
      
      fits <- fits %>%
        dplyr::inner_join(groups, by = c("id" = "id"))

      # Draw plot
      
      p <- fits %>%
          dplyr::mutate(group_id = as.factor(.data$group_id)) %>%
          ggplot2::ggplot(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2))
      
      if(show_covariance){
        p <- p +
          ggplot2::stat_ellipse(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2, fill = .data$group_id), geom = "polygon", alpha = 0.2) +
          ggplot2::guides(fill = "none") +
          ggplot2::scale_fill_brewer(palette = "Dark2")
      } else{
        
      }

      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5, ggplot2::aes(colour = .data$group_id))
      } else{
        p <- p +
          ggplot2::geom_point(size = 2.25, ggplot2::aes(colour = .data$group_id))
      }
      
      if(low_dim_method == "PCA"){
        p <- p +
          ggplot2::labs(title = "Low dimensional projection of time series",
                        x = paste0("PC 1"," (", eigen_pc1, ")"),
                        y = paste0("PC 2"," (", eigen_pc2, ")"),
                        colour = NULL) +
          ggplot2::scale_colour_brewer(palette = "Dark2") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         legend.position = "bottom")
      } else{
        p <- p +
          ggplot2::labs(title = "Low dimensional projection of time series",
                        x = "Dimension 1",
                        y = "Dimension 2",
                        colour = NULL) +
          ggplot2::scale_colour_brewer(palette = "Dark2") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         legend.position = "bottom")
      }
    }

    if(is.null(group_var)){
      
      if(low_dim_method == "PCA"){
        fits <- fits %>%
          broom::augment(dat_filtered) %>%
          dplyr::rename(id = 1) %>%
          dplyr::mutate(id = as.factor(id)) %>%
          dplyr::rename(.fitted1 = .data$.fittedPC1,
                        .fitted2 = .data$.fittedPC2)
      } else{
        fits <- fits %>%
          dplyr::mutate(id = as.factor(.data$id))
      }

      p <- fits %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$.fitted1, y = .data$.fitted2))

      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5, colour = "black")
      } else{
        p <- p +
          ggplot2::geom_point(size = 2, colour = "black")
      }
      
      if(low_dim_method == "PCA"){
        p <- p +
          ggplot2::labs(title = "Low dimensional projection of time series",
                        x = paste0("PC 1"," (", eigen_pc1, ")"),
                        y = paste0("PC 2"," (", eigen_pc2, ")")) +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      } else{
        p <- p +
          ggplot2::labs(title = "Low dimensional projection of time series",
                        x = "Dimension 1",
                        y = "Dimension 2") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
      }
    }
  } else{
    
    if(low_dim_method == "PCA"){
      p <- fits %>%
        broom::tidy(matrix = "eigenvalues") %>%
        dplyr::select(c(.data$PC, .data$percent))
    } else{
      p <- fits
    }
  }
  return(p)
}
