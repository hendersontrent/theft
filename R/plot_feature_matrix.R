#' Produce a heatmap matrix of the calculated feature value vectors and each unique time series with automatic hierarchical clustering.
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr drop_na
#' @importFrom reshape2 melt
#' @param data a dataframe with at least 2 columns called 'names' and 'values'
#' @param is_normalised a Boolean as to whether the input feature values have already been scaled. Defaults to FALSE
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to NULL
#' @param method a rescaling/normalising method to apply. Defaults to 'RobustSigmoid'
#' @return an object of class ggplot that contains the heatmap graphic
#' @author Trent Henderson
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' d <- tsibbledata::aus_retail %>%
#'   filter(State == "New South Wales")
#' outs <- calculate_features(data = d, id_var = "Industry", time_var = "Month", values_var = "Turnover", feature_set = "all")
#' plot_feature_matrix(outs, is_normalised = FALSE, id_var = "Industry", method = "RobustSigmoid")
#' }
#'

plot_feature_matrix <- function(data, is_normalised = FALSE, id_var = NULL, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")){

  # Make RobustSigmoid the default

  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }

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

  if(!is.null(id_var) & !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }

  # Method selection

  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")

  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }

  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }

  #------------- Assign ID variable ---------------

  if (nrow(data) <= 22){
    stop("Not enough data to compute feature matrix. Need multiple samples per feature.")
  }

  if (is.null(id_var) & nrow(data) > 22){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }

  if(!is.null(id_var) & nrow(data) > 22){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var))
  }

  #------------- Normalise data -------------------

  if(is_normalised){
    normed <- data_id
  } else if (is_normalised == FALSE & nrow(data_id) == 22){
    message("Not enough data to standardise feature vectors. Using raw calculated values.")
    normed <- data_id
  }else{
    normed <- data_id %>%
      dplyr::select(c(id, names, values)) %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = normalise_catch(values, method = method)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()

    if(nrow(normed) != nrow(data_id)){
      message("Filtered out rows containing NaNs.")
    }
  }

  #------------- Hierarchical clustering ----------

  dat <- normed %>%
    tidyr::pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
    tibble::column_to_rownames(var = "id")

  dat_filtered <- dat %>%
    tidyr::drop_na()

  if(nrow(dat_filtered) != nrow(dat)){
    message("Dropped rows with NAs to enable clustering.")
  }

  row.order <- hclust(dist(dat_filtered))$order # Hierarchical cluster on rows
  col.order <- hclust(dist(t(dat_filtered)))$order # Hierarchical cluster on columns
  dat_new <- dat_filtered[row.order, col.order] # Re-order matrix by cluster outputs
  cluster_out <- reshape2::melt(as.matrix(dat_new)) %>% # Turn into dataframe
    dplyr::rename(id = Var1,
                  names = Var2)

  #------------- Draw graphic ---------------------

  message("Rendering graphic...")

  p <- cluster_out %>%
    ggplot2::ggplot(ggplot2::aes(x = names, y = id, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::labs(title = "Heatmap of hierarchically-clustered scaled features and individual time series",
                  x = "Feature",
                  y = "Time Series",
                  fill = paste0(method," scaled feature value")) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_distiller(palette = "RdYlBu") +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                   panel.grid = ggplot2::element_blank())

  return(p)
}
