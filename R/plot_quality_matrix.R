#' Produce a matrix visualisation of data types computed by feature calculation function.
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom stats reorder
#' @param data a dataframe with at least 2 columns called \code{"names"} and \code{"values"}
#' @return an object of class \code{ggplot}
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
#' plot_quality_matrix(data = featMat)
#' }
#'

plot_quality_matrix <- function(data){

  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')

  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least two columns called 'names' and 'values'. These are automatically produced by feature calculations such as catch_all(). Please consider running one of these first and then passing the resultant dataframe in to this function.")
  }

  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least two columns called 'names' and 'values'. These are automatically produced by feature calculations such as catch_all(). Please consider running one of these first and then passing the resultant dataframe in to this function.")
  }

  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }

  #--------------- Calculate proportions ------------

  tmp <- data %>%
    dplyr::mutate(quality = dplyr::case_when(
                  is.na(values)                         ~ "NaN",
                  is.nan(values)                        ~ "NaN",
                  is.infinite(values)                   ~ "-Inf or Inf",
                  is.numeric(values) & !is.na(values) &
                    !is.na(values) & !is.nan(values)    ~ "Good")) %>%
    dplyr::group_by(names, quality) %>%
    dplyr::summarise(counter = dplyr::n()) %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(props = counter / sum(counter)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(quality = factor(quality, levels = c("-Inf or Inf", "NaN", "Good")))

  # Calculate order of 'good' quality feature vectors to visually steer plot

  ordering <- tmp %>%
    dplyr::filter(quality == "Good") %>%
    dplyr::mutate(ranker = dplyr::dense_rank(props)) %>%
    dplyr::select(c(names, ranker))

  # Join back in

  tmp1 <- tmp %>%
    dplyr::left_join(ordering, by = c("names" = "names"))

  #--------------- Draw plot ------------------------

  # Define a nice colour palette consistent with RColorBrewer in other functions

  my_palette <- c("-Inf or Inf" = "#7570B3",
                  "NaN" = "#D95F02",
                  "Good" = "#1B9E77")

  # Plot

  p <- tmp1 %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(names, -ranker), y = props)) +
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = quality)) +
    ggplot2::labs(title = "Data quality for computed features",
                  x = "Feature",
                  y = "Proportion of Outputs",
                  fill = "Data Type") +
    ggplot2::scale_y_continuous(limits = c(0,1),
                                breaks = seq(from = 0, to = 1, by = 0.1)) +
    ggplot2::scale_fill_manual(values = my_palette) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  return(p)
}
