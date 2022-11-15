#' All features available in theft in tidy format
#'
#' The variables include:
#'
#' @format A tidy dataframe with 2 variables:
#' \describe{
#'   \item{feature_set}{Name of the set the feature is from}
#'   \item{feature}{Name of the feature}
#' }
#'
"feature_list"


#' Sample of randomly-generated time series to produce function tests and vignettes
#'
#' The variables include:
#'
#' @format A tidy dataframe with 4 variables:
#' \describe{
#'   \item{id}{Unique identifier for the time series}
#'   \item{timepoint}{Time index}
#'   \item{values}{Value}
#'   \item{process}{Group label for the type of time series}
#' }
#'
"simData"


#' Computed values for top features results for use in vignette
#'
#' Format is:
#'
#' @format A list object
#' \describe{
#'   \item{ResultsTable}{data.frame of results for top features}
#'   \item{FeatureFeatureCorrelationPlot}{ggplot heatmap of feature-feature correlation matrix}
#'   \item{ViolinPlots}{ggplot of distributions rendered as violins for top features}
#' }
#'
"demo_outputs"


#' Computed values for multi-feature classification results for use in vignette
#'
#' Format is:
#'
#' @format A list object
#' \describe{
#'   \item{FeatureSetResultsPlot}{ggplot comparing feature set classification accuracy}
#'   \item{TestStatistics}{data.frame of test statistics}
#'   \item{RawClassificationResults}{data.frame of raw classification accuracy}
#' }
#'
"demo_multi_outputs"
