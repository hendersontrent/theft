#' All features available in theft in tidy format
#'
#' The variables include:
#'
#' @format A tidy data frame with 2 variables:
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
#' @format A tidy tsibble with 4 variables:
#' \describe{
#'   \item{id}{Unique identifier for the time series}
#'   \item{timepoint}{Time index}
#'   \item{values}{Value}
#'   \item{process}{Group label for the type of time series}
#' }
#'
"simData"
