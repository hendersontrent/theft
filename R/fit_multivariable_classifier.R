#--------------- Helper functions ----------------

#----------------
# Random shuffles
#----------------

calculate_accuracy <- function(x, seed){

  set.seed(seed)

  # Randomly shuffle class labels and compute accuracy

  y <- sample(x, replace = FALSE)
  acc <- sum(x == y, na.rm = TRUE) / length(x)
  return(acc)
}

simulate_null_acc <- function(x, num_permutations = 10000){

  # Run function over num_permutations

  outs <- 1:num_permutations %>%
    purrr::map(~ calculate_accuracy(x, seed = .x)) %>%
    unlist()

  return(outs)
}

#--------------
# Model fitting
#--------------

# Function for returning accuracies over the train procedure

extract_prediction_accuracy <- function(mod, balanced_accuracy = FALSE) {
  results <- as.data.frame(mod$results)
  if (balanced_accuracy) {
    results %<>%
      dplyr::select(c(Accuracy, Accuracy)) %>%
      dplyr::rename(statistic = Accuracy,
                    statistic_sd = AccuracySD)
  } else {
    results %<>%
      dplyr::select(c(Balanced_Accuracy, Balanced_AccuracySD)) %>%
      dplyr::rename(statistic = Balanced_Accuracy,
                    statistic_sd = Balanced_AccuracySD)
  }

  results %<>%
    dplyr::slice_max(statistic, n = 1) # Catches cases where multiple results are returned by {caret} in `mod`

  return(results)
}

# Function for iterating over random shuffle permutations of class labels

fit_empirical_null_models <- function(data, s, test_method, theControl, pb = NULL, univariate = FALSE){

  # Print {purrr} iteration progress updates in the console

  if(!is.null(pb)){
    pb$tick()$print()
  } else{
  }

  # Null shuffles and computations

  y <- data %>% dplyr::pull(group)
  y <- as.character(y)

  set.seed(s)
  shuffles <- sample(y, replace = FALSE)

  shuffledtest <- data %>%
    dplyr::mutate(group = shuffles) %>%
    dplyr::mutate(group = as.factor(group))

  if(univariate){
    processes <- c("center", "scale")
  } else{
    processes <- c("center", "scale", "nzv")
  }

  modNull <- caret::train(group ~ .,
                          data = shuffledtest,
                          method = test_method,
                          trControl = theControl,
                          preProcess = processes)

  if(theControl$method == "none"){

    null_models <- as.data.frame(caret::confusionMatrix(shuffledtest$group, predict(modNull, newdata = shuffledtest))$overall) %>%
      dplyr::mutate(category = "Null") %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::rename(statistic = 1)

  } else{
    null_models <- extract_prediction_accuracy(mod = modNull)
  }

  return(null_models)
}

#--------------
# Calculate balanced accuracy in caret
#--------------

calculate_balanced_accuracy <- function(data, lev = NULL, model = NULL) {
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The twoClassSummary() function isn't appropriate."))
  }
  sens <- sensitivity(data$pred, data$obs)
  spec <- specificity(data$pred, data$obs)
  balanced_accuracy <- (sens + spec)/2
  names(balanced_accuracy) <- "Balanced_Accuracy"
  return(balanced_accuracy)
}

#--------------
# Model fitting
#--------------

fit_multivariable_models <- function(data, test_method, use_balanced_accuracy, use_k_fold, num_folds, use_empirical_null, null_testing_method, num_permutations, set = NULL){

  # Set up input matrices

  if(!is.null(set)){

    message(paste0("\nCalculating models for ", set))

    tmp <- data %>%
      dplyr::filter(method == set) %>%
      dplyr::select(-c(method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(id))

  } else{

    tmp <- data %>%
      dplyr::select(-c(method)) %>%
      tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
      dplyr::select(-c(id))
  }

  # Fit models

  set.seed(123)

  if(use_k_fold){

    # Train model
    if (use_balanced_accuracy) {
      fitControl <- caret::trainControl(method = "cv",
                                        number = num_folds,
                                        summaryFunction = calculate_balanced_accuracy,
                                        classProbs = TRUE)
    } else {
      fitControl <- caret::trainControl(method = "cv",
                                      number = num_folds,
                                      classProbs = TRUE)
    }


    mod <- caret::train(group ~ .,
                        data = tmp,
                        method = test_method,
                        trControl = fitControl,
                        preProcess = c("center", "scale", "nzv"))

    # Get main predictions

    mainOuts <- extract_prediction_accuracy(mod = mod) %>%
      dplyr::mutate(category = "Main")

  } else{

    if (use_balanced_accuracy) {
      fitControl <- caret::trainControl(method = "none",
                                        summaryFunction = calculate_balanced_accuracy,
                                        classProbs = TRUE)
    } else {
      fitControl <- caret::trainControl(method = "none",
                                        classProbs = TRUE)
    }

    mod <- caret::train(group ~ .,
                        data = tmp,
                        method = test_method,
                        trControl = fitControl,
                        preProcess = c("center", "scale", "nzv"))

    # Get main predictions

    mainOuts <- as.data.frame(caret::confusionMatrix(tmp$group, predict(mod, newdata = tmp))$overall) %>%
      dplyr::mutate(category = "Main") %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::rename(statistic = 1)
  }

  if(use_empirical_null){

    if(null_testing_method == "null model fits"){

      # Set up progress bar for {purrr::map} iterations

      pb <- dplyr::progress_estimated(length(1:num_permutations))

      # Run procedure

      nullOuts <- 1:num_permutations %>%
        purrr::map( ~ fit_empirical_null_models(data = tmp,
                                                s = .x,
                                                test_method = test_method,
                                                theControl = fitControl,
                                                pb = pb,
                                                univariate = FALSE))

      nullOuts <- data.table::rbindlist(nullOuts, use.names = TRUE) %>%
        dplyr::mutate(category = "Null")

      finalOuts <- dplyr::bind_rows(mainOuts, nullOuts)

    } else{
      finalOuts <- mainOuts
    }

    } else{
      finalOuts <- mainOuts
    }

    if(!is.null(set)){
      finalOuts <- finalOuts %>%
        dplyr::mutate(method = set,
                      num_features_used = (ncol(tmp) - 1))
    }

  return(finalOuts)
}

#--------------------
# p-value calculation
#--------------------

calculate_multivariable_statistics <- function(data, set = NULL, p_value_method){

  # Wrangle vectors

  if(!is.null(set)){
    vals <- data %>%
      dplyr::filter(method %in% c(set, "model free shuffles"))
  } else{
    vals <- data
  }

  true_val <- vals %>%
    dplyr::filter(category == "Main") %>%
    dplyr::pull(statistic)

  stopifnot(length(true_val) == 1)

  nulls <- vals %>%
    dplyr::filter(category == "Null") %>%
    dplyr::pull(statistic)

  if(p_value_method == "empirical"){

    # Use ECDF to calculate p-value

    fn <- stats::ecdf(nulls)
    p_value <- 1 - fn(true_val)

  } else{

    # Calculate p-value from Gaussian with null distribution parameters

    p_value <- stats::pnorm(true_val, mean = mean(nulls), sd = stats::sd(nulls), lower.tail = FALSE)
  }

  tmp_outputs <- data.frame(statistic_value = true_val,
                            p_value = p_value)

  if(!is.null(set)){
    tmp_outputs <- tmp_outputs %>%
      dplyr::mutate(method = set) %>%
      dplyr::select(c(method, statistic_value, p_value))
  }

  return(tmp_outputs)
}


#---------------- Main function ----------------

#' Fit a classifier to feature matrix using all features or all features by set
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na pivot_wider pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom data.table rbindlist
#' @importFrom stats sd reorder ecdf pnorm
#' @importFrom purrr map
#' @importFrom janitor clean_names
#' @importFrom caret preProcess train confusionMatrix
#' @param data the dataframe containing the raw feature data as calculated by \code{theft::calculate_features}
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to \code{"id"}
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to \code{"group"}
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to \code{FALSE}
#' @param test_method the algorithm to use for quantifying class separation. Defaults to \code{"gaussprRadial"}
#' @param use_balanced_accuracy a Boolean specifying whether to use balanced accuracy as the summary metric for caret model training. Defaults to \code{FALSE}
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates. Defaults to \code{TRUE}
#' @param num_folds an integer specifying the number of folds (train-test splits) to perform if \code{use_k_fold} is set to \code{TRUE}. Defaults to \code{10}
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values. Defaults to \code{FALSE}
#' @param null_testing_method a string specifying the type of statistical method to use to calculate p-values. Defaults to \code{model free shuffles}
#' @param p_value_method a string specifying the method of calculating p-values. Defaults to \code{"empirical"}
#' @param num_permutations an integer specifying the number of class label shuffles to perform if \code{use_empirical_null} is \code{TRUE}. Defaults to \code{100}
#' @return an object of class list containing dataframe summaries of the classification models and a \code{ggplot} object if \code{by_set} is \code{TRUE}
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
#' fit_multivariable_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   by_set = FALSE,
#'   test_method = "gaussprRadial",
#'   use_k_fold = TRUE,
#'   num_folds = 10,
#'   use_empirical_null = TRUE,
#'   null_testing_method = "model free shuffles",
#'   p_value_method = "empirical",
#'   num_permutations = 100)
#' }
#'

fit_multivariable_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE, test_method = "gaussprRadial",
                                        use_balanced_accuracy = FALSE, use_k_fold = TRUE, num_folds = 10,
                                        use_empirical_null = FALSE, null_testing_method = c("model free shuffles", "null model fits"),
                                        p_value_method = c("empirical", "gaussian"), num_permutations = 100){

  #---------- Check arguments ------------

  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  expected_cols_3 <- "method"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')

  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by calculate_features(). Please consider running this first and then passing the resultant dataframe in to this function.")
  }

  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by calculate_features(). Please consider running this first and then passing the resultant dataframe in to this function.")
  }

  if(expected_cols_3 %ni% the_cols){
    stop("data should contain at least three columns called 'names', 'values', and 'method'. These are automatically produced by calculate_features(). Please consider running this first and then passing the resultant dataframe in to this function.")
  }

  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }

  if(!is.null(id_var) && !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }

  # Null testing options

  theoptions <- c("model free shuffles", "null model fits")

  if(is.null(null_testing_method) || missing(null_testing_method)){
    null_testing_method <- "model free shuffles"
    message("No argument supplied to null_testing_method. Using 'model free shuffles' as default.")
  }

  if(length(null_testing_method) != 1){
    stop("null_testing_method should be a single string of either 'model free shuffles' or 'null model fits'.")
  }

  if(null_testing_method %ni% theoptions){
    stop("null_testing_method should be a single string of either 'model free shuffles' or 'null model fits'.")
  }

  if(null_testing_method == "model free shuffles" && num_permutations < 1000){
    message("Null testing method 'model free shuffles' is very fast. Consider running more permutations for more reliable results. N = 10000 is recommended.")
  }

  # p-value options

  theoptions_p <- c("empirical", "gaussian")

  if(is.null(p_value_method) || missing(p_value_method)){
    p_value_method <- "empirical"
    message("No argument supplied to p_value_method Using 'empirical' as default.")
  }

  if(length(p_value_method) != 1){
    stop("p_value_method should be a single string of either 'empirical' or 'gaussian'.")
  }

  if(p_value_method %ni% theoptions_p){
    stop("p_value_method should be a single string of either 'empirical' or 'gaussian'.")
  }

  #------------- Renaming columns -------------

  if (is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }

  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(id, group, method, names, values))
  }

  num_classes <- length(unique(data_id$group)) # Get number of classes in the data

  if(num_classes < 2){
    stop("Your data has less than two unique classes. At least two are required to performed classification analysis.")
  }

  # Set defaults for classification method

  if((missing(test_method) || is.null(test_method))){
    test_method <- "gaussprRadial"
    message("test_method is NULL or missing, fitting 'gaussprRadial' by default.")
  }

  if(length(test_method) != 1){
    stop("test_method should be a single string specification of a classification model available in the `caret` package. 'svmLinear' or 'gaussprRadial' are recommended as starting points.")
  }

  # Splits and shuffles

  if(use_k_fold == TRUE && !is.numeric(num_folds)){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }

  if(use_empirical_null == TRUE && !is.numeric(num_permutations)){
    stop("num_permutations should be a postive integer. A minimum of 50 permutations is recommended.")
  }

  if(use_empirical_null == TRUE && num_permutations < 3){
    stop("num_permutations should be a positive integer >= 3 for empirical null calculations. A minimum of 50 permutations is recommended.")
  }

  if(use_k_fold == TRUE && num_folds < 1){
    stop("num_folds should be a positive integer. 10 folds is recommended.")
  }

  #------------- Preprocess data --------------

  # Widening for model matrix

  data_id <- data_id %>%
    dplyr::mutate(names = paste0(method, "_", names)) %>%
    dplyr::select(-c(method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")

  ncols <- ncol(data_id)

  # Delete features that are all NaNs and features with constant values

  data_id <- data_id %>%
    dplyr::select_if(~sum(!is.na(.)) > 0) %>%
    dplyr::select(where(~dplyr::n_distinct(.) > 1))

  if(ncol(data_id) < ncols){
    message(paste0("Dropped ", ncols - ncol(data_id), " features due to containing NAs or only a constant."))
  }

  # Check NAs

  nrows <- nrow(data_id)

  data_id <- data_id %>%
    tidyr::drop_na()

  if(nrow(data_id) < nrows){
    message(paste0("Dropped ", nrows - nrow(data_id), " unique IDs due to NA values."))
  }

  # Clean up column (feature) names so models fit properly (mainly an issue with SVM formula) and re-join set labels
  # and prep factor levels as names for {caret} if the 3 base two-class options aren't being used

  data_id <- data_id %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = 3:ncol(data_id), names_to = "names", values_to = "values") %>%
    dplyr::mutate(method = gsub("_.*", "\\1", names)) %>%
    dplyr::mutate(group = make.names(group),
                  group = as.factor(group))

  #------------- Fit models -------------------

  #---------------------
  # Set up useful method
  # information
  #---------------------

  classifier_name <- test_method
  statistic_name <- "Mean classification accuracy"

  # Very important coffee console message

  if(null_testing_method == "null model fits" & num_permutations > 100){
    message("This will take a while. Great reason to go grab a coffee and relax ^_^")
  }

  if(by_set){

    sets <- unique(data_id$method)

    # Compute accuracies for each feature set

    output <- sets %>%
      purrr::map(~ fit_multivariable_models(data = data_id,
                                           test_method = test_method,
                                           use_balanced_accuracy = use_balanced_accuracy,
                                           use_k_fold = use_k_fold,
                                           num_folds = num_folds,
                                           use_empirical_null = use_empirical_null,
                                           null_testing_method = null_testing_method,
                                           num_permutations = num_permutations,
                                           set = .x))

    output <- data.table::rbindlist(output, use.names = TRUE)

  } else{

    output <- fit_multivariable_models(data = data_id,
                                      test_method = test_method,
                                      use_balanced_accuracy = use_balanced_accuracy,
                                      use_k_fold = use_k_fold,
                                      num_folds = num_folds,
                                      use_empirical_null = use_empirical_null,
                                      null_testing_method = null_testing_method,
                                      num_permutations = num_permutations,
                                      set = NULL)
  }

  # Run nulls if random shuffles are to be used

  if(null_testing_method == "model free shuffles"){

    # Run random shuffles procedure

    nullOuts <- data.frame(statistic = simulate_null_acc(x = data_id$group, num_permutations = num_permutations)) %>%
      dplyr::mutate(statistic_sd = NA,
                    category = "Null",
                    method = "model free shuffles",
                    num_features_used = NA)

    output <- dplyr::bind_rows(output, nullOuts)
  }

  #--------------- Evaluate results ---------------

  if(by_set){

    #---------- Draw bar plot ---------

    # Draw plot

    FeatureSetResultsPlot <- output %>%
      dplyr::filter(category == "Main") %>%
      dplyr::mutate(method = paste0(method, " (", num_features_used, ")")) %>%
      dplyr::mutate(statistic = statistic * 100)

    if(use_k_fold){

      FeatureSetResultsPlot <- FeatureSetResultsPlot %>%
        mutate(statistic_sd = statistic_sd * 100) %>%
        dplyr::mutate(lower = statistic - (2 * statistic_sd),
                      upper = statistic + (2 * statistic_sd)) %>%
        ggplot2::ggplot(ggplot2::aes(x = stats::reorder(method, -statistic))) +
        ggplot2::geom_bar(ggplot2::aes(y = statistic, fill = method), stat = "identity") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), colour = "black") +
        ggplot2::labs(subtitle = "Number of features is indicated in parentheses. Error bars are +- 2 times pointwise SD")

    } else{
      FeatureSetResultsPlot <- FeatureSetResultsPlot %>%
        ggplot2::ggplot(ggplot2::aes(x = stats::reorder(method, -statistic))) +
        ggplot2::geom_bar(ggplot2::aes(y = statistic, fill = method), stat = "identity") +
        ggplot2::labs(subtitle = "Number of features is indicated in parentheses")
    }

    FeatureSetResultsPlot <- FeatureSetResultsPlot +
        ggplot2::labs(title = "Classification accuracy by feature set",
                      y = "Classification accuracy (%)",
                      x = "Feature set",
                      fill = NULL) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(limits = c(0, 100),
                                  breaks = seq(from = 0, to = 100, by = 20),
                                  labels = function(x) paste0(x, "%")) +
      ggplot2::scale_fill_brewer(palette = "Dark2") +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

    #---------- Compute p values ------

    if(use_empirical_null){

      TestStatistics <- sets %>%
        purrr::map(~ calculate_multivariable_statistics(data = output, set = .x, p_value_method = p_value_method))

      TestStatistics <- data.table::rbindlist(TestStatistics, use.names = TRUE) %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)

      output <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)

      myList <- list(FeatureSetResultsPlot, TestStatistics, output)
      names(myList) <- c("FeatureSetResultsPlot", "TestStatistics", "RawClassificationResults")

      } else{

        output <- output %>%
          dplyr::mutate(classifier_name = classifier_name,
                        statistic_name = statistic_name) %>%
          dplyr::select(-c(category))

        myList <- list(FeatureSetResultsPlot, output)
        names(myList) <- c("FeatureSetResultsPlot", "RawClassificationResults")
    }
  } else{

    if(use_empirical_null){

      TestStatistics <- calculate_multivariable_statistics(data = output, set = NULL, p_value_method = p_value_method) %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)

      output <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name)

      myList <- list(TestStatistics, output)
      names(myList) <- c("TestStatistics", "RawClassificationResults")

    } else{

      output <- output %>%
        dplyr::mutate(classifier_name = classifier_name,
                      statistic_name = statistic_name) %>%
        dplyr::select(-c(category))

      myList <- list(output)
      names(myList) <- c("RawClassificationResults")
    }
  }
  return(myList)
}
