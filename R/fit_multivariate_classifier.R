#--------------- Helper function ----------------

prepare_model_matrix <- function(data){
  
  # Widening for model matrix
  
  my_matrix <- data %>%
    dplyr::mutate(names_long = paste0(method, "_", names)) %>%
    dplyr::select(-c(names, method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names_long", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    dplyr::mutate(group = as.factor(group))
  
  # Check group variable NAs
  
  nrows <- nrow(my_matrix)
  
  my_matrix <- my_matrix %>%
    dplyr::filter(!is.na(group))
  
  if(nrow(my_matrix) < nrows){
    message(paste0("Dropped ", nrows - nrow(my_matrix), " rows due to NaN values in the 'group' variable column."))
  }
  
  # Delete columns (features) with NaNs and track the number that are deleted
  
  ncols <- ncol(my_matrix)
  
  my_matrix <- my_matrix %>%
    dplyr::select_if(~ !any(is.na(.)))
  
  if(ncol(my_matrix) < ncols){
    message(paste0("Dropped ", ncols - ncol(my_matrix), " features due to NaN values."))
  }
  return(my_matrix)
}

#---------------- Main function ----------------

#' Fit a classifier to feature matrix using all features or by set
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom e1071 svm
#' @importFrom data.table rbindlist
#' @importFrom stats glm binomial
#' @importFrom stats sd
#' @param data the dataframe containing the raw feature matrix
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to "id"
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to "group"
#' @param is_normalised a Boolean as to whether the input feature values have already been scaled. Defaults to FALSE
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to FALSE
#' @param test_method the algorithm to use for quantifying class separation
#' @return an object of class dataframe containing results
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
#' fit_multivariate_classifier(featMat,
#'   id_var = "id",
#'   group_var = "group",
#'   is_normalised = FALSE,
#'   by_set = FALSE,
#'   test_method = "linear svm") 
#' }
#' 

fit_multivariate_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE, is_normalised = FALSE,
                                        test_method = c("linear svm", "rbf svm")){
  
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
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid' or 'MinMax'")
  }
  
  # Set defaults for classification method
  
  methods <- c("linear svm", "rbf svm")
  
  if(test_method %ni% methods){
    stop("test_method should be a single string specification of 't-test', 'binomial logistic', 'linear svm', or 'rbf svm'.")
  }
  
  if(length(test_method) != 1){
    stop("test_method should be a single string specification of 't-test', 'binomial logistic', 'linear svm', or 'rbf svm'.")
  }
  
  #------------- Renaming columns -------------
  
  if (is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var))
  }
  
  num_classes <- length(unique(data_id$group)) # Get number of classes in the data
  
  if(num_classes == 1){
    stop("Your data only has one class label. At least two are required to performed analysis.")
  }
  
  if((missing(test_method) || is.null(test_method)) && num_classes > 1){
    test_method <- "linear svm"
    message("test_method is missing. Running linear svm as a default.")
  }
  
  #------------- Normalise data -------------------
  
  if(is_normalised){
    normed <- data_id
  } else{
    
    normed <- data_id %>%
      tidyr::drop_na() %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = (values - mean(values, na.rm = TRUE)) / stats::sd(values, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    if(nrow(normed) != nrow(data_id)){
      message("Filtered out rows containing NaNs.")
    }
  }
  
  #------------- Preprocess data --------------
  
  if(by_set){
    
    sets <- unique(normed$method)
    
    for(s in sets){
      
      #------------- Preprocess data -------------
      
      setData <- normed %>%
        dplyr::filter(method == s)
      
      inputData <- prepare_model_matrix(data = setData)
      
      #------------- Fit classifiers -------------
      
    }
    
  } else{
    
    #------------- Preprocess data -------------
    
    inputData <- prepare_model_matrix(data = normed)
    
    #------------- Fit classifiers -------------
    
    #---------------
    # Main procedure
    #---------------
    
    # Fit classifier
    
    if(test_method == "linear svm"){
      mod <- e1071::svm(group ~., data = inputData, kernel = "linear", cross = 10, probability = TRUE)
    } else{
      mod <- e1071::svm(group ~., data = inputData, kernel = "radial", cross = 10, probability = TRUE)
    }
    
    # Get outputs for main model
    
    cm <- as.data.frame(table(inputData$group, predict(mod))) %>%
      dplyr::mutate(flag = ifelse(Var1 == Var2, "Same", "Different"))
    
    same_total <- cm %>%
      dplyr::filter(flag == "Same") %>%
      summarise(Freq = sum(Freq)) %>%
      pull()
    
    all_total <- cm %>%
      summarise(Freq = sum(Freq)) %>%
      pull()
    
    statistic <- same_total / all_total
    statistic_name <- "Classification accuracy"
    
    #---------------
    # Empirical null
    #---------------
    
    # Generate shuffled class labels
    
    nullList <- list()
    repeats <- seq(from = 100, to = 1000, by = 100)
    
    for(r in repeats){
      
      set.seed(r)
      y <- inputData %>% dplyr::pull(1)
      y <- as.character(y)
      shuffles <- sample(y, replace = FALSE)
      
      inputData2 <- inputData %>%
        dplyr::mutate(group = shuffles,
                      group = as.factor(group))
      
      # Fit classifier
      
      if(test_method == "linear svm"){
        mod <- e1071::svm(group ~., data = inputData2, kernel = "linear", cross = 10, probability = TRUE)
      } else{
        mod <- e1071::svm(group ~., data = inputData2, kernel = "radial", cross = 10, probability = TRUE)
      }
      
      # Get outputs
      
      cmNULL <- as.data.frame(table(inputData2$group, predict(modNULL))) %>%
        dplyr::mutate(flag = ifelse(Var1 == Var2, "Same", "Different"))
      
      same_totalNULL <- cmNULL %>%
        dplyr::filter(flag == "Same") %>%
        summarise(Freq = sum(Freq)) %>%
        pull()
      
      all_totalNULL <- cmNULL %>%
        summarise(Freq = sum(Freq)) %>%
        pull()
      
      statisticNULL <- same_totalNULL / all_totalNULL
      nullStorage <- data.frame(test_statistic_value = statisticNULL)
      nullList[[r]] <- nullStorage
    }
    
    # Bind empirical nulls together
    
    tmpNULLs <- data.table::rbindlist(nullList, use.names = TRUE) %>%
      dplyr::pull(1)
  }
}
