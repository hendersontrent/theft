#--------------- Helper functions ----------------

#-------------
# Model matrix
#-------------

prepare_model_matrices <- function(mydata, seed){
  
  # Pivot wider for correct train-test splits
  
  mydata2 <- mydata %>%
    dplyr::select(-c(method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values")
  
  # Train-test split
  
  set.seed(seed)
  bound <- floor((nrow(mydata2)/4)*3)
  mydata2 <- mydata2[sample(nrow(mydata2)), ]
  train <- mydata2[1:bound, ]
  test <- mydata2[(bound + 1):nrow(mydata2), ]
  
  # Get train mean and SD for normalisation
  
  train_scales <- train %>%
    tidyr::pivot_longer(cols = 3:ncol(train), names_to = "names", values_to = "values") %>%
    dplyr::group_by(names) %>%
    dplyr::summarise(mean = mean(values, na.rm = TRUE),
                     sd = stats::sd(values, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # Normalise train and test sets and widen model matrices
  
  train <- train %>%
    tidyr::pivot_longer(cols = 3:ncol(train), names_to = "names", values_to = "values") %>%
    dplyr::left_join(train_scales, by = c("names" = "names")) %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = (values - mean) / sd) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(mean, sd)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    dplyr::mutate(group = as.factor(group))
  
  test <- test %>%
    tidyr::pivot_longer(cols = 3:ncol(test), names_to = "names", values_to = "values") %>%
    dplyr::left_join(train_scales, by = c("names" = "names")) %>%
    dplyr::group_by(names) %>%
    dplyr::mutate(values = (values - mean) / sd) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(mean, sd)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    dplyr::select(-c(id)) %>%
    dplyr::mutate(group = as.factor(group))
  
  myMatrix <- list(train, test)
  return(myMatrix)
}

#--------------
# Model fitting
#--------------

fit_multivariate_models <- function(mydata1, mydata2){
  
  # Main procedure
  
  if(test_method == "linear svm"){
    mod <- e1071::svm(group ~., data = mydata1, kernel = "linear", cross = 10, probability = TRUE)
  } else{
    mod <- e1071::svm(group ~., data = mydata1, kernel = "radial", cross = 10, probability = TRUE)
  }
  
  # Get outputs for main model
  
  cm <- as.data.frame(table(mydata2$group, predict(mod, newdata = mydata2))) %>%
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
  
  # Empirical null
    
  y <- mydata1 %>% dplyr::pull(group)
  y <- as.character(y)
  shuffles <- sample(y, replace = FALSE)
    
  inputData2 <- mydata1 %>%
    dplyr::mutate(group = shuffles,
                  group = as.factor(group))
    
  # Fit classifier
    
  if(test_method == "linear svm"){
    modNULL <- e1071::svm(group ~., data = inputData2, kernel = "linear", cross = 10, probability = TRUE)
  } else{
    modNULL <- e1071::svm(group ~., data = inputData2, kernel = "radial", cross = 10, probability = TRUE)
  }
    
  # Get outputs for model
    
  cmNULL <- as.data.frame(table(inputData2$group, predict(modNULL, newdata = mydata2))) %>%
    dplyr::mutate(flag = ifelse(Var1 == Var2, "Same", "Different"))
    
  same_totalNULL <- cmNULL %>%
    dplyr::filter(flag == "Same") %>%
    summarise(Freq = sum(Freq)) %>%
    pull()
    
  all_totalNULL <- cmNULL %>%
    summarise(Freq = sum(Freq)) %>%
    pull()
    
  statisticNULL <- same_totalNULL / all_totalNULL
    
  outputs <- data.frame(category = c("Main", "Null"),
                        statistic = c(statistic, statisticNULL))
    
  return(outputs)
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
#' @param by_set Boolean specifying whether to compute classifiers for each feature set. Defaults to FALSE
#' @param num_splits an integer specifying the number of train-test splits to perform for error bars. Defaults to 5
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
#'   by_set = FALSE,
#'   num_splits = 5,
#'   test_method = "linear svm") 
#' }
#' 

fit_multivariate_classifier <- function(data, id_var = "id", group_var = "group",
                                        by_set = FALSE, num_splits = 5,
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
  
  # Splits
  
  if(!is.integer(num_splits) || num_splits < 1){
    stop("num_splits should be an integer >=1 specifying the number of train-test splits to perform.")
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
  
  #------------- Preprocess data --------------
  
  if(by_set){
    
    sets <- unique(normed$method)
    storage <- list()
    
    for(s in sets){
      
      #------------- Preprocess data -------------
      
      setData <- normed %>%
        dplyr::filter(method == s)
      
      inputData <- prepare_model_matrices(mydata = setData, seed = 123)
      
      #------------- Fit classifiers -------------
      
    }
    
  } else{
    
    #------------- Preprocess data -------------
    
    storage <- list()
    
    for(n in 1:num_splits){
      
      inputData <- prepare_model_matrices(mydata = data_id, seed = n)
      modelOutputs <- fit_multivariate_models(mydata1 = as.data.frame(inputData[1], mydata2 = as.data.frame(inputData[2])))
      
    }
  }
}
