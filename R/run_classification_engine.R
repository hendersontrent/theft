#' Fit a statistical or machine learning model to predict group membership of feature-space time-series data
#' @import dplyr
#' @importFrom magrittr %>%
#' @import mgcv
#' @import rstan
#' @importFrom e1071 svm
#' @importFrom randomForest randomForest
#' @importFrom caTools sample.split
#' @importFrom neuralnet neuralnet
#' @importFrom parallel detectCores
#' @param data a tidy dataframe of feature results where each feature is a separate column
#' @param id_var a variable that uniquely identifies each observation
#' @param group_var a variable that denotes the categorical groups each observation relates to and is the target of prediction
#' @param premise the type of analytical work to be conducted. Defaults to 'prediction'
#' @param method the classification model to use. Defaults to non-mixed-effects Generalised Additive Model 'GAM'
#' @return an object of the class of model that was fit
#' @author Trent Henderson
#' @references Wood, S.N. (2011) Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. Journal of the Royal Statistical Society (B) 73(1):3-36
#' @references Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. http://mc-stan.org/.
#' @references David Meyer, Evgenia Dimitriadou, Kurt Hornik, Andreas Weingessel and Friedrich Leisch (2019). e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.7-3. https://CRAN.R-project.org/package=e1071
#' @references A. Liaw and M. Wiener (2002). Classification and Regression by randomForest. R News 2(3), 18--22.
#' @references Stefan Fritsch, Frauke Guenther and Marvin N. Wright (2019). neuralnet: Training of Neural Networks. R package version 1.44.2. https://CRAN.R-project.org/package=neuralnet
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' temp <- tempfile()
#' download.file("https://ndownloader.figshare.com/files/24950795",temp)
#' ts <- read.csv(temp, header = FALSE)
#' 
#' ts <- ts %>%
#'   dplyr::mutate(id = dplyr::row_number()) %>%
#'   tidyr::pivot_longer(!id, names_to = "timepoint", values_to = "value") %>%
#'   dplyr::mutate(timepoint = as.numeric(gsub("V", "\\1", timepoint)))
#'
#' temp1 <- tempfile()
#' download.file("https://ndownloader.figshare.com/files/24950798",temp1)
#' ts_info <- read.csv(temp1, header = TRUE)
#'
#' main <- ts %>%
#'   dplyr::left_join(ts_info, by = c("id" = "ID")) %>%
#'   dplyr::mutate(group = dplyr::case_when(
#'                 grepl("synthetic", Keywords)  ~ 1,
#'                 !grepl("synthetic", Keywords) ~ 0)) # Creates binary
#'
#' ids <- unique(main$id)
#' ids_filt <- sample(ids, 50) # Random small sample to test
#' 
#' d1 <- main %>%
#'   dplyr::filter(id %in% ids_filt)
#'  
#'  outs <- calculate_features(data = main_filt, id_var = "id", group_var = "group", time_var = "timepoint", value_var = "value", feature_set = "feasts")
#'  outsN <- normalise_feature_frame(data = outs, names_var = "names", values_var = "values", method = "RobustSigmoid")
#'  mod <- run_classification_engine(data = outsN, id_var = "id", group_var = "group", premise = "inference", method = "BayesGLM")
#'}
#'

run_classification_engine <- function(data, id_var = NULL, group_var = NULL, premise = c("inference", "prediction"),
                                      method = c("GAM","MixedGAM","BayesGLM","MixedBayesGLM","SVM", "RandomForest", "NeuralNet")){
  
  #------------ Checks and argument validation ------------
  
  # Make prediction the default premise
  
  if(missing(premise)){
    premise <- "prediction"
  } else{
    premise <- match.arg(premise)
  }
  
  # Make GAM the default method
  
  if(missing(method)){
    method <- "GAM"
  } else{
    method <- match.arg(method)
  }
  
  # Argument checks
  
  premises <- c("inference", "prediction")
  methods <- c("GAM", "MixedGAM", "BayesGLM", "MixedBayesGLM", "SVM", "RandomForest", "NeuralNet")
  '%ni%' <- Negate('%in%')
  
  if(premise %ni% premises){
    stop("premise should be a single selection of: 'inference' or 'prediction'.")
  }
  
  if(length(premise) != 1){
    stop("premise should be a single selection of: 'inference' or 'prediction'.")
  }
  
  if(method %ni% methods){
    stop("method should be a single selection of: 'GAM', 'MixedGAM', 'BayesGLM', 'MixedBayesGLM', 'SVM', 'RandomForest' or 'NeuralNet'.")
  }
  
  if(length(method) != 1){
    stop("method should be a single selection of: 'GAM', 'MixedGAM', 'BayesGLM', 'MixedBayesGLM', 'RandomForest' or 'NeuralNet'.")
  }
  
  if(!is.null(id_var) & !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  if(!is.null(group_var) & !is.character(group_var)){
    stop("group_var should be a string specifying a variable in the input data that identifies the group membership variable/column.")
  }
  
  # Print prompt to normalise data
  
  message("Function assumes input data has been normalised. Please revise with sawlog::normalise_features() if your data is not normalised. This has large implications for model accuracy.")
  
  #------------ Quantitative data checks ------------------
  
  #-----------------
  # Outcome variable
  #-----------------
  
  # Recode into binary outcome and retain mapping
  
  if(is.null(group_var)){
    stop("group_var must be specified.")
  } else{
    data_group <- data %>%
      dplyr::rename(group = dplyr::all_of(group_var))
  }
  
  if(length(unique(data_group$group)) != 2){
    stop("group_var should have two levels.")
  }
  
  data_group <- data_group %>%
    dplyr::mutate(group = as.integer(as.factor(group)))
  
  #------------
  # ID variable
  #------------
  
  # Check if integer, recode into integer if not
  
  if(is.null(id_var)){
    data_id <- data_group %>%
      dplyr::mutate(id = dplyr::row_number())
  } else{
    data_id <- data_group %>%
      dplyr::rename(id = dplyr::all_of(id_var))
  }
  
  data_id <- data_id %>%
    dplyr::mutate(id = as.integer(id))
  
  #-----------
  # Predictors
  #-----------
  
  pred_check <- data_id %>%
    dplyr::select(-c(id, group))
  
  ncol_1 <- ncol(pred_check)
  
  # Check if numeric
  
  checker <- pred_check %>%
    dplyr::select(where(is.numeric))
  
  ncol_2 <- ncol(checker)
  
  if(ncol_1 != ncol_2){
    stop("Non-numeric values detected in feature vectors. Please re-assess data.")
  }
  
  # Final return
  
  final <- data_id
  
  #------------ Model specification and fit ---------------
  
  #----------
  # Inference
  #----------
  
  if(premise == "inference"){
    
    if(method %ni% c('GAM', "MixedGAM", "BayesGLM", "MixedBayesGLM")){
      stop("For premise 'inference', method should be a single selection of: 'GAM', 'MixedGAM', 'BayesGLM' or 'MixedBayesGLM'.")
    }
    
    if(method == "GAM"){
      
      # Build model formula
      
      final <- final %>%
        dplyr::mutate(group = as.factor(group))
      
      mm <- as.formula(paste("group ~ ", paste(final[!final %in% c("group", "id")], collapse = " + ")))
      
      # Fit model
      
      message("Fitting model... This may take a long time.")
      
      m1 <- mgcv::gam(formula = mm, data = final, method = "REML", family = binomial("logit"))
      
      return(m1)
      
    }
    
    if(method == "MixedGAM"){
      
      # Build model formula
      
      final <- final %>%
        dplyr::mutate(group = as.factor(group))
      
      mm <- as.formula(paste("group ~ ", paste(final[!final %in% c("group", "id")], collapse = " + ")))
      mm <- as.formula(paste(mm,"s(id, bs = 're')", collapse = " + ")) # Random effects for (1|id)
      
      # Fit model
      
      message("Fitting model... This may take a long time.")
      
      m1 <- mgcv::gam(formula = mm, data = final, method = "REML", family = binomial("logit"))
      
      return(m1)
      
    }
    
    if(method == "BayesGLM"){
      
      options(mc.cores = parallel::detectCores()) # Parallel processing
      
      # Set up data for Stan
      
      name_list <- as.vector(colnames(final))
      name_list <- name_list[!name_list %in% c("group", "id")]
      
      X <- final %>%
        dplyr::select(-c(id, group))
      
      stan_data <- list(N = nrow(final),
                        K = as.integer(length(name_list)),
                        y = final$group,
                        X = as.matrix(X))
      
      # Run model
      
      message("Fitting model... This may take a long time.")
      
      m1 <- rstan::stan(file = system.file("stan", "BayesGLM.stan", package = "sawlog"), 
                        data = stan_data, iter = 3000, chains = 3, seed = 123, control = list(max_treedepth = 15))
      
      return(m1)
      
    }
    
    if(method == "MixedBayesGLM"){
      
      options(mc.cores = parallel::detectCores()) # Parallel processing
      
      # Set up data for Stan
      
      name_list <- as.vector(colnames(final))
      name_list <- name_list[!name_list %in% c("group", "id")]
      
      X <- final %>%
        dplyr::select(-c(id, group))
      
      stan_data <- list(N = nrow(final),
                        K = as.integer(length(name_list)),
                        y = final$group,
                        X = as.matrix(X),
                        id = final$id)
      
      # Run model
      
      message("Fitting model... This may take a long time.")
      
      m1 <- rstan::stan(file = system.file("stan", "MixedBayesGLM.stan", package = "sawlog"), 
                        data = stan_data, iter = 3000, chains = 3, seed = 123, control = list(max_treedepth = 15))
      
      return(m1)
      
    }
    
  }
  
  #-----------
  # Prediction
  #-----------
  
  if(premise == "prediction"){
    
    if(method %ni% c('GAM', "BayesGLM", "SVM", "RandomForest", "NeuralNet")){
      stop("For premise 'inference', method should be a single selection of: 'GAM', 'BayesGLM', 'SVM', 'RandomForest' or 'NeuralNet'.")
    }
    
    #------- Make train-test split --------
    
    set.seed(123) # Fix RNG
    split <- caTools::sample.split(final$group_var, SplitRatio = 0.75) 
    train <- subset(final, split == TRUE) 
    test <- subset(final, split == FALSE)
    
    if(method == "GAM"){
      
      # Build model formula
      
      train <- train %>%
        dplyr::mutate(group = as.factor(group))
      
      test <- test %>%
        dplyr::mutate(group = as.factor(group))
      
      mm <- as.formula(paste("group ~ ", paste(n[!n %in% c("group", "id")], collapse = " + ")))
      
      #------- Train -------
      
      message("Fitting model... This may take a long time.")
      
      m1 <- mgcv::gam(formula = mm, data = train, method = "REML", family = binomial("logit"))
      
      #------- Test -------
      
      preds <- predict(m1, newdata = test, type = "response")
      
      confusion_matrix <- ftable(test$group, preds)
      accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
      
      print(paste0("Test set classification accuracy: ", accuracy))
      
      return(m1)
      
    }
    
    if(method == "BayesGLM"){
      
      options(mc.cores = parallel::detectCores()) # Parallel processing
      
      # Set up data for Stan
      
      name_list <- as.vector(colnames(train))
      name_list <- name_list[!name_list %in% c("group", "id")]
      
      X <- train %>%
        dplyr::select(-c(id, group))
      
      X_test <- test %>%
        dplyr::select(-c(id, group))
      
      stan_data <- list(N = nrow(train),
                        K = as.integer(length(name_list)),
                        y = train$group,
                        X = as.matrix(X),
                        N_test = nrow(test),
                        X_test = nrow(test))
      
      # Run model
      
      message("Fitting model... This may take a long time.")
      
      m1 <- rstan::stan(file = system.file("stan", "BayesGLM_prediction.stan", package = "sawlog"), 
                        data = stan_data, iter = 3000, chains = 3, seed = 123, control = list(max_treedepth = 15))
      
      ext_fit <- extract(m1)
      accuracy <- paste0(round(mean(apply(ext_fit$y_test, 2, median) == y_test), digits = 2),"%")
      print(paste0("Test set classification accuracy: ", accuracy))
      
      return(m1)
      
    }
    
    if(method == "SVM"){
      
      # Build model formula
      
      train <- train %>%
        dplyr::mutate(group = as.factor(group))
      
      test <- test %>%
        dplyr::mutate(group = as.factor(group))
      
      mm <- as.formula(paste("group ~ ", paste(n[!n %in% c("group", "id")], collapse = " + ")))
      
      #------- Train -------
      
      message("Fitting model... This may take a long time.")
      
      m1 <- e1071::svm(formula = mm, data = train, kernel = 'radial')
      
      #------- Test -------
      
      preds <- predict(m1, newdata = test)
      
      confusion_matrix <- ftable(test$group, preds)
      accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
      print(paste0("Test set classification accuracy: ", accuracy))
      
      return(m1)
      
    }
    
    if(method == "RandomForest"){
      
      # Build model formula
      
      train <- train %>%
        dplyr::mutate(group = as.factor(group))
      
      test <- test %>%
        dplyr::mutate(group = as.factor(group))
      
      mm <- as.formula(paste("group ~", paste(n[!n %in% c("group", "id")], collapse = " + ")))
      
      #------- Train -------
      
      message("Fitting model... This may take a long time.")
      
      m1 <- randomForest::randomForest(formula = mm, data = train, importance = TRUE)
      
      #------- Test -------
      
      preds <- predict(m1, newdata = test)
      
      confusion_matrix <- ftable(test$group, preds)
      accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
      print(paste0("Test set classification accuracy: ", accuracy))
      
      return(m1)
      
    }
    
    if(method == "NeuralNet"){
      
      # Build model formula
      
      train <- train %>%
        dplyr::mutate(group = as.factor(group))
      
      test <- test %>%
        dplyr::mutate(group = as.factor(group))
      
      mm <- as.formula(paste("group ~", paste(n[!n %in% c("group", "id")], collapse = " + ")))
      
      #------- Train -------
      
      message("Fitting model... This may take a long time.")
      
      # Conditional hidden weights based on input data size
      # NOTE: Should these weights be different?
      # NOTE: Do thresholds need to be increased due to 'difficult' real data?
      
      if(nrow(train) <= 1000){
        m1 <- neuralnet::neuralnet(formula = mm, data = train, hidden = c(5), linear.output = FALSE, stepmax = 1e6)
      }
      
      if(nrow(train) > 1000){
        m1 <- neuralnet::neuralnet(formula = mm, data = train, hidden = c(3,2), linear.output = FALSE, stepmax = 1e6)
      }
      
      #------- Test -------
      
      preds <- predict(m1, newdata = test)
      
      confusion_matrix <- ftable(test$group, preds)
      accuracy <- paste0(round(sum(diag(confusion_matrix))/confusion_matrix*100, digits = 2),"%")
      print(paste0("Test set classification accuracy: ", accuracy))
      
      return(m1)
      
    }
    
  }
  
}
