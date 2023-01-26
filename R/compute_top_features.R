#--------------- Helper functions ----------------

#--------------------------
# Pairwise correlation plot
#--------------------------

draw_top_feature_plot <- function(data, cor_method, clust_method, num_features){
  
  # Wrangle dataframe
  
  cor_dat <- data %>%
    dplyr::select(c(.data$id, .data$names, .data$values)) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = .data$id, names_from = .data$names, values_from = .data$values) %>%
    dplyr::select(-c(.data$id))
  
  # Calculate correlations and take absolute
  
  result <- abs(stats::cor(cor_dat, method = cor_method))
  
  # Perform clustering
  
  row.order <- stats::hclust(stats::dist(result, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
  col.order <- stats::hclust(stats::dist(t(result), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
  dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
  cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe
  
  # Define a nice colour palette consistent with RColorBrewer in other functions
  
  mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
  
  # Draw plot
  
  FeatureFeatureCorrelationPlot <- cluster_out %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$value)) +
    ggplot2::labs(title = paste0("Pairwise correlation matrix of top ", num_features, " features"),
                  x = NULL,
                  y = NULL,
                  fill = "Absolute correlation coefficient") +
    ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                               show.limits = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(FeatureFeatureCorrelationPlot)
}

#-------------
# Violin plots
#-------------

plot_feature_discrimination <- function(data, id_var = "id", group_var = "group",
                                        normalise = FALSE,
                                        method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                                        rank_data){
  
  #------------- Normalise data -------------------
  
  if(normalise){
    
    normed <- data %>%
      dplyr::select(c(.data$id, .data$names, .data$values, .data$group)) %>%
      tidyr::drop_na() %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(values = normalise_feature_vector(.data$values, method = method)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    if(nrow(normed) != nrow(data)){
      message("Filtered out rows containing NaNs.")
    }
  } else{
    normed <- data
  }
  
  #------------- Normalise data -------------------
  
  facet_order <- rank_data %>%
    dplyr::pull(.data$feature)
  
  normed <- normed %>% 
    dplyr::mutate(names = factor(.data$names, levels = facet_order))
  
  #------------- Produce plots --------------------
  
  # Draw plot
  
  p <- normed %>%
    dplyr::mutate(group = as.factor(.data$group)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$group, y = .data$values, colour = .data$group)) +
    ggplot2::geom_violin()
  
  if(length(unique(normed$names)) > 8){
    p <- p +
      ggplot2::geom_point(size = 0.7, alpha = 0.9, position = ggplot2::position_jitter(w = 0.05))
  } else{
    p <- p +
      ggplot2::geom_point(size = 1, alpha = 0.9, position = ggplot2::position_jitter(w = 0.05))
  }
  
  p <- p +  
    ggplot2::labs(title = "Class discrimination for top performing features",
                  subtitle = "Features are ordered by performance from left to right",
                  x = "Class",
                  y = "Value") +
    ggplot2::scale_colour_brewer(palette = "Dark2") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90))
  
  if(normalise){
    p <- p +
      ggplot2::facet_wrap(~names, ncol = 3)
  } else{
    p <- p +
      ggplot2::facet_wrap(~names, ncol = 3, scales = "free_y")
  }
  
  return(p)
}

#-------------- Main exported function ---------------

#' Return an object containing results from top-performing features on a classification task
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom stats hclust dist cor
#' @importFrom reshape2 melt
#' @importFrom janitor clean_names
#' @param data the \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to \code{"id"}
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to \code{"group"}
#' @param num_features the number of top features to retain and explore. Defaults to \code{40}
#' @param normalise_violin_plots a Boolean of whether to normalise features before plotting. Defaults to \code{FALSE}
#' @param method a rescaling/normalising method to apply to violin plots. Defaults to \code{"RobustSigmoid"}
#' @param cor_method the correlation method to use. Defaults to \code{"pearson"}
#' @param test_method the algorithm to use for quantifying class separation. Defaults to \code{"gaussprRadial"}
#' @param clust_method the hierarchical clustering method to use for the pairwise correlation plot. Defaults to \code{"average"}
#' @param use_balanced_accuracy a Boolean specifying whether to use balanced accuracy as the summary metric for caret model training. Defaults to \code{FALSE}
#' @param use_k_fold a Boolean specifying whether to use k-fold procedures for generating a distribution of classification accuracy estimates if a \code{caret} model is specified for \code{test_method}. Defaults to \code{ FALSE}
#' @param num_folds an integer specifying the number of k-folds to perform if \code{use_k_fold} is set to \code{TRUE}. Defaults to \code{10}
#' @param use_empirical_null a Boolean specifying whether to use empirical null procedures to compute p-values if a \code{caret} model is specified for \code{test_method}. Defaults to \code{FALSE}
#' @param null_testing_method a string specifying the type of statistical method to use to calculate p-values. Defaults to \code{model free shuffles}
#' @param p_value_method a string specifying the method of calculating p-values. Defaults to \code{"empirical"}
#' @param num_permutations an integer specifying the number of class label shuffles to perform if \code{use_empirical_null} is \code{TRUE}. Defaults to \code{50}
#' @param pool_empirical_null a Boolean specifying whether to use the pooled empirical null distribution of all features or each features' individual empirical null distribution if a \code{caret} model is specified for \code{test_method} use_empirical_null is \code{TRUE}. Defaults to \code{FALSE}
#' @param seed fixed number for R's random number generator to ensure reproducibility
#' @return an object of class list containing a dataframe of results, a feature x feature matrix plot, and a violin plot
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
#' compute_top_features(featMat,
#'   num_features = 10,
#'   normalise_violin_plots = FALSE,
#'   method = "RobustSigmoid",
#'   cor_method = "pearson",
#'   test_method = "gaussprRadial",
#'   clust_method = "average",
#'   use_balanced_accuracy = FALSE,
#'   use_k_fold = FALSE,
#'   num_folds = 10,
#'   use_empirical_null = TRUE,
#'   null_testing_method = "ModelFreeShuffles",
#'   p_value_method = "gaussian",
#'   num_permutations = 100,
#'   pool_empirical_null = FALSE,
#'   seed = 123) 
#' }
#' 

compute_top_features <- function(data, num_features = 40,
                                 normalise_violin_plots = FALSE,
                                 method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                                 cor_method = c("pearson", "spearman"),
                                 test_method = "gaussprRadial", 
                                 clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid"),
                                 use_balanced_accuracy = FALSE,
                                 use_k_fold = FALSE, num_folds = 10, 
                                 use_empirical_null = FALSE, null_testing_method = c("ModelFreeShuffles", "NullModelFits"),
                                 p_value_method = c("empirical", "gaussian"), num_permutations = 50,
                                 pool_empirical_null = FALSE, seed = 123){
  
  # Set defaults
  
  stopifnot(inherits(data, "feature_calculations") == TRUE)
  method <- match.arg(method)
  cor_method <- match.arg(cor_method)
  clust_method <- match.arg(clust_method)
  null_testing_method <- match.arg(null_testing_method)
  p_value_method <- match.arg(p_value_method)
  
  # Upstream correction for deprecated 'binomial logistic' specification
  
  if(length(test_method) == 1 && test_method == "binomial logistic"){
    test_method <- "BinomialLogistic"
    message("'binomial logistic' is deprecated. Please specify 'BinomialLogistic' instead. Performing this conversion automatically.")
  }
  
  # Null testing options
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method == "model free shuffles"){
    message("'model free shuffles' is deprecated, please use 'ModelFreeShuffles' instead.")
    null_testing_method <- "ModelFreeShuffles"
  }
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method == "null model fits"){
    message("'null model fits' is deprecated, please use 'NullModelFits' instead.")
    null_testing_method <- "NullModelFits"
  }
  
  theoptions <- c("ModelFreeShuffles", "NullModelFits")
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method %ni% theoptions){
    stop("null_testing_method should be a single string of either 'ModelFreeShuffles' or 'NullModelFits'.")
  }
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic") && null_testing_method == "ModelFreeShuffles" && num_permutations < 1000){
    message("Null testing method 'ModelFreeShuffles' is fast. Consider running more permutations for more reliable results. N = 10000 is recommended.")
  }
  
  # Classes in the data
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    group = dplyr::all_of(group_var)) %>%
      dplyr::select(c(.data$id, .data$group, .data$method, .data$names, .data$values))
  }
  
  num_classes <- length(unique(data_id$group)) # Get number of classes in the data
  
  if(num_classes == 1){
    stop("Your data only has one class label. At least two are required to performed analysis.")
  }
  
  if(num_classes == 2 && test_method %ni% c("t-test", "wilcox", "BinomialLogistic")){
    message("Your data has two classes. Setting test_method to one of 't-test', 'wilcox', or 'BinomialLogistic' is recommended.")
  }
  
  if(((missing(test_method) || is.null(test_method))) && num_classes == 2){
    test_method <- "t-test"
    message("test_method is NULL or missing. Running t-test as default for 2-class problem.")
  }
  
  if(((missing(test_method) || is.null(test_method))) && num_classes > 2){
    test_method <- "gaussprRadial"
    message("test_method is NULL or missing, fitting 'gaussprRadial' by default.")
  }
  
  if(test_method %in% c("t-test", "wilcox", "BinomialLogistic") && num_classes > 2){
    stop("t-test, Mann-Whitney-Wilcoxon Test and binomial logistic regression can only be run for 2-class problems.")
  }
  
  # Number of top features
  
  if(num_features > length(unique(data_id$names))){
    num_features <- length(unique(data_id$names))
    message(paste0("Number of specified features exceeds number of features in your data. Automatically adjusting to ", num_features))
  }
  
  # Prep factor levels as names for {caret} if the 3 base two-class options aren't being used
  
  if(test_method %ni% c("t-test", "wilcox", "BinomialLogistic")){
    data_id <- data_id %>%
      dplyr::mutate(group = make.names(.data$group),
                    group = as.factor(.data$group))
  } else{
    data_id <- data_id %>%
      dplyr::mutate(group = as.factor(.data$group))
  }
  
  #---------------  Computations ----------------
  
  #---------------
  # Classification
  #---------------
  
  # Fit algorithm
  
  classifierOutputs <- fit_single_feature_classifier(data_id, 
                                                     id_var = "id", 
                                                     group_var = "group",
                                                     test_method = test_method,
                                                     use_balanced_accuracy = use_balanced_accuracy,
                                                     use_k_fold = use_k_fold,
                                                     num_folds = num_folds,
                                                     use_empirical_null = use_empirical_null,
                                                     null_testing_method = null_testing_method,
                                                     p_value_method = p_value_method,
                                                     num_permutations = num_permutations,
                                                     pool_empirical_null = pool_empirical_null,
                                                     seed = seed)
  
  # Filter results to get list of top features
  
  if(test_method %in% c("t-test", "wilcox", "BinomialLogistic")){
    
    message("\nSelecting top features using p-values.")
    
    ResultsTable <- classifierOutputs %>%
      dplyr::slice_min(.data$p_value, n = num_features)
    
  } else{
    
    if(use_empirical_null == FALSE){
      
      if(use_balanced_accuracy){
        message("\nSelecting top features using mean balanced classification accuracy.")
        
        ResultsTable <- classifierOutputs %>%
          dplyr::slice_max(.data$balanced_accuracy, n = num_features)
        
      } else{
        message("\nSelecting top features using mean classification accuracy.")
        
        ResultsTable <- classifierOutputs %>%
          dplyr::slice_max(.data$accuracy, n = num_features)
      }
    } else{
      
      # Catch cases where most of the p-values are the same (likely 0 given empirical null performance from experiments)
      
      if(use_balanced_accuracy){
        
        unique_p_values <- classifierOutputs %>%
          dplyr::slice_min(.data$p_value_balanced_accuracy, n = num_features)
        
        if(length(unique(unique_p_values$feature)) > num_features || length(unique(unique_p_values$p_value_balanced_accuracy)) == 1){
          
          message("\nNot enough unique p-values to select top features informatively. Selecting top features using mean classification accuracy instead.")
          
          ResultsTable <- classifierOutputs %>%
            dplyr::slice_max(.data$balanced_accuracy, n = num_features)
          
        } else{
          
          message("\nSelecting top features using p-value.")
          
          ResultsTable <- classifierOutputs %>%
            dplyr::slice_min(.data$p_value_balanced_accuracy, n = num_features)
        }
        
      } else{
        
        unique_p_values <- classifierOutputs %>%
          dplyr::slice_min(.data$p_value_accuracy, n = num_features)
        
        if(length(unique(unique_p_values$feature)) > num_features || length(unique(unique_p_values$p_value_accuracy)) == 1){
          
          message("\nNot enough unique p-values to select top features informatively. Selecting top features using mean classification accuracy instead.")
          
          ResultsTable <- classifierOutputs %>%
            dplyr::slice_max(.data$accuracy, n = num_features)
          
        } else{
          
          message("\nSelecting top features using p-value.")
          
          ResultsTable <- classifierOutputs %>%
            dplyr::slice_min(.data$p_value_accuracy, n = num_features)
        }
      }
    }
  }
  
  # Filter original data to just the top performers
  
  dataFiltered <- data_id %>%
    dplyr::mutate(names = paste0(.data$method, "_", .data$names)) %>%
    dplyr::select(-c(.data$method)) %>%
    tidyr::pivot_wider(id_cols = c("id", "group"), names_from = "names", values_from = "values") %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(cols = !c("id", "group"), names_to = "names", values_to = "values") %>%
    dplyr::filter(.data$names %in% ResultsTable$feature)
  
  #-----------------------
  # Feature x feature plot
  #-----------------------
  
  FeatureFeatureCorrelationPlot <- try(draw_top_feature_plot(data = dataFiltered,
                                                             cor_method = cor_method,
                                                             clust_method = clust_method,
                                                             num_features = num_features))
  
  #---------------
  # Violin plot
  #---------------
  
  ViolinPlots <- plot_feature_discrimination(dataFiltered, 
                                             id_var = "id", 
                                             group_var = "group",
                                             normalise = normalise_violin_plots,
                                             method = method,
                                             rank_data = ResultsTable)
  
  #---------------  Returns ---------------------
  
  # Compile into one object and return
  
  if(inherits(FeatureFeatureCorrelationPlot, "try-error") == TRUE){
    message("An error occured in producing the pairwise correlation plot. Only returning numerical results and violin plots instead.")
    myList <- list(ResultsTable, ViolinPlots)
    names(myList) <- c("ResultsTable", "ViolinPlots")
  } else{
    myList <- list(ResultsTable, FeatureFeatureCorrelationPlot, ViolinPlots)
    names(myList) <- c("ResultsTable", "FeatureFeatureCorrelationPlot", "ViolinPlots")
  }
  
  return(myList)
}
