#------------------------- feature_calculations object methods -------------------------

#' Produce a plot for a feature_calculations object
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr pivot_wider pivot_longer drop_na
#' @importFrom reshape2 melt
#' @importFrom stats hclust dist cor
#' @param data the \code{feature_calculations} object containing the raw feature matrix produced by \code{calculate_features}
#' @param type \code{string} specifying the type of plot to draw. Defaults to \code{"quality"}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"z-score"}
#' @param clust_method the hierarchical clustering method to use for the pairwise correlation plot. Defaults to \code{"average"}
#' @return an object of class \code{ggplot} that contains the heatmap graphic
#' @author Trent Henderson
#' @export
#' 

plot.feature_calculations <- function(data, type = c("quality", "matrix", "cor"), 
                                      method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax"),
                                      clust_method = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid")){
  
  stopifnot(inherits(data, "feature_calculations") == TRUE)
  type <- match.arg(type)
  method <- match.arg(method)
  clust_method <- match.arg(clust_method)
  
  if(type == "quality"){
    
    #--------------- Calculate proportions ------------
    
    tmp <- data %>%
      dplyr::mutate(quality = dplyr::case_when(
        is.na(.data$values)                               ~ "NaN",
        is.nan(.data$values)                              ~ "NaN",
        is.infinite(.data$values)                         ~ "-Inf or Inf",
        is.numeric(.data$values) & !is.na(.data$values) &
          !is.na(.data$values) & !is.nan(.data$values)    ~ "Good")) %>%
      dplyr::group_by(.data$names, .data$quality) %>%
      dplyr::summarise(counter = dplyr::n()) %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(props = .data$counter / sum(.data$counter)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(quality = factor(.data$quality, levels = c("-Inf or Inf", "NaN", "Good")))
    
    # Calculate order of 'good' quality feature vectors to visually steer plot
    
    ordering <- tmp %>%
      dplyr::filter(.data$quality == "Good") %>%
      dplyr::mutate(ranker = dplyr::dense_rank(.data$props)) %>%
      dplyr::select(c(.data$names, .data$ranker))
    
    # Join back in
    
    tmp <- tmp %>%
      dplyr::left_join(ordering, by = c("names" = "names"))
    
    if(ignore_good_features){
      tmp <- tmp %>%
        dplyr::filter(.data$quality != "Good")
      
      if(nrow(tmp) == 0){
        message("All feature values are good. Exiting without producing plot.")
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
      }
    }
    
    #--------------- Draw plot ------------------------
    
    # Define a nice colour palette consistent with RColorBrewer in other functions
    
    my_palette <- c("-Inf or Inf" = "#7570B3",
                    "NaN" = "#D95F02",
                    "Good" = "#1B9E77")
    
    # Plot
    
    p <- tmp %>%
      ggplot2::ggplot(ggplot2::aes(x = stats::reorder(.data$names, -.data$ranker), y = .data$props)) +
      ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = .data$quality)) +
      ggplot2::labs(title = "Data quality for computed features",
                    x = "Feature",
                    y = "Proportion of Outputs",
                    fill = "Data Type") +
      ggplot2::scale_y_continuous(limits = c(0,1),
                                  breaks = seq(from = 0, to = 1, by = 0.1)) +
      ggplot2::scale_fill_manual(values = my_palette) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     legend.position = "bottom")
    
    if(length(unique(tmp$names)) > 22){
      p <- p + 
        ggplot2::theme(axis.text.x = ggplot2::element_blank())
    } else{
      p <- p + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  } else if(type == "matrix"){
  
    #------------- Apply normalisation -------------
    
    data_id <- data_id %>%
      dplyr::rename(feature_set = .data$method) %>% # Avoids issues with method arg later
      dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
      dplyr::select(-c(.data$feature_set)) %>%
      dplyr::group_by(.data$names) %>%
      dplyr::mutate(values = normalise(.data$values, method = method)) %>%
      dplyr::ungroup()
    
    #------------- Hierarchical clustering ----------
    
    dat <- data_id %>%
      tidyr::pivot_wider(id_cols = "id", names_from = "names", values_from = "values") %>%
      tibble::column_to_rownames(var = "id")
    
    # Remove any columns with >50% NAs to prevent masses of rows getting dropped due to poor features
    
    dat_filtered <- dat[, which(colMeans(!is.na(dat)) > 0.5)]
    
    # Drop any remaining rows with NAs
    
    dat_filtered <- dat_filtered %>%
      tidyr::drop_na()
    
    if(nrow(dat_filtered) != nrow(dat)){
      message("Dropped rows with NAs to enable clustering.")
    }
    
    row.order <- stats::hclust(stats::dist(dat_filtered, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
    col.order <- stats::hclust(stats::dist(t(dat_filtered), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
    dat_new <- dat_filtered[row.order, col.order] # Re-order matrix by cluster outputs
    
    cluster_out <- reshape2::melt(as.matrix(dat_new)) %>% # Turn into dataframe
      dplyr::rename(id = .data$Var1,
                    names = .data$Var2)
    
    #------------- Draw graphic ---------------------
    
    # Define a nice colour palette consistent with RColorBrewer in other functions
    
    mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
    
    if(interactive){
      p <- cluster_out %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$names, y = .data$id, fill = .data$value,
                                     text = paste('<br><b>ID:</b>', .data$id,
                                                  '<br><b>Feature:</b>', .data$names,
                                                  '<br><b>Scaled Value:</b>', round(.data$value, digits = 3)))) +
        ggplot2::geom_raster() +
        ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                                   show.limits = TRUE)
      
    } else{
      p <- cluster_out %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$names, y = .data$id, fill = .data$value))  +
        ggplot2::geom_raster() +
        ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                                   show.limits = TRUE)
    }
    
    p <- p +
      ggplot2::labs(title = "Data matrix",
                    x = "Feature",
                    y = "Time series") +
      ggplot2::theme_bw() + 
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank()) +
      ggplot2::labs(fill = "Scaled value")
    
    if(length(unique(cluster_out$names)) <= 22){
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       axis.ticks.y = ggplot2::element_blank())
    } else {
      p <- p +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank())
    }
    
    p <- p +
      ggplot2::theme(legend.position = "bottom")
    
  } else {
    
    # Dataframe length checks and tidy format wrangling
    
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var),
                    names = dplyr::all_of(names_var),
                    values = dplyr::all_of(values_var))
    
    #------------- Clean up structure --------------
    
    data_id <- data_id %>%
      dplyr::rename(feature_set = .data$method) %>% # Avoids issues with method arg later
      dplyr::select(c(.data$id, .data$names, .data$values, .data$feature_set)) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(names = paste0(.data$feature_set, "_", .data$names)) %>% # Catches errors when using all features across sets (i.e., there's duplicates)
      dplyr::select(-c(.data$feature_set))
    
    if(nrow(data_id) < nrow(data)){
      message("Filtered out rows containing NaNs.")
    }
    
    #------------- Data reshaping -------------------
    
    features <- unique(data_id$names)
    
    ids_to_keep <- data_id %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(counter = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$counter == length(features))
    
    ids_to_keep <- ids_to_keep$id
    
    cor_dat <- data_id %>%
      dplyr::filter(.data$id %in% ids_to_keep) %>%
      tidyr::pivot_wider(id_cols = "names", names_from = "id", values_from = "values") %>%
      dplyr::select(-c(.data$names))
    
    #--------- Correlation ----------
    
    # Calculate correlations and take absolute
    
    result <- abs(stats::cor(cor_dat, method = cor_method))
    
    #--------- Clustering -----------
    
    # Wrangle into tidy format
    
    melted <- reshape2::melt(result)
    
    # Perform clustering
    
    row.order <- stats::hclust(stats::dist(result, method = "euclidean"), method = clust_method)$order # Hierarchical cluster on rows
    col.order <- stats::hclust(stats::dist(t(result), method = "euclidean"), method = clust_method)$order # Hierarchical cluster on columns
    dat_new <- result[row.order, col.order] # Re-order matrix by cluster outputs
    cluster_out <- reshape2::melt(as.matrix(dat_new)) # Turn into dataframe
    
    #--------- Graphic --------------
    
    # Define a nice colour palette consistent with RColorBrewer in other functions
    
    mypalette <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
    
    if(interactive){
      p <- cluster_out %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2,
                                     text = paste('<br><b>ID 1:</b>', .data$Var1,
                                                  '<br><b>ID 2:</b>', .data$Var2,
                                                  '<br><b>Absolute correlation:</b>', round(.data$value, digits = 3))))
    } else{
      p <- cluster_out %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$Var1, y = .data$Var2)) 
    }
    
    p <- p +
      ggplot2::geom_raster(ggplot2::aes(fill = .data$value)) +
      ggplot2::labs(title = "Pairwise correlation matrix",
                    x = "Feature",
                    y = "Feature",
                    fill = "Absolute correlation coefficient") +
      ggplot2::scale_fill_stepsn(n.breaks = 6, colours = rev(mypalette),
                                 show.limits = TRUE) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
                     legend.position = "bottom")
    
    if(nrow(cluster_out) <= 20){
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
      p <- p +
        ggplot2::theme(axis.text = ggplot2::element_blank())
    }
  }
  
  return(p)
}

#------------------------- low_dimension object methods -------------------------

#' Produce a plot for a low_dimension object
#' @importFrom rlang .data
#' @import dplyr
#' @import ggplot2
#' @import tibble
#' @importFrom tidyr drop_na
#' @importFrom broom augment tidy
#' @param data the \code{low_dimension} object containing the raw feature matrix produced by \code{reduce_dims}
#' @param method a rescaling/normalising method to apply. Defaults to \code{"z-score"}
#' @return an object of class \code{ggplot} that contains the heatmap graphic
#' @author Trent Henderson
#' @export
#' 

plot.low_dimension <- function(data, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax")){
  
  stopifnot(inherits(data, "low_dimension") == TRUE)
  method <- match.arg(method)
  
  if(inherits(data, "prcomp") == TRUE){
    xx
  } else{
    xx
  }
}
