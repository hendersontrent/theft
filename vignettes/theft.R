## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6,
  fig.width = 6,
  warning = FALSE,
  fig.align = "center"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(theft)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  theft::simData

## ---- message = FALSE, warning = FALSE----------------------------------------
head(simData)

## ---- message = FALSE, warning = FALSE----------------------------------------
feature_matrix <- calculate_features(data = simData, 
                                     id_var = "id", 
                                     time_var = "timepoint", 
                                     values_var = "values", 
                                     group_var = "process", 
                                     feature_set = "catch22",
                                     seed = 123)

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  feature_matrix <- calculate_features(data = simData,
#                                       id_var = "id",
#                                       time_var = "timepoint",
#                                       values_var = "values",
#                                       group_var = "process",
#                                       feature_set = "catch22",
#                                       catch24 = TRUE,
#                                       seed = 123)

## ---- message = FALSE, warning = FALSE----------------------------------------
head(feature_list)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix, type = "quality")

## ---- message = FALSE, warning = FALSE----------------------------------------
normed <- normalise(feature_matrix, method = "z-score")

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix, type = "matrix")

## ---- message = FALSE, warning = FALSE----------------------------------------
low_dim <- reduce_dims(feature_matrix, 
                       method = "RobustSigmoid", 
                       low_dim_method = "PCA", 
                       seed = 123)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(low_dim)

## ---- message = FALSE, warning = FALSE----------------------------------------
low_dim2 <- reduce_dims(feature_matrix, 
                        method = "RobustSigmoid", 
                        low_dim_method = "t-SNE", 
                        perplexity = 10,
                        seed = 123)

plot(low_dim2)

## ---- message = FALSE, warning = FALSE----------------------------------------
plot(feature_matrix, type = "cor")

## ---- message = FALSE, warning = FALSE, eval = FALSE--------------------------
#  demo_outputs <- compute_top_features(feature_matrix,
#                                       num_features = 10,
#                                       normalise_violin_plots = FALSE,
#                                       method = "RobustSigmoid",
#                                       cor_method = "pearson",
#                                       test_method = "gaussprRadial",
#                                       clust_method = "average",
#                                       use_balanced_accuracy = FALSE,
#                                       use_k_fold = TRUE,
#                                       num_folds = 10,
#                                       use_empirical_null =  TRUE,
#                                       null_testing_method = "ModelFreeShuffles",
#                                       p_value_method = "gaussian",
#                                       num_permutations = 10,
#                                       pool_empirical_null = FALSE,
#                                       seed = 123)

## ---- message = FALSE, warning = FALSE, echo = FALSE--------------------------
load("outputs.rda")

## ---- message = FALSE, warning = FALSE----------------------------------------
head(demo_outputs$ResultsTable)

