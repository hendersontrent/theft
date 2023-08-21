library(purloiner)
library(theft)

feature_matrix <- purloiner::extract_features(data = theft::simData, 
                                              id_var = "id", 
                                              time_var = "timepoint", 
                                              values_var = "values", 
                                              group_var = "process", 
                                              feature_set = "catch22", 
                                              catch24 = FALSE)
