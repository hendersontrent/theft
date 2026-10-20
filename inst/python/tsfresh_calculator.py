#---------------------------------------
# This script sets out to define a call
# to {tsfresh} to calculate all available
# features on a given input time series
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 15 April 2021
#---------------------------------------

def tsfresh_calculator(timeseries, column_id, column_sort, cleanup, n_jobs, warn, classes = None):
  
    if warn == "No":
        import warnings
        warnings.filterwarnings("ignore")

    from tsfresh import extract_features
    from tsfresh import extract_relevant_features
    import pandas as pd
    
    if cleanup == "Yes":
        y = pd.Series(classes['group'].values, index=classes[column_id].values)
        extracted_features = extract_relevant_features(timeseries, y, column_id = column_id, column_sort = column_sort, n_jobs = n_jobs)
    else:
        extracted_features = extract_features(timeseries, column_id = column_id, column_sort = column_sort, n_jobs = n_jobs)
    
    # Return the series IDs as a column so R can join on them instead of relying on row order
    
    extracted_features.index.name = column_id
    extracted_features = extracted_features.reset_index()
    
    return extracted_features
