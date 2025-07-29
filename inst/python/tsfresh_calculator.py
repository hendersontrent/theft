#---------------------------------------
# This script sets out to define a call
# to {tsfresh} to calculate all available
# features on a given input time series
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 15 April 2021
#---------------------------------------

def tsfresh_calculator(timeseries, column_id, column_sort, cleanup, n_jobs, classes = None):

    from tsfresh import extract_features
    from tsfresh import extract_relevant_features
    import pandas as pd
    
    if cleanup == "Yes":
        y = pd.Series(classes['group'].values, index=classes['id'])
        extracted_features = extract_relevant_features(timeseries, y, column_id = column_id, column_sort = column_sort, n_jobs = n_jobs)
    else:
        extracted_features = extract_features(timeseries, column_id = column_id, column_sort = column_sort, n_jobs = n_jobs)
    
    return extracted_features
