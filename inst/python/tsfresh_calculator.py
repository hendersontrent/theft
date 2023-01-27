#---------------------------------------
# This script sets out to define a call
# to {tsfresh} to calculate all available
# features on a given input time series
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 15 April 2021
#---------------------------------------

def tsfresh_calculator(timeseries, y = None, column_id, column_sort, cleanup):

    from tsfresh import extract_features
    from tsfresh import extract_relevant_features
    
    if cleanup == "Yes":
        extracted_features = extract_relevant_features(timeseries, column_id = column_id, column_sort = column_sort)
    else:
        extracted_features = extract_features(timeseries, column_id = column_id, column_sort = column_sort)
    
    return extracted_features
