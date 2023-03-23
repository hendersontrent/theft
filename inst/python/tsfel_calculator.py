#---------------------------------------
# This script sets out to define a call
# to {TSFEL} to calculate all available
# features on a given input time series
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 19 April 2021
#---------------------------------------

def tsfel_calculator(x):
    
    import tsfel

    # Instantiate calculation configuration

    cfg_file = tsfel.get_features_by_domain()

    # Produce calculations
    
    extracted_features = tsfel.time_series_features_extractor(cfg_file, x, verbose=0)
    
    return extracted_features
