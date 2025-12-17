#---------------------------------------
# This script sets out to define a call
# to {pyhctsa} to calculate all available
# features on a given input time series
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 17 December 2025
#---------------------------------------

def pyhctsa_calculator(timeseries, warn, config_yaml):
  
    if warn == "No":
        import warnings
        warnings.filterwarnings("ignore")

    from pyhctsa.FeatureCalculator.calculator import FeatureCalculator
    calc = FeatureCalculator(configPath=config_yaml)
    
    extracted_features = calc.extract(timeseries)
    
    return extracted_features
