#---------------------------------------
# This script sets out to define a call
# to {Kats} to calculate all available
# features on a given input time series
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 24 June 2021
#---------------------------------------

def kats_calculator(timepoints, values, warn):
  
    if warn == "No":
        import warnings
        warnings.filterwarnings("ignore")
    
    import pandas as pd
    from kats.consts import TimeSeriesData
    from kats.tsfeatures.tsfeatures import TsFeatures
    
    # Transform data to correct object
    
    data = pd.DataFrame({'time':timepoints, 'value':values})
    data['time'] = pd.to_datetime(data['time'])
    data['time'] = [x.date() for x in data.time]
    data = TimeSeriesData(data)

    # Instantiate TsFeatures

    model = TsFeatures()

    # Run calculations

    extracted_features = model.transform(data)
    
    return extracted_features
