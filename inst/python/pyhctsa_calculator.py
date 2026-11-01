#---------------------------------------
# This script sets out to define a call
# to {pyhctsa} to calculate all available
# features on a given input time series
#---------------------------------------

#---------------------------------------
# Author: Trent Henderson, 17 December 2025
#---------------------------------------

def pyhctsa_calculator(timeseries, warn, n_jobs=0):

    if warn == "No":
        import warnings
        warnings.filterwarnings("ignore")

    import numpy as np
    from pyhctsa.calculator import FeatureCalculator

    # pyhctsa's own default feature set, so it always matches the installed version

    calc = FeatureCalculator()

    # One 1-D array per time series (series may differ in length)

    timeseries = [np.asarray(ts, dtype=float).ravel() for ts in timeseries]

    if n_jobs >= 2:
        from pyhctsa.distribute import LocalDistributor
        dist = LocalDistributor(n_workers=n_jobs)
        try:
            extracted_features = calc.extract(timeseries, distributor=dist)
        finally:
            dist.close()
    else:
        extracted_features = calc.extract(timeseries)

    return extracted_features
