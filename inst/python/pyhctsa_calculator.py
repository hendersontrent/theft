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
        import os
        from pyhctsa.distribute import LocalDistributor

        # Workers are spawned as fresh interpreters, so they don't inherit the
        # filter above; pass it on through the environment while they run

        old_pywarnings = os.environ.get("PYTHONWARNINGS")
        if warn == "No":
            os.environ["PYTHONWARNINGS"] = "ignore"
        try:
            dist = LocalDistributor(n_workers=n_jobs)
            try:
                extracted_features = calc.extract(timeseries, distributor=dist)
            finally:
                dist.close()
        finally:
            if old_pywarnings is None:
                os.environ.pop("PYTHONWARNINGS", None)
            else:
                os.environ["PYTHONWARNINGS"] = old_pywarnings
    else:
        extracted_features = calc.extract(timeseries)

    return extracted_features
