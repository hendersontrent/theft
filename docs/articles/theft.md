# Introduction to theft

``` r
library(theft)
```

## Purpose

`theft` enables the standardised calculation of time-series features
from seven existing feature sets in both R and Python as well as any
user-supplied features.

## Core functionality

All time-series datasets passed into `theft` must be a `tbl_ts`
generated through the [`tsibble`](https://tsibble.tidyverts.org/) R
package. This ensures consistency with the broader
[`tidyverts`](https://tidyverts.org/) collection of packages. To explore
package functionality, we are going to use a dataset that comes with
`theft` called `simData`. This dataset contains some simulated time
series processes, including Gaussian noise, AR(1), ARMA(1,1), MA(1),
noisy sinusoid, and a random walk. The dataset can be accessed via:

``` r
theft::simData
```

Note that `simData` is a `tsibble` with two `key` variables: `id` and
`process` which identify each time series by its unique ID and group,
and an `index` variable of `timepoint` which denotes time indices.

The data follows the following structure:

``` r
head(simData)
#> # A tsibble: 6 x 4 [1]
#> # Key:       id, process [1]
#>     values timepoint id      process
#>      <dbl>     <int> <chr>   <chr>  
#> 1  0.0918          1 AR(1)_1 AR(1)  
#> 2  0.00617         2 AR(1)_1 AR(1)  
#> 3  0.154           3 AR(1)_1 AR(1)  
#> 4  0.100           4 AR(1)_1 AR(1)  
#> 5 -0.0219          5 AR(1)_1 AR(1)  
#> 6 -0.230           6 AR(1)_1 AR(1)
```

### Calculating time-series features

The core function in `theft` is `calculate_features`. You can choose
which subset of features to calculate with the `feature_set` argument.
The choices are currently `"catch22"`, `"feasts"`, `"tsfeatures"`,
`"tsfresh"`, `"tsfel"`, `"kats"`, and `"hctsa"`. In addition, two basic
feature sets `"quantiles"` (a set of 101 quantiles) and `"moments"` (the
first four moments of the distribution: mean, variance, skewness, and
kurtosis) are also available to be specified for users seeking to
compute simple baselines against which to compare the more sophisticated
feature sets (see [this recent paper](https://arxiv.org/abs/2303.17809)
for more discussion on this idea).

Note that `kats`, `tsfresh`, `tsfel`, and `pyhctsa` are Python packages.
The R package `reticulate` is used to call Python code that uses these
packages and applies it within the broader *tidy* data philosophy
embodied by `theft`. `theft` currently provides access to
$`\approx 6000`$ features from these seven sets alone. However, as
discussed in the functionality demonstrations below, you can also supply
your own list of features too!

#### Installing Python feature sets

Prior to using `theft` (only if you want to use the `kats`, `tsfresh`,
`tsfel`, or `hctsa` feature sets; the R-based sets will run fine) you
should have a working Python 3.9 installation and run the function
`install_python_pkgs(venv, python, hctsa = FALSE)` (or with
`hctsa = TRUE` if you want that large feature set) after first
installing `theft`, where the `venv` argument is the name of the virtual
environment you want to create and `python` is the path to the Python
interpreter you want to use. Note that the filepath to the Python
interpreter will differ depending on your operating system. For example,
on Windows it might be a path similar to
`"C:/Users/YourName/AppData/Local/Programs/Python/Python310/python.exe"`,
whereas on on Linux or MacOS (which was used for this vignette) it could
be a path similar to `"/usr/local/bin/python3.10"` or
`"/usr/bin/python3"`.

For example, if you wanted to install the Python libraries to the
default virtual environment folder used by `reticulate`, you would run
the following after first having installed `theft` (here I am just
creating a new virtual environment called `"theft-package"`—you can call
it whatever you like!):

``` r
install_python_pkgs(venv = "theft-package", python = "/usr/local/bin/python3.10")
```

You can then run the following to activate the virtual environment:

``` r
init_theft("theft-package")
```

You are now ready to commit theft!

**NOTE 1: You only need to call** `init_theft` **once per session.**

**NOTE 2: There are also separate installation functions for each Python
feature set, such as** `install_tsfresh` **if you only need one of the
libraries and want to keep your dependencies light.**

**NOTE 3: There are also separate** `init_theft` **functions for if you
are interested in only using one of the Python feature sets, such as**
`init_theft_hctsa`.

#### Calculating features

The core function in `theft` is `calculate_features` which takes the
following arguments:

- `data`—a `tbl_ts` containing the time series data
- `feature_set`—character or vector of characters denoting the set of
  time-series features to calculate. Can be one or more of `"catch22"`,
  `"feasts"`, `"tsfeatures"`, `"tsfresh"`, `"tsfel"`, `"kats"`,
  `"quantiles"`, or `"moments"`
- `features`—a named list containing a set of user-supplied functions to
  calculate on `data`. Each function should take a single argument which
  is the time series. Defaults to `NULL` for no manually-specified
  features. Each list entry must have a name as `calculate_features`
  looks for these to name the features. If you don’t want to use the
  existing feature sets and only compute those passed to `features`, set
  `feature_set = NULL`
- `catch24`—Boolean specifying whether to compute `catch24` in addition
  to `catch22` if `catch22` is one of the feature sets selected.
  Defaults to `FALSE`
- `tsfresh_cleanup`—Boolean specifying whether to use the in-built
  `tsfresh` relevant feature filter or not. Defaults to `FALSE`
- `use_compengine`—Boolean specifying whether to use the `"compengine"`
  features in `tsfeatures`. Defaults to `FALSE` to provide immense
  computational efficiency benefits
- `seed`—integer denoting a fixed number for R’s random number generator
  to ensure reproducibility. Defaults to `123`
- `z_score`—Boolean specifying whether to *z*-score the time-series
  before computing features. Defaults to `FALSE`
- `n_jobs`—integer denoting the number of parallel processes to use if
  `"tsfresh"` or `"tsfel"` are specified in `feature_set`. Defaults to
  `0` for no parallelisation
- `warn`—Boolean specifying whether to produce warnings from feature set
  packages. Defaults to `TRUE`

Here is an example with the `catch22` set:

``` r
feature_matrix <- calculate_features(data = simData, feature_set = "catch22")
head(feature_matrix)
#>        id group feature_set                    names      values
#> 1 AR(1)_1 AR(1)     catch22       DN_HistogramMode_5 -0.14319571
#> 2 AR(1)_1 AR(1)     catch22      DN_HistogramMode_10 -0.38084145
#> 3 AR(1)_1 AR(1)     catch22                CO_f1ecac  2.10193383
#> 4 AR(1)_1 AR(1)     catch22           CO_FirstMin_ac  5.00000000
#> 5 AR(1)_1 AR(1)     catch22 CO_HistogramAMI_even_2_5  0.07406701
#> 6 AR(1)_1 AR(1)     catch22            CO_trev_1_num  0.22413119
```

Note that `data` must be a `tsibble::tbl_ts` object, which has specified
`key` (i.e., identifying) and `index` (i.e., time) variables. `theft`
treats the first variable in the `key` as the ID variable and the second
as the grouping variable (if there is one). Any other key variables will
be ignored by `theft`.

You can also supply your own named list of functions to compute as
time-series features. Below is an example with mean and standard
deviation. Note that the list *must* be named as `theft` uses the list
element names to label the time-series features internally. Note that if
you don’t want to use any of the existing feature sets in `theft` and
only calculate the features you supply to `features`, just set
`feature_set = NULL`.

``` r
feature_matrix2 <- calculate_features(data = simData, 
                                      feature_set = NULL,
                                      features = list("mean" = mean, "sd" = sd))

head(feature_matrix2)
#>         id group feature_set names      values
#> 1  AR(1)_1 AR(1)        User  mean  0.01835873
#> 2  AR(1)_1 AR(1)        User    sd  0.09866731
#> 3 AR(1)_10 AR(1)        User  mean -0.02025604
#> 4 AR(1)_10 AR(1)        User    sd  0.10892881
#> 5 AR(1)_11 AR(1)        User  mean -0.02012172
#> 6 AR(1)_11 AR(1)        User    sd  0.08864821
```

### Comparison of feature sets

For a detailed comparison of the six feature sets, see [this
paper](https://ieeexplore.ieee.org/document/9679937) for a detailed
review[^1].

## Reading and processing hctsa-formatted files

As `theft` is based on the foundations laid by
[`hctsa`](https://github.com/benfulcher/hctsa), there is also
functionality for reading in `hctsa`-formatted Matlab files and
automatically processing them into tidy dataframes ready for feature
extraction in `theft`. The `process_hctsa_file` function takes a string
filepath to the Matlab file and does all the work for you, returning a
dataframe with naming conventions consistent with other `theft`
functionality. As per `hctsa` specifications for [Input File Format
1](https://time-series-features.gitbook.io/hctsa-manual/installing-and-using-hctsa/calculating/input_files#input-file-format-1-.mat-file),
this file should have 3 variables with the following exact names:
`timeSeriesData`, `labels`, and `keywords`. The filepath can be a local
drive path or a URL.

## Analysing, interpreting, and visualising time-series features

Please see the companion package
[`theftdlc`](https://github.com/hendersontrent/theftdlc) (‘`theft`
downloadable content’) for a large suite of functions that are designed
to work on top of `theft`.

[^1]: T. Henderson and B. D. Fulcher, “An Empirical Evaluation of
    Time-Series Feature Sets,” 2021 International Conference on Data
    Mining Workshops (ICDMW), 2021, pp. 1032-1038, doi:
    10.1109/ICDMW53433.2021.00134.
