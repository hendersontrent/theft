
# theft <img src="man/figures/logo.png" align="right" width="120" />

[![CRAN
version](http://www.r-pkg.org/badges/version/catch22)](http://www.r-pkg.org/pkg/theft)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/catch22)](http://www.r-pkg.org/pkg/theft)
[![DOI](https://zenodo.org/badge/351259952.svg)](https://zenodo.org/badge/latestdoi/351259952)

Tools for Handling Extraction of Features from Time series (theft)

## Installation

*Coming to CRAN soon… Stay posted!*

You can install the development version of `theft` from GitHub using the
following:

``` r
devtools::install_github("hendersontrent/theft")
```

## General purpose

`theft` is a software package for R that facilitates user-friendly
access to a structured analytical pipeline for the computation,
analysis, and visualisation of time-series features. The package
provides a single point of access to a large number of time-series
features from a range of existing R and Python packages and lets the
user specify which groups (or all) of the these features to calculate.
The packages which `theft` currently ‘steals’ features from include:

-   [Rcatch22](https://github.com/hendersontrent/Rcatch22)
-   [feasts](https://feasts.tidyverts.org)
-   [tsfeatures](https://github.com/robjhyndman/tsfeatures)
-   [Kats](https://facebookresearch.github.io/Kats/)
-   [tsfresh](https://tsfresh.com)
-   [TSFEL](https://tsfel.readthedocs.io/en/latest/)

Note that `Kats`, `tsfresh` and `tsfel` are Python packages. The R
package `reticulate` is used to call Python code that uses these
packages and applies it within the broader *tidy* data philosophy
embodied by `theft`. At present, depending on the input time series,
`theft` provides access to &gt;1300 features. Prior to using `theft`
(only if you want to use the `Kats`, `tsfresh` or `tsfel` feature sets -
the R-based sets will run fine) you should have a working Python
installation and download `Kats` using the instructions located
[here](https://facebookresearch.github.io/Kats/), `tsfresh`
[here](https://tsfresh.com) or `tsfel`
[here](https://github.com/fraunhoferportugal/tsfel).

## Statistical and graphical tools

The package also contains a suite of tools for automatic processing of
extracted feature vectors, low dimensional projections, data matrix
visualisations, top feature and multivariate feature classification
analyses, and various other statistical and graphical procedures.

## Web application

An [interactive web
application](https://dynamicsandneuralsystems.shinyapps.io/timeseriesfeaturevis/)
has been built on top of `theft` which enables users to access most of
the functionality included in the package from within a web browser
without any code. The application automates the entire workflow included
in `theft`, converts all static graphics included in the package into
interactive visualisations, and enables downloads of feature
calculations. Note that since `theft` is an active development project,
not all functionality has been copied across to the webtool yet.

## Citation


    To cite package 'theft' in publications use:

      Trent Henderson (2022). theft: Tools for Handling Extraction of
      Features from Time series. R package version 0.3.9.0.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {theft: Tools for Handling Extraction of Features from Time series},
        author = {Trent Henderson},
        year = {2022},
        note = {R package version 0.3.9.0},
      }
