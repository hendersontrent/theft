
# sawlog <img src="man/figures/logo.png" align="right" width="120" />

[![CRAN
version](http://www.r-pkg.org/badges/version/catch22)](http://www.r-pkg.org/pkg/catch22)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/catch22)](http://www.r-pkg.org/pkg/catch22)

R package for Structured Analysis Workflow for statistical Learning On
Groups of time-series features (sawlog)

## Installation

*Coming to CRAN soon… Stay posted\!*

You can install the development version of `sawlog` from GitHub using
the following:

``` r
devtools::install_github("hendersontrent/sawlog")
```

## General purpose

`sawlog` is an R package that facilitates user-friendly access to a
principled and structured analytical pipeline for the computation,
analysis, and visualisation of time-series feature-based classification
problems. The package pulls and concatenates a large number of
time-series feature statistic methods from a range of existing R
packages and lets the user specify which groups (or all) of the these
features to use in modelling.

`sawlog` provides high-level, easy-to-use functions that let users
specify their intention of statistical inference or out-of-sample
prediction/classification modelling, and fit an array of statistical and
machine learning models. The difference in philosophy between
inferential and predictive modelling is significant. To the best of the
author’s knowledge, this is the first R package, at least in the
time-series space, to explicitly distinguish between the premises as a
form of methodology. Inferential functions in `sawlog` focus on using
the entire available dataset and generate full Bayesian or maximum
likelihood estimates of population time-series feature statistics.
Predictive functions in `sawlog` adopt a machine learning train-test
split approach for learning and validation, where the Bayesian models
use the fitted model to generate posterior predictions on the test set.

The available models include:

### Inferential models

  - Mixed-effects (optional) [Generalised additive
    models](https://en.wikipedia.org/wiki/Generalized_additive_model)
    (GAM) *\[statistical\]*
  - Mixed-effects (optional) Bayesian generalised linear models (GLM)
    written in [Stan](https://mc-stan.org) *\[statistical\]*

### Out-of-sample classification models

  - GAM *\[statistical\]*
  - Bayesian GLM *\[statistical\]*
  - Support Vector Machine (SVM) *\[machine learning\]*
  - Random forest *\[machine learning\]*
  - Neural network *\[machine learning\]*

### Other functionality

`sawlog` also automates model diagnostics and data visualisation,
providing one-line functions that can generate markdown documents
containing model outputs, assumption testing, and other results and
diagnostics. As the goal is to drive usability and adoption of
feature-based approaches to time-series problems, facilitating access to
this simple end-to-end workflow from feature calculations to model
diagnostics through an intuitive suite of functions is critical.
