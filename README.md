
# sawlog <img src="man/figures/logo.png" align="right" width="120" />

R package for Structured Analysis Workflow for statistical Learning On
Groups of time-series features (sawlog)

## Installation

Currently, installation of `sawlog` requires a working installation of
[GNU Scientific Library (GSL)](https://www.gnu.org/software/gsl/) as
`sawlog` uses functions coded in C from the
[`catchEmAll`](https://github.com/hendersontrent/catchEmAll) package
that relies on GSL. GSL is a library of scientific and statistical
functions for C and C++ programmers. Please download and install this
before installing `sawlog`. This dependency may be rectified in the near
future.

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
machine learning models including:

### Inferential models

  - Mixed-effects (optional)[Generalised additive
    models](https://en.wikipedia.org/wiki/Generalized_additive_model)
    (GAM) *\[statistical\]*
  - Bayesian generalised linear models (GLM) using the probabilistic
    programming language [Stan](https://mc-stan.org) *\[statistical\]*

### Out-of-sample classification models

  - GAM *\[statistical\]*
  - Bayesian GLM *\[statistical\]*
  - Support Vector Machine (SVM) *\[machine learning\]*
  - Random Forest *\[machine learning\]*

`sawlog` also automates model diagnostics and data visualisation,
providing one-line functions that can generate markdown documents
containing model outputs, assumption testing, and other results and
diagnostics. As the goal is to drive usability and adoption of
feature-based approaches to time-series problems, facilitating access to
this simple end-to-end workflow from feature calculations to model
diagnostics through an intuitive API of functions is critical.
