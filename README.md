
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
automatically fit an array of statistical and machine learning models
for classification problems. Users can decide to use the informed
default model parameters or add their own parameterisations. The
difference in philosophy between inferential and predictive modelling is
significant. To the best of the author’s knowledge, this is the first R
package, at least in the time-series feature field, to explicitly
distinguish between the premises as a form of methodology. Inferential
functions in `sawlog` that adopt a Bayesian approach focus on using the
entire available dataset and generate full Bayesian posterior estimates
of the relationship between time-series features and group membership.
This also means smaller datasets can benefit from simulations through
Hamiltonian Monte Carlo over distributions and handle class-imbalance
through additional flexible parameterisation.

Machine learning model options in `sawlog` adopt a train-test split
approach for learning and validation, however, this may be replaced with
K-fold cross-validation in the future.

The available models include:

  - Logistic Gaussian process classification *\[Statistical, Bayesian\]*
  - Bayesian logistic regression *\[Statistical, Bayesian\]*
  - Mixed-effects Bayesian logistic regression *\[Statistical,
    Bayesian\]*
  - Generalised additive model (GAM) *\[Statistical, Frequentist\]*
  - Mixed-effects GAM *\[Statistical, Frequentist\]*
  - Support Vector Machine (SVM) *\[Machine learning\]*
  - Random forest *\[Machine learning\]*
  - Neural network *\[Machine learning\]*

All Bayesian models are written in the probabilistic programming
language [Stan](https://mc-stan.org).

### Other functionality

`sawlog` also automates model diagnostics and data visualisation,
providing one-line functions that can generate markdown documents
containing model outputs, assumption testing, and other results and
diagnostics. As the goal is to drive usability and adoption of
feature-based approaches to time-series problems, facilitating access to
this simple end-to-end workflow from feature calculations to model
diagnostics through an intuitive suite of functions is critical.

## Future work

Future package development will seek to use the
[Keras](https://keras.io) deep learning library for the neural network
model selection option.

## Citation

``` 

To cite package 'sawlog' in publications use:

  Trent Henderson (2021). sawlog: Structured Analysis Workflow for
  statistical Learning On Groups of time-series features. R package
  version 0.1.1.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {sawlog: Structured Analysis Workflow for statistical Learning On Groups of time-series features},
    author = {Trent Henderson},
    year = {2021},
    note = {R package version 0.1.1},
  }
```
