
# theft <img src="man/figures/logo.png" align="right" width="120" />

[![CRAN
version](https://www.r-pkg.org/badges/version/theft)](https://www.r-pkg.org/pkg/theft)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/theft)](https://www.r-pkg.org/pkg/theft)
[![DOI](https://zenodo.org/badge/351259952.svg)](https://zenodo.org/badge/latestdoi/351259952)

Tools for Handling Extraction of Features from Time series (theft)

## Installation

You can install the stable version of `theft` from CRAN:

``` r
install.packages("theft")
```

You can install the development version of `theft` from GitHub using the
following:

``` r
devtools::install_github("hendersontrent/theft")
```

Please also check out our paper [Feature-Based Time-Series Analysis in R
using the theft Package](https://arxiv.org/abs/2208.06146) which
discusses the motivation and theoretical underpinnings of `theft` and
walks through all of its functionality using the [Bonn EEG
dataset](https://journals.aps.org/pre/abstract/10.1103/PhysRevE.64.061907)
— a well-studied neuroscience dataset.

## General purpose

`theft` is a software package for R that facilitates user-friendly
access to a structured analytical workflow for the extraction, analysis,
and visualisation of time-series features. As of `v0.5.4`, `theft` now
acts as a ‘mothership’ one-stop-shop package ecosystem which calls
several other smaller, more bespoke packages:

<img src="vignettes/theft-ecosystem.png" alt="The theft R package ecosystem" />

Users are encouraged to either use `theft` to access all the
functionality, or, if they require only specific functions, to explore
the individual packages. That being said, `theft` provides a structured
workflow for taking users from data import through to detailed insights
about their temporal data. This structured workflow is presented in the
graphic below:

<img src="vignettes/workflow-graphic_v05.png" width="700" alt="Structured workflow of the theft package for R" />

As you can see from the graphic above, `theft` contains a convenient and
extensive suite of tools for semi-automated processing of extracted
features (including data quality assessments and normalisation methods),
low dimensional projections (linear and nonlinear), data matrix and
feature distribution visualisations, time-series classification
procedures, statistical hypothesis testing, and various other
statistical and graphical tools. `theft` currently facilitates access to
$>1200$ time-series features.

## Citation

If you use `theft` in your own work, please cite both the paper:

T. Henderson and Ben D. Fulcher. [Feature-Based Time-Series Analysis in
R using the theft Package](https://arxiv.org/abs/2208.06146). arXiv,
(2022).

and the software:


    To cite package 'theft' in publications use:

      Henderson T (2023). _theft: Tools for Handling Extraction of Features
      from Time Series_. R package version 0.5.4,
      <https://hendersontrent.github.io/theft/>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {theft: Tools for Handling Extraction of Features from Time Series},
        author = {Trent Henderson},
        year = {2023},
        note = {R package version 0.5.4},
        url = {https://hendersontrent.github.io/theft/},
      }
