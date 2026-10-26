# Download and install tsfresh, TSFEL, and Kats from Python into a new virtual environment

Download and install tsfresh, TSFEL, and Kats from Python into a new
virtual environment

## Usage

``` r
install_python_pkgs(venv, python, hctsa = FALSE)
```

## Arguments

- venv:

  `character` specifying the name of the new virtual environment to
  create

- python:

  `character` specifying the filepath to the Python interpreter to use.
  Python 3.10 is recommended

- hctsa:

  `Boolean` denoting whether to install pyhctsa or not. Defaults to
  `FALSE`

## Value

no return value; called for side effects

## Author

Trent Henderson

## Examples

``` r
if (FALSE) { # \dontrun{
install_python_pkgs("theft-test", "/usr/local/bin/python3.10")
} # }
```
