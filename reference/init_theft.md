# Communicate to R the Python virtual environment containing the relevant libraries for calculating features

Communicate to R the Python virtual environment containing the relevant
libraries for calculating features

## Usage

``` r
init_theft(venv, hctsa = FALSE)
```

## Arguments

- venv:

  `character` specifying the name of the to the Python virtual
  environment where `"tsfresh"`, `"TSFEL"`, and/or `"Kats"` are
  installed

- hctsa:

  `Boolean` denoting whether pyhctsa was installed or not. Defaults to
  `FALSE`

## Value

no return value; called for side effects

## Author

Trent Henderson

## Examples

``` r
if (FALSE) { # \dontrun{
install_python_pkgs("theft-test")
init_theft("theft-test")
} # }
```
