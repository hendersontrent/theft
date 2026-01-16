# Communicate to R the Python virtual environment containing tsfresh only

Communicate to R the Python virtual environment containing tsfresh only

## Usage

``` r
init_theft_tsfresh(venv)
```

## Arguments

- venv:

  `character` specifying the name of the to the Python virtual
  environment where `"tsfresh"` is installed

## Value

no return value; called for side effects

## Author

Trent Henderson

## Examples

``` r
if (FALSE) { # \dontrun{
install_python_pkgs("theft-test")
init_theft_tsfresh("theft-test")
} # }
```
