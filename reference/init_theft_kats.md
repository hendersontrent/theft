# Communicate to R the Python virtual environment containing kats only

Communicate to R the Python virtual environment containing kats only

## Usage

``` r
init_theft_kats(venv)
```

## Arguments

- venv:

  `character` specifying the name of the to the Python virtual
  environment where `"kats"` is installed

## Value

no return value; called for side effects

## Author

Trent Henderson

## Examples

``` r
if (FALSE) { # \dontrun{
install_python_pkgs("theft-test")
init_theft_kats("theft-test")
} # }
```
