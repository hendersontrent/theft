# Communicate to R the Python virtual environment containing tsfel only

Communicate to R the Python virtual environment containing tsfel only

## Usage

``` r
init_theft_tsfel(venv)
```

## Arguments

- venv:

  `character` specifying the name of the to the Python virtual
  environment where `"tsfel"` is installed

## Value

no return value; called for side effects

## Author

Trent Henderson

## Examples

``` r
if (FALSE) { # \dontrun{
install_python_pkgs("theft-test")
init_theft_tsfel("theft-test")
} # }
```
