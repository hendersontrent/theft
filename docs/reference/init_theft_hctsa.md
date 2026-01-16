# Communicate to R the Python virtual environment containing pyhctsa only

Communicate to R the Python virtual environment containing pyhctsa only

## Usage

``` r
init_theft_hctsa(venv)
```

## Arguments

- venv:

  `character` specifying the name of the to the Python virtual
  environment where `"pyhctsa"` is installed

## Value

no return value; called for side effects

## Author

Trent Henderson

## Examples

``` r
if (FALSE) { # \dontrun{
install_python_pkgs("theft-test")
init_theft_hctsa("theft-test")
} # }
```
