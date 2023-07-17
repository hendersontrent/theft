# NOTE: This is from {tidyselect} but due to import limitations for CRAN (and it not being namespaced) it's rebuilt here
# See https://github.com/r-lib/tidyselect/blob/main/R/helpers-where.R for implementation

mywhere <- function(fn) {
  predicate <- rlang::as_function(fn)
  
  function(x, ...) {
    out <- predicate(x, ...)
    
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    
    out
  }
}
