eval_filter_expr <- function(f, expr) {
  value <- tryCatch({
    eval(expr, envir = f, enclos = parent.env(parent.frame()))
  }, error = function(e) {
    return(FALSE)
  })
  if (length(value) == 0)
    value <- FALSE
  value
}

eval_filter_fn <- function(f, filter_fn) {
  value <- tryCatch({
    filter_fn(f)
  }, error = function(e) {
    return(FALSE)
  })
  if (length(value) == 0)
    value <- FALSE
  value
}

non_atomic_properties <- function(items) {
  unique(unlist(lapply(items$features, function(item) {
    non_atomic <- vapply(item$properties, function(x) {
      length(x) == 0 || !is.atomic(x) || length(x) > 1
    }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
    names(item$properties)[non_atomic]
  })))
}
