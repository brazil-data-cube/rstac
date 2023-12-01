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
