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

items_as_tibble <- function(items) {
  non_atomic_props <- function(items) {
    unique(unlist(lapply(items$features, function(item) {
      non_atomic <- vapply(item$properties, function(x) {
        is.null(x) || !is.atomic(x) || length(x) > 1
      }, FUN.VALUE = logical(1), USE.NAMES = FALSE)
      names(item$properties)[non_atomic]
    })))
  }
  atomic <- setdiff(items_properties(items), non_atomic_props(items))
  data <- lapply(items$features, function(item) {
    values <- item$properties[atomic]
    names(values) <- atomic
    values
  })
  data <- do.call(mapply, args = c(list(FUN = c, SIMPLIFY = FALSE), data))
  structure(
    data,
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = if (length(data)) c(NA, -length(data[[1]])) else integer(0)
  )
}
