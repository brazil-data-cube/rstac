
modify_list <- function(x, y) {
  if (is.null(x))
    x <- list()
  stopifnot(is.list(x), is.list(y))
  ynames <- names(y)
  for (n in ynames) {
    x[[n]] <- y[[n]]
  }
  x
}

deprec_parameter <- function(deprec_var, deprec_version, msg = NULL) {
  called_fun <- sys.call(-1)[[1]]
  message(
    "The parameter ", deprec_var, " in ", called_fun,
    " is deprecated in version ", deprec_version, " of rstac. ", msg
  )
}

foreach_item <- function(items, fn, ...) {
  items$features <- lapply(items$features, fn, ...)
  return(items)
}

map_lgl <- function(x, fn, ..., use_names = FALSE) {
  vapply(x, fn, FUN.VALUE = logical(1), ..., USE.NAMES = use_names)
}

map_chr <- function(x, fn, ..., use_names = FALSE) {
  vapply(x, fn, FUN.VALUE = character(1), ..., USE.NAMES = use_names)
}

apply_deeply <- function(x, i = NULL, fn = identity, ...) {
  val <- if (length(i) == 0) {
    fn(x, ...)
  } else {
    star <- which(i == "*")
    if (length(star) == 0) {
      x <- tryCatch(x[[i]], error = function(e) NULL)
      if (is.null(x)) NULL else fn(x, ...)
    } else if (star[[1]] == 1) {
      lapply(x, apply_deeply, i = i[-1], fn = fn)
    } else {
      x <- tryCatch(x[[i[seq_len(star[[1]] - 1)]]], error = function(e) NULL)
      if (is.null(x)) NULL
      else lapply(x, apply_deeply, i = i[-seq(star[[1]])], fn = fn)
    }
  }
  if (is.null(names(val)) &&
      all(vapply(val, function(x) is.atomic(x) && length(x) == 1, logical(1))))
    return(unlist(val, recursive = FALSE))
  return(val)
}
