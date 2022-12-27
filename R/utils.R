#' @title Utility functions
#'
#' @description Function similar to modifyList of utils, however it is checked
#'  if the left parameter is null.
#'
#' @param x   a `list` to be compared in left side.
#'
#' @param y   a `list` to be compared in right side.
#'
#' @return a `list` with modified values.
#'
#' @noRd
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

# nocov start
links_filter <- function(x, ..., filter_fn = NULL) {

  stopifnot("links" %in% names(x))

  # check items parameter
  check_subclass(x, c("STACItem", "STACItemCollection", "STACCatalog",
                      "STACCollection", "STACCollectionList"))

  dots <- substitute(list(...))[-1]

  if (length(dots) > 0) {

    if (!is.null(names(dots)))
      .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {

      sel <- vapply(x$links, function(l) {
        eval(dots[[i]], envir = l)
      }, logical(1))

      x$links <- x$links[sel]
    }
  }

  if (!is.null(filter_fn)) {

    sel <- vapply(x$links, function(l) {
      filter_fn(l$links)
    }, logical(1))

    x$links <- x$links[sel]
  }

  x$links
}


.field_filter <- function(x, field, ..., filter_fn = NULL) {

  stopifnot(field %in% names(x))

  x <- x[[field]]
  stopifnot(!is.atomic(x))

  dots <- .check_unnamed(.capture_dots(...))

  if (length(dots) > 0) {

    if (!is.null(names(dots)))
      .error("Invalid filter arguments.")

    for (i in seq_along(dots)) {

      sel <- vapply(x, function(xi) {
        eval(dots[[i]], envir = xi)
      }, logical(1))

      x <- x[sel]
    }
  }

  if (!is.null(filter_fn)) {

    sel <- vapply(x, function(xi) {
      filter_fn(xi)
    }, logical(1))

    x <- x[sel]
  }

  x
}

.field_apply <- function(x, field, apply_fn, ...) {

  stopifnot(field %in% names(x))

  x <- x[[field]]
  stopifnot(!is.atomic(x))

  lapply(x, apply_fn, ...)
}

.field_mutate <- function(x, field, ..., apply_fn = NULL) {

  stopifnot(field %in% names(x))

  x <- x[[field]]
  stopifnot(!is.atomic(x))

  dots <- .check_named(.capture_dots(...))
  stopifnot(length(dots) > 0 && is.null(apply_fn))

  if (length(dots) > 0) {

    if (!is.null(names(dots)))
      .error("Invalid filter arguments.")

    value_d <- lapply(dots, function(di) {

      lapply(x, function(xi) eval(di, envir = xi))
    })
  }

  if (!is.null(apply_fn)) {

    value_fn <- vapply(x, function(xi) {
      apply_fn(xi)
    }, logical(1))

    x <- x[value_fn]
  }

  x
}

.capture_dots <- function(...) {

  lapply(substitute(list(...), env = environment()), unlist, recursive = F)[-1]
}

.is_named <- function(x) {

  !is.null(names(x)) && all(nzchar(names(x)))
}

.check_named <- function(x) {

  stopifnot(.is_named(x))

  x
}

.check_unnamed <- function(x) {

  stopifnot(!.is_named(x))

  x
}
# nocov end
