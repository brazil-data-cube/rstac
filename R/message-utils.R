#' @title Utility functions
#'
#' @param msg   a `character` string with format error message.
#'
#' @param ...   values to be passed to `msg` parameter.
#'
#' @noRd
.error <- function(msg, ...) {
  stop(sprintf(msg, ...), call. = FALSE)
}

#' @title Utility functions
#'
#' @param msg   a `character` string with format warning message.
#'
#' @param ...   values to be passed to `msg` parameter.
#'
#' @noRd
.warning <- function(msg, ...) {
  warning(sprintf(msg, ...), call. = FALSE)
}
