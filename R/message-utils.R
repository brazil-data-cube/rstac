#' @title Utility functions
#'
#' @param msg   a `character` string with format error message.
#'
#' @param ...   values to be passed to `msg` parameter.
#'
#' @param class error class name
#'
#' @noRd
.error <- function(msg, ..., class = NULL) {
  stop(errorCondition(sprintf(msg, ...), class = class, call = NULL))
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
