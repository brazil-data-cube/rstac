#' @title Utility functions
#'
#' @description
#' These function retrieves information about either `rstac` queries
#' (`RSTACQuery` objects) or `rstac` documents
#' (`RSTACDocument` objects).
#'
#' @param x   either a `RSTACQuery` object expressing a STAC query
#' criteria or any `RSTACDocument`.
#'
#' @param ... config parameters to be passed to [GET][httr::GET]
#' method, such as [add_headers][httr::add_headers] or [set_cookies][httr::set_cookies].
#'
#' @return
#' The `stac_version()` function returns a `character` STAC API
#' version.
#'
#' @name utilities
#'
#' @export
stac_version <- function(x, ...) {

  UseMethod("stac_version", x)
}
