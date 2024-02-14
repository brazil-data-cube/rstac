#' @title Utility functions
#'
#' @description
#' These function retrieves information about either `rstac` queries
#' (`rstac_query` objects) or `rstac` documents
#' (`rstac_doc` objects).
#'
#' @param x   either a `rstac_query` object expressing a STAC query
#' criteria or any `rstac_doc`.
#'
#' @param ... config parameters to be passed to [GET][httr::GET]
#' method, such as [add_headers][httr::add_headers] or [set_cookies][httr::set_cookies].
#'
#' @return
#' The `stac_version()` function returns a `character` STAC API
#' version.
#'
#' @name stac_functions
#'
#' @export
stac_version <- function(x, ...) {
  UseMethod("stac_version", x)
}

#' @rdname stac_functions
#'
#' @export
stac_type <- function(x) {
  UseMethod("stac_type", x)
}
