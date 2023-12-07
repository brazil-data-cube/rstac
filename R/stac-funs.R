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
#' @export
stac_version <- function(x, ...) {
  UseMethod("stac_version", x)
}

stac_subclass <- function(obj) {
  if (!is.list(obj) || is.null(names(obj)))
    .error("Invalid STAC document.")
  if ("type" %in% names(obj)) {
    if (obj$type == "Feature")
      return("doc_item")
    if (obj$type == "FeatureCollection")
      return("doc_items")
    if (obj$type == "Collection")
      return("doc_collection")
    if (obj$type == "Catalog")
      return("doc_catalog")
    .error("Invalid STAC document. Key value 'type': '", obj$type,
           "' is not a supported STAC document.")
  } else {
    if ("conformsTo" %in% names(obj))
      return("doc_conformance")
    if ("collections" %in% names(obj))
      return("doc_collections")
    if ("id" %in% names(obj) && "links" %in% names(obj))
      return("doc_collection")
    if ("links" %in% names(obj))
      return("doc_catalog")
    .error("Invalid STAC document.")
  }
}
