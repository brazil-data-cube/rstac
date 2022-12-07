#' @title R client library for STAC (rstac)
#'
#' @section The `rstac` functions:
#' The rstac package provides two categories of functions:
#' API endpoints and data access and organization.
#'
#' @section STAC API endpoints functions:
#' \itemize{
#'   \item [stac()]: implements STAC `/stac` endpoint for version
#'    0.8.1 or below, and `/` for versions 0.9.0 or higher.
#'   \item [collections()]: implements `/collections`
#'     and \code{/collections/\{collectionId\}} WFS3 endpoints.
#'   \item [items()]: implements
#'     \code{/collections/\{collectionId\}/items} and
#'     \code{/collections/\{collectionId\}/items/\{featureId\}} WFS3 endpoints.
#'   \item [stac_search()]: implements STAC `/stac/search`
#'   endpoint for version 0.8.1 or below, and `/search` endpoint for
#'   versions 0.9.0 or higher.
#' }
#'
#' @section Data access and organization functions:
#' \itemize{
#'   \item [get_request()]: makes HTTP GET requests to STAC web
#'   service.
#'   \item [post_request()]: makes HTTP POST requests to STAC web
#'   service.
#'   \item [items_matched()]: returns how many items matched the
#'   search criteria.
#'   \item [items_length()]: informs how many items are
#'   stored locally.
#'   \item [items_fetch()]: fetches all matched items from service.
#'   \item [assets_download()]: download all assets in batch.
#' }
#'
#' @section Data types:
#' The package implements the follow S3 classes: `STACItemCollection`,
#'  `STACItem`, `STACCatalog`, `STACCollectionList` and
#'  `STACCollection`. These classes are regular lists representing the
#'  corresponding JSON STAC objects.
#'
#' @name rstac
"_PACKAGE"
NULL

#' Pipe
#'
#' Magrittr compound assignment pipe-operator.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @param lhs,rhs A visualization and a function to apply to it.
#' @export
NULL

#' @importFrom httr GET POST write_disk add_headers content status_code
#' http_type
#' @importFrom crayon bold
#' @importFrom utils modifyList URLdecode
#' @importFrom jsonlite fromJSON
#' @importFrom lifecycle deprecated
NULL

cql2_global_env <- NULL
.onLoad <- function(lib, pkg) {
  cql2_global_env <<- cql2_load()
}
