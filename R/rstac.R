#' @title R client library for STAC (rstac)
#'
#' @section The \code{rstac} functions:
#' The rstac package provides two categories of functions:
#' API endpoints and data access and organization.
#'
#' @section STAC API endpoints functions:
#' \itemize{
#'   \item \code{\link{stac}}: implements STAC \code{/stac} endpoint for version
#'    0.8.1 or below, and \code{/} for versions 0.9.0 or higher.
#'   \item \code{\link{collections}}: implements \code{/collections}
#'     and \code{/collections/\{collectionId\}} WFS3 endpoints.
#'   \item \code{\link{items}}: implements
#'     \code{/collections/\{collectionId\}/items} and
#'     \code{/collections/\{collectionId\}/items/\{featureId\}} WFS3 endpoints.
#'   \item \code{\link{stac_search}}: implements STAC \code{/stac/search}
#'   endpoint for version 0.8.1 or below, and \code{/search} endpoint for
#'   versions 0.9.0 or higher.
#' }
#'
#' @section Data access and organization functions:
#' \itemize{
#'   \item \code{\link{get_request}}: makes HTTP GET requests to STAC web
#'   service.
#'   \item \code{\link{post_request}}: makes HTTP POST requests to STAC web
#'   service.
#'   \item \code{\link{items_matched}}: returns how many items matched the
#'   search criteria.
#'   \item \code{\link{items_length}}: informs how many items are
#'   stored locally.
#'   \item \code{\link{items_fetch}}: fetches all matched items from service.
#'   \item \code{\link{assets_download}}: download all assets in batch.
#' }
#'
#' @section Data types:
#' The package implements the follow S3 classes: \code{STACItemCollection},
#'  \code{STACItem}, \code{STACCatalog}, \code{STACCollectionList} and
#'  \code{STACCollection}. These classes are regular lists representing the
#'  corresponding JSON STAC objects.
#'
#' @name rstac
"_PACKAGE"
NULL

#' @importFrom httr GET POST write_disk add_headers content status_code http_type
#' @importFrom crayon bold
#' @importFrom utils modifyList
NULL
