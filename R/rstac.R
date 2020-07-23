#' @title R client library for STAC (rstac)
#'
#' @section The \code{rstac} functions:
#' The rstac package provides two categories of functions:
#' API endpoints and assets access.
#'
#' The rstac functions can be divided in two groups, one those functions
#' implementing STAC API endpoints and, other, those functions for data
#' access (e.g. assets) and metadata organization.
#'
#' @section STAC API endpoints functions:
#' \itemize{
#'   \item \code{\link{stac}}: implements STAC \code{/stac} endpoint.
#'   \item \code{\link{stac_collections}}: implements \code{/collections}
#'     and \code{/collections/\{collectionId\}} WFS3 endpoints.
#'   \item \code{\link{stac_items}}: implements
#'     \code{/collections/\{collectionId\}/items} and
#'     \code{/collections/\{collectionId\}/items/\{itemId\}} WFS3 endpoints.
#'   \item \code{\link{stac_search}}: implements STAC \code{/stac/search}
#'     endpoint.
#' }
#'
#' @section Data access and organization functions:
#' \itemize{
#'   \item \code{\link{stac_request}}: makes HTTP requests to STAC web service.
#'   \item \code{\link{items_matched}}: returns how many items matched the
#'     search criteria.
#'   \item \code{\link{items_length}}: informs how many items are
#'     stored locally.
#'   \item \code{items_fetch}: fetches all matched items from service.
#'   \item \code{items_group}: group items by some property field.
#'   \item \code{\link{assets_download}}: download all assets in batch.
#' }
#'
#' @section Data types:
#' The package implements two S3 classes, \code{stac_items} and
#' \code{stac_collection}. These classes are regular lists representing
#' the corresponding JSON STAC objects.
#'
#' @name rstac
"_PACKAGE"
NULL

#' @importFrom jsonlite validate fromJSON
#' @importFrom curl curl_fetch_memory new_handle handle_setheaders curl_download
#' @importFrom httr GET POST write_disk add_headers content status_code http_type
NULL
