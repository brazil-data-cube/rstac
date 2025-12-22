#' @title R client library for STAC (rstac)
#'
#' @section The `rstac` functions:
#' The rstac package provides two categories of functions:
#' API endpoints and data access and organization.
#'
#' @section STAC API endpoints functions:
#' \itemize{
#'   \item [stac()]: implements STAC `/stac` endpoint for version
#'     0.8.1 or below, and `/` for versions 0.9.0 or higher.
#'   \item [conformance()]: implements `/conformance` endpoint.
#'   \item [collections()]: implements `/collections`
#'     and \code{/collections/\{collectionId\}} endpoints.
#'   \item [items()]: implements
#'     \code{/collections/\{collectionId\}/items} and
#'     \code{/collections/\{collectionId\}/items/\{featureId\}} endpoints.
#'   \item [queryables()]: implements `/queryables` and
#'     \code{/collections/\{collectionId\}/queryables} endpoints.
#'   \item [stac_search()]: implements STAC `/stac/search`
#'   endpoint for version 0.8.1 or below, and `/search` endpoint for
#'   versions 0.9.0 or higher.
#'   \item [ext_filter()]: implements `/filter` `CQL2` endpoint.
#' }
#'
#' @section Data access and organization functions:
#' \itemize{
#'   \item [get_request()]: makes HTTP GET requests to STAC web service.
#'   \item [post_request()]: makes HTTP POST requests to STAC web service.
#'   \item [items_matched()]: returns how many items matched the
#'   search criteria.
#'   \item [items_fetch()]: fetches all matched items from service.
#'   \item [items_filter()]: selects items according to some criteria.
#'   \item [items_as_sf()]: converts items to a `sf` object.
#'   \item [items_fields()]: help explore fields inside items.
#'   \item [items_compact()]: removes all items with empty assets.
#'   \item [items_reap()]: extracts contents from items.
#'   \item [items_length()]: informs how many items are fetched locally.
#'   \item [items_sign()]: appends tokens to assets' URL and turn
#'   them accessible.
#'   \item [assets_select()]: select assets in items.
#'   \item [assets_rename()]: rename assets in items.
#'   \item [assets_url()]: extract all URL to assets in items.
#'   \item [assets_download()]: download assets in batch.
#' }
#'
#' @section Data types:
#' The package implements the following S3 classes: `doc_items`,
#'  `doc_item`, `doc_catalog`, `doc_collections` and
#'  `doc_collection`. These classes are regular lists representing the
#'  corresponding JSON STAC objects.
#'
#' @name rstac
"_PACKAGE"
NULL

#' @importFrom magrittr %>%
#' @keywords internal
#' @export
magrittr::`%>%`

#' @importFrom utils txtProgressBar setTxtProgressBar modifyList URLdecode
#' @importFrom httr write_disk http_type content status_code parse_url add_headers build_url GET POST
#' @importFrom jsonlite fromJSON read_json
#' @importFrom sf st_geometry st_geometry_type st_sf st_sfc st_transform st_intersects
#' @importFrom grid grid.raster
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom crayon bold
NULL
