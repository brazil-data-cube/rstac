#' @title Items functions
#'
#' @description The \code{items_matched} returns how many items matched the
#' search criteria. It implements \code{search:metadata} STAC API
#' extension (v0.8.0).
#'
#' @param items      a \code{stac_items} object representing the request
#'  results of \code{/stac/search}, \code{/collections/{collectionId}/items}, or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @return
#' An \code{integer} value. If STAC web server does not support this
#' extension, returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'
#' stac_search("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     get_request() %>%
#'     items_matched()
#' }
#'
#' @export
items_matched <- function(items) {

  # TODO: check stac API version and find properly field
  # STAC API (<=0.8.1): "search:metadata"
  # STAC API (>=0.9.0): "context"
  # How to check STAC API version:
  # Maybe `stac_version` field.
  # .stac_version <- function()
  # TODO: create a hook extension

  # Check object class
  .check_obj(items, "stac_items")


  # v0.8.1 extension
  if (!is.null(items$`search:metadata`$matched))
    matched <- items$`search:metadata`$matched
  # v0.9.0 extension
  else if (!is.null(items$`context`$matched)) {
    matched <- items$`context`$matched
  } else {
    matched <- NULL
  }

  # try WFS3 spec
  if (is.null(matched))
    matched <- items$numberMatched

  if (is.null(matched))
    .warning("STAC 'search:metadata' extension not available")

  return(matched)
}
