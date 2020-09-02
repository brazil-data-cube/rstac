#' @title Items functions
#'
#' @description The \code{items_length} function returns how many items
#' there are in the \code{items} object.
#'
#' @param items      a \code{stac_item_collection} object representing the request
#'  results of \code{/stac/search}, \code{/collections/{collectionId}/items}, or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @return
#' An \code{integer} value.
#'
#' @examples
#' \donttest{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
#'      force_version = "0.8.1") %>%
#'  stac_search(collections = "MOD13Q1",
#'          bbox = c(-55.16335, -4.26325, -49.31739, -1.18355),
#'          limit = 500) %>%
#'  get_request() %>%
#'  items_length()
#'
#' }
#'
#' @export
items_length <- function(items) {

  # Check object class
  .check_obj(items, "stac_item_collection")

  return(length(items$features))
}
