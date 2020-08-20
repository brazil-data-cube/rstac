#' @title Items functions
#'
#' @description The \code{items_length} function returns how many items
#' there are in the \code{items} object.
#'
#' @param items      a \code{stac_items} object representing the request
#'  results of \code{/stac/search}, \code{/collections/{collectionId}/items}, or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @return
#' An \code{integer} value.
#'
#' @examples
#' \dontrun{
#'
#' stac(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     stac_search() %>%
#'     get_request() %>%
#'     items_length()
#' }
#'
#' @export
items_length <- function(items) {

  # Check object class
  .check_obj(items, "stac_items")

  return(length(items$features))
}
