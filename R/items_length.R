#' @title Items functions
#'
#' @author Rolf Simoes
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
#' stac_search("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     stac_request() %>%
#'     items_length()
#' }
#'
#' @export
items_length <- function(items) {

  # Check object class
  .check_obj(items, expected = c("stac_items"))

  return(length(items$features))
}
