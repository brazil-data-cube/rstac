#' @title Items functions
#'
#' @author Rolf Simoes
#'
#' @description The \code{items_length} function returns how many items
#' there are in the \code{items} object.
#'
#' @param items A \code{stac_items} object.
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

  if (!inherits(items, "stac_items"))
    stop(sprintf("Invalid `stac_items` object."))

  return(length(items$features))
}
