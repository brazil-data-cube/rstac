#' @title Items function
#'
#' @author Rolf Simoes
#'
#' @description The \code{items_fetch} function returns the pagination of all
#'  items of the stac object
#'
#' @param items      A \code{stac_items} object representing the result of
#'  \code{/stac/search}, \code{/collections/{collectionId}/items}, or
#'  \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param headers    A \code{list} of named arguments to be passed as
#'  http request headers. This is used in \emph{addition} to eventual headers
#'  defined in \code{stac} object parameter.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{stac_collections}}
#' \code{\link{stac_items}}
#'
#' @return
#' A \code{stac_items} object.
#'
#' @examples
#' \dontrun{
#'
#' stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'             collections = "MOD13Q1",
#'             bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'     stac_request() %>%
#'     items_fetch()
#' }
#'
#' @export
items_fetch <- function(items, headers = list()) {

  if (!inherits(items, "stac_items", "stac_item"))
    stop(sprintf("Invalid `stac_items` object."), call. = FALSE)

  next_url <- Filter(function(x) x$rel == "next", items$links)
  if (length(next_url) == 0)
    return(items)

  next_url <- next_url[[1]]$href

  s <- attr(items, "stac")
  # if (is.null(s) || (inherits(s, "stac") && s$))

  content <- stac_request(s, method = s$method)
  return(content)
}
