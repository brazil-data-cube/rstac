#' @title Items functions
#'
#' @author Rolf Simoes
#'
#' @description The \code{items_matched} returns how many items matched the
#' search criteria. It implements \code{search:metadata} STAC API
#' extension (v0.8.0).
#'
#' @param items    A \code{stac_items} object.
#'
#' @return
#' An \code{integer} value. If STAC web server does not support this
#' extension, returns \code{NULL}.
#'
#' @examples
#' \dontrun{
#'
#' stac_search("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     stac_request() %>%
#'     items_matched()
#' }
#'
#' @export
items_matched <- function(items) {

  # TODO: check stac API version and find properly field
  # STAC API (<=0.8.0): "search:metadata"
  # STAC API (>=0.9.0): "context"
  # How to check STAC API version:
  # Maybe `stac_version` field.
  # .stac_version <- function()
  if (!inherits(items, "stac_items"))
    stop(sprintf("Invalid `stac_items` object."))

  matched <- items[["search:metadata"]][["matched"]]
  if (is.null(matched))
    warning("STAC 'search:metadata' extension not available", call. = FALSE)

  return(matched)
}


# TODO:
