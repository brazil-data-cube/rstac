#' @title Endpoint functions
#'
#' @author Rolf Simoes
#'
#' @description This function implements \code{/stac} API
#' endpoint (v0.8.0). It prepares query parameters to be provided to
#' \code{stac_request} function.
#'
#' This endpoint should return a STAC Catalog containing all data Items
#' searcheable in the API.
#'
#' @param url     A \code{character} informing the base url of a
#' STAC web service.
#'
#' @seealso
#' \code{\link{stac_search}}, \code{\link{stac_request}}
#'
#' @return
#' A \code{stac} object containing all request parameters to be
#' provided to \code{stac_request}.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     stac_request()
#' }
#'
#' @export
stac <- function(url) {

  # TODO: check stac API version and generate the properly endpoint
  # STAC API (<=0.8.0): "/stac"
  # STAC API (>=0.9.0): "/"
  # How to check STAC API version:
  # Maybe request endpoint "/", the landing page endpoint of WFS3.
  content <- structure(list(url = url,
                            endpoint = "/stac",
                            params = list(),
                            method = "get"),
                       class = "stac")
  return(content)
}
