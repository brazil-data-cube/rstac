#' @title Endpoint functions
#'
#' @author Rolf Simoes
#'
#' @description This function implements \code{/stac} API
#'  endpoint (v0.8.0). It prepares query parameters to be provided to
#'  \code{stac_request} function.
#'
#' This endpoint should return a STAC Catalog containing all data Items
#'  searchable in the API.
#'
#' @param url     a \code{character} informing the base url of a
#'  STAC web service.
#'
#' @seealso
#' \code{\link{stac_search}}, \code{\link{get_request}},
#'  \code{\link{post_request}}
#'
#' @return
#' A \code{stac} object containing all request parameters to be
#' provided to \code{stac_request}.
#'
#' @examples
#' \dontrun{
#'
#'  stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'  get_request()
#' }
#'
#' @export
stac <- function(url) {

  # check url parameter
  .check_obj(url, "character")

  # TODO: add these code excerpts bellow in different file
  expected <- list("get" =
                     list(responses =
                            list("200" =
                                   list("application/json" = "stac_catalog"))),
                   "post" =
                     list(enctypes = c("application/json"),
                          responses =
                            list("200" =
                                   list("application/json" = "stac_catalog"))))

  content <- structure(list(url = .make_url(url, endpoint = "/stac"),
                            params = list(),
                            expected_responses = expected),
                       class = "stac")
  return(content)
}

#' @export
print.stac <- function(x, ...) {
  cat("<stac>\n")

  named_vector("$url", x$url)
  named_vector("$params", x$params)
}
