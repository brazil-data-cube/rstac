#' @title Endpoint functions
#'
#' @author Rolf Simoes
#'
#' @description The \code{stac} function implements \code{/stac} API
#' endpoint (v0.8.1). It prepares query parameters to be provided to
#' \code{stac_request} function. This endpoint should return a STAC
#' Catalog containing all data Items searchable in the API.
#'
#' @param url        a \code{character} informing the base url of a
#'  STAC web service.
#'
#' @seealso
#' \code{\link{stac_search}}, \code{\link{get_request}},
#' \code{\link{post_request}}
#'
#' @return
#' A \code{stac} object with subclass \code{mutator} containing all request
#' parameters to be provided to API service.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     get_request()
#' }
#'
#' @rdname stac
#' @export
stac <- function(url) {

  # check url parameter
  .check_obj(url, "character")

  content <- build_stac(url = url,
                        endpoint = "/stac",
                        params = list())

  return(content)
}

build_stac <- function(url, endpoint, params, mutator, base_stac) {

  base_params <- list()
  if (!missing(base_stac)) {

    .check_obj(base_stac, "stac")
    base_params <- base_stac$params
  }

  if (missing(mutator))
    mutator <- character()

  new_stac <- structure(list(url = url,
                             endpoint = endpoint,
                             params = utils::modifyList(base_params, params)),
                        class = c(mutator, "stac"))
  return(new_stac)
}

params_get_mutator.stac <- function(s) {

  return(s$params)
}

params_post_mutator.stac <- function(s, enctype) {

  return(s$params)
}

content_get_response.stac <- function(s, res) {

  content <- structure(
    .check_response(res, "200", "application/json"),
    stac = s,
    request = list(method = "get"),
    class = "stac_catalog")

  return(content)
}

content_post_response.stac <- function(s, res, enctype) {

  content <- structure(
    .check_response(res, "200", "application/json"),
    stac = s,
    request = list(method = "post", enctype = enctype),
    class = "stac_catalog")

  return(content)
}

