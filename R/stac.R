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
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
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

   content <- structure(list(url = url,
                             endpoint = "/stac",
                             params = list(),
                             expected_responses = expected,
                             mutator = "stac"),
                        class = "stac")

  return(content)
}

build_stac <- function(url, endpoint, params, expected_responses,
                       mutator, old_stac) {

  old_params <- list()
  if (!missing(old_stac) && inherits(old_stac, "stac"))
    old_params <- old_stac$params

  new_stac <- structure(list(url = url,
                             endpoint = endpoint,
                             params = utils::modifyList(old_params, params),
                             expected_responses = expected_responses,
                             mutator = mutator),
                        class = "stac")
  return(new_stac)
}
