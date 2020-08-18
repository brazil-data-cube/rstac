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
#' A \code{stac} object with subclass \code{subclass} containing all request
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

  content <- .build_stac(url = url,
                        endpoint = "/stac",
                        params = list())

  return(content)
}

#' @title stac object builder function
#'
#' @author Rolf Simoes
#'
#' @description The \code{.build_stac} function builds a stac object based on a
#'  given \code{stac} object and others parameters.
#'
#' @param url        a \code{character} informing the base url of a
#'  STAC web service.
#'
#' @param endpoint   a \code{character} a path to be appended in the final
#' url.
#'
#' @param params     a named \code{list} with all url query parameters to be
#' appended in the url.
#'
#' @param subclass   a \code{character} corresponding to the subclass of the
#'  object to be created.
#'
#' @param base_stac  a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{collections},
#' or \code{items} functions.
#'
#' @return
#' A \code{stac} object with subclass \code{subclass} containing all request
#'  parameters to be provided to API service.
.build_stac <- function(url, endpoint, params, subclass, base_stac) {

  base_params <- list()
  if (!missing(base_stac)) {

    .check_obj(base_stac, "stac")
    base_params <- base_stac$params
  }

  if (missing(subclass))
    subclass <- character()

  new_stac <- structure(list(url = url,
                             endpoint = endpoint,
                             params = utils::modifyList(base_params, params)),
                        class = c(subclass, "stac"))
  return(new_stac)
}

params_get_request.stac <- function(s) {

  return(s$params)
}

params_post_request.stac <- function(s, enctype) {

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
