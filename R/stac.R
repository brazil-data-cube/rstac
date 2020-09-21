#' @title Endpoint functions
#'
#' @description The \code{stac} function implements \code{/stac} API
#' endpoint (>=0.8.0). It prepares search fields parameters to be provided to
#' a STAC API web service. This endpoint should return a STAC Catalog document
#' containing all published data catalogs.
#'
#' @param url           a \code{character} informing the base url of a
#'  STAC web service.
#'
#' @param force_version a \code{character} providing the version of the stac
#'  used. If not provided, the rstac package will make requests to try to find
#'  the version of STAC used. It is highly recommended that you inform the STAC
#'  version you are using.
#'
#' @seealso
#' \code{\link{stac_search}}, \code{\link{collections}},
#' \code{\link{items}}, \code{\link{get_request}},
#' \code{\link{post_request}}
#'
#' @return
#' A \code{stac} object with subclass \code{subclass} containing all request
#' parameters to be provided to API service.
#'
#' @examples
#' \donttest{
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'   get_request()
#' }
#'
#' @rdname stac
#' @export
stac <- function(url, force_version = NULL) {

  # check url parameter
  .check_obj(url, "character")

  # check version
  force_version <- force_version[[1]]
  if (!is.null(force_version) && force_version < "0.8.0")
    .warning("STAC API version '%s' is not supported by `rstac` package.",
             force_version)

  # create a new STAC
  RSTACQuery(version = force_version,
             url = url,
             params = list(),
             subclass = "stac")
}

get_endpoint.stac <- function(s) {

  if (s$version < "0.9.0")
    return("/stac")
  return("/")
}

before_request.stac <- function(s) {

  check_query_verb(s, verbs = c("GET", "POST"))

  return(s)
}

after_response.stac <- function(s, res) {

  content <- content_response(res, "200", "application/json")

  RSTACDocument(content = content, s = s, subclass = "STACCatalog")
}
