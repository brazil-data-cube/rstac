#' @title Endpoint functions
#'
#' @description The \code{stac} function implements \code{/stac} API
#' endpoint (v0.8.1). It prepares search fields parameters to be provided to
#' a STAC API web service. This endpoint should return a STAC Catalog document
#' containing all data Items searchable in the API.
#'
#' @param url           a \code{character} informing the base url of a
#'  STAC web service.
#'
#' @param ...          other params to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods
#'
#' @param force_version a \code{character} providing the version of the stac
#'  used. If not provided, the rstac package will make requests to try to find
#'  the version of STAC used. It is highly recommended that you inform the STAC
#'  version you are using.
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
stac <- function(url, ..., force_version) {

  # check url parameter
  .check_obj(url, "character")

  # determine STAC version
  if (missing(force_version))
    version <- .detect_version(url, ...)
  else
    version <- force_version

  if (version < "0.8.0")
    .warning("The STAC version '%s' is not supported by `rstac` package.",
             version)

  content <- .build_stac(url = url,
                         endpoint = .stac_landpage_endpoint(version = version),
                         version = version,
                         headers = list(...),
                         params = list())

  return(content)
}

#' @title verify stac object
#'
#' @param url        a \code{character} informing the base url of a
#'  STAC web service.
#'
#' @param ...       other params to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods.
#'
#' @return a \code{character}  with the STAC version found or an error if it is
#'  not possible to obtain it.
.detect_version <- function(url, ...) {
  # TODO: implement token

  res <- httr::GET(url = .make_url(url, endpoint = "/"), ...)
  content <- .check_response(res, "200", "application/json")

  version <- content[["stac_version"]]

  if (is.null(version)) {
    res <- httr::GET(url = .make_url(url, endpoint = "/stac"), ..., )
    content <- .check_response(res, "200", "application/json")

    version <- content[["stac_version"]]
  }

  if (is.null(version))
    .error("Could not determine STAC version in URL '%s'.", url)

  return(version)
}

#' @title stac object builder function
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
#' @param version    a \code{character} with the STAC version.
#'
#' @param headers    a \code{character} of named arguments to be passed as
#' HTTP request headers.
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
#'
#' @noRd
.build_stac <- function(url, endpoint, version, headers, params, subclass,
                        base_stac) {


  base_params <- list()
  if (!missing(base_stac)) {

    .check_obj(base_stac, "stac")
    base_params <- base_stac$params
  }

  if (missing(version))
    version <- base_stac$version

  if (missing(headers))
    headers <- base_stac$headers

  if (missing(subclass))
    subclass <- character()

  new_stac <- structure(list(url = url,
                             endpoint = endpoint,
                             version = version,
                             headers = headers,
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
