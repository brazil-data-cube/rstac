#' @title Endpoint functions
#'
#' @description The `stac` function implements `/stac` API
#' endpoint (>=0.8.0), and `/` for versions 0.9.0 or higher. It prepares
#' search fields parameters to be provided to a STAC API web service. This
#' endpoint should return a STAC Catalog document containing all published data
#' catalogs.
#'
#' @param base_url      a `character` informing the base url of a
#'  STAC web service.
#'
#' @param force_version a `character` providing the version of the STAC
#'  used. If not provided, the rstac package will make requests to try to find
#'  the version of STAC used. It is highly recommended that you inform the STAC
#'  version you are using.
#'
#' @seealso
#' [stac_search()], [collections()],
#' [items()], [get_request()],
#' [post_request()]
#'
#' @return
#' A `RSTACQuery` object with the subclass `stac` containing all
#' request parameters to be provided to API service.
#'
#' @examples
#' \dontrun{
#'  stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'    get_request()
#' }
#'
#' @rdname stac
#' @export
stac <- function(base_url, force_version = NULL) {

  # check url parameter
  .check_obj(base_url, "character")

  # check version
  force_version <- force_version[[1]]
  if (!is.null(force_version) && force_version < "0.8.0")
    .warning("STAC API version '%s' is not supported by `rstac` package.",
             force_version)

  # create a new STAC
  RSTACQuery(version = force_version,
             base_url = base_url,
             params = list(),
             subclass = "stac")
}

#' @export
endpoint.stac <- function(q) {

  if (q$version < "0.9.0")
    return("/stac")
  return("/")
}

#' @export
before_request.stac <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  return(q)
}

#' @export
after_response.stac <- function(q, res) {

  content <- content_response(res, "200", "application/json")

  RSTACDocument(content = content, q = q, subclass = "STACCatalog")
}
