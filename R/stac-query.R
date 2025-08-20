#' @title Endpoint functions
#'
#' @description The `stac` function implements `/stac` API
#' endpoint (>=0.8.0), and `/` for versions 0.9.0 or higher. It prepares
#' search field parameters to be provided to a STAC API web service. This
#' endpoint should return a STAC Catalog document containing all published data
#' catalogs.
#'
#' @param base_url      a `character` informing the base URL of a
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
#' A `rstac_query` object with the subclass `stac` containing all
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
  check_character(base_url, "STAC URL must be a character value.")
  # check version
  force_version <- force_version[[1]]
  if (!is.null(force_version) && force_version < "0.8.0")
    .warning("STAC API version '%s' is not supported by `rstac` package.",
             force_version)
  # create a new STAC
  base_url <- url_normalize(base_url)
  rstac_query(
    version = force_version,
    base_url = base_url,
    params = list(),
    subclass = "stac"
  )
}

#' @export
before_request.stac <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  if (!is.null(q$version) && q$version < "0.9.0")
    return(set_query_endpoint(q, endpoint = "./stac"))
  set_query_endpoint(q, endpoint = "./")
}

#' @export
after_response.stac <- function(q, res, simplify_vector = TRUE) {
  content <- content_response_json(res, simplify_vector)
  doc_catalog(content)
}
