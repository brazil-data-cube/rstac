#' @title Endpoint functions
#'
#' @description
#' The queryables endpoint allows the user to discover which
#' properties can be used in the filter extension.
#' This endpoint can be accessed from the catalog (`/queryables`)
#' or from a collection (`/collections/{collection_id}/queryables`).
#'
#' @param q  a `RSTACQuery` object expressing a STAC query criteria.
#'
#' @seealso [ext_filter()],  [conformance()], [collections()]
#'
#' @return
#' A `RSTACQuery` object with the subclass `queryables` for `/queryables`
#' endpoint.
#'
#' @examples
#' \dontrun{
#' # Catalog's queryables
#' rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   rstac::queryables() %>% rstac::get_request()
#'
#' # Collection's queryables
#' rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   rstac::collections(collection_id = "sentinel-2-l2a") %>%
#'   rstac::queryables() %>%
#'   rstac::get_request()
#' }
#'
#' @export
queryables <- function(q) {
  # check q parameter
  check_subclass(q, c("collection_id", "stac"))

  RSTACQuery(version = q$version,
             base_url = q$base_url,
             params = q$params,
             subclass = unique(c("queryables", subclass(q))))
}

#' @export
endpoint.queryables <- function(q) {
  if ("collection_id" %in% subclass(q)) {
    col_id <- q$params[["collection_id"]]
    return(paste("/collections", col_id, "queryables", sep = "/"))
  }
  return("/queryables")
}

#' @export
before_request.queryables <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  # don't send 'collection_id' in url's query string or content body
  if ("collection_id" %in% subclass(q)) {
    q <- omit_query_params(q, names = "collection_id")
  }
  return(q)
}

#' @export
after_response.queryables <- function(q, res) {
  content <- content_response(
    res, "200", c("application/geo+json", "application/json")
  )
  RSTACDocument(content = content, q = q, subclass = "Queryables")
}
