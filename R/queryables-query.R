#' @title Endpoint functions
#'
#' @description
#' The `/queryables` endpoint allows the user to discover which
#' properties can be used in the filter extension.
#' This endpoint can be accessed from the catalog (`/queryables`)
#' or from a collection (`/collections/{collection_id}/queryables`).
#'
#' @param q  a `rstac_query` object expressing a STAC query criteria.
#'
#' @seealso [ext_filter()],  [conformance()], [collections()]
#'
#' @return
#' A `rstac_query` object with the subclass `queryables` for `/queryables`
#' endpoint.
#'
#' @examples
#' \dontrun{
#' # Catalog's queryables
#' stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   queryables() %>%
#'   get_request()
#'
#' # Collection's queryables
#' stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   collections(collection_id = "sentinel-2-l2a") %>%
#'   queryables() %>%
#'   get_request()
#' }
#'
#' @export
queryables <- function(q) {
  check_query(q, c("collection_id", "stac"))
  rstac_query(
    version = q$version,
    base_url = q$base_url,
    params = q$params,
    subclass = "queryables"
  )
}

#' @export
before_request.queryables <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  if ("collection_id" %in% names(q$params)) {
    return(set_query_endpoint(q,
      endpoint = "./collections/%s/queryables",
      params = "collection_id"
    ))
  }
  set_query_endpoint(q, endpoint = "./queryables")
}

#' @export
after_response.queryables <- function(q, res, simplify_vector = TRUE) {
  content <- content_response_json(res, simplify_vector)
  doc_queryables(content)
}
