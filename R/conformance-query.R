#' @title doc_conformance endpoint
#'
#' @description
#' The conformance endpoint provides the capabilities of
#' the service.
#' This endpoint is accessible from the provider's catalog (`/conformance`).
#'
#' @param q a `rstac_query` object expressing a STAC query criteria.
#'
#' @seealso [get_request()],  [stac()], [collections()]
#'
#' @return
#' A `rstac_query` object with the subclass `conformance` for `/conformance`
#' endpoint.
#'
#' @examples
#' \dontrun{
#' stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   conformance() %>%
#'   get_request()
#' }
#'
#' @export
conformance <- function(q) {
  check_query(q, "stac")
  rstac_query(
    version = q$version,
    base_url = q$base_url,
    params = q$params,
    subclass = "conformance"
  )
}

#' @export
before_request.conformance <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  set_query_endpoint(q, endpoint = "./conformance")
}

#' @export
after_response.conformance <- function(q, res, simplify_vector = TRUE) {
  content <- content_response_json(res, simplify_vector)
  doc_conformance(content)
}
