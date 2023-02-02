#' @title Conformance endpoint
#'
#' @description
#' The conformance endpoint provides the capabilities of
#' the service.
#' This endpoint is accessible from the provider's catalog (`/conformance`).
#'
#' @param q a `RSTACQuery` object expressing a STAC query criteria.
#'
#' @seealso [get_request()],  [stac()], [collections()]
#'
#' @return
#' A `RSTACQuery` object with the subclass `conformance` for `/conformance`
#' endpoint.
#'
#' @examples
#' \dontrun{
#' stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   conformance() %>% get_request()
#' }
#'
#' @export
conformance <- function(q) {
  # check q parameter
  check_subclass(q, "stac")

  RSTACQuery(version = q$version,
             base_url = q$base_url,
             params = q$params,
             subclass = unique(c("conformance", subclass(q))))
}

#' @export
endpoint.conformance <- function(q) {
return("/conformance")
}

#' @export
before_request.conformance <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  return(q)
}

#' @export
after_response.conformance <- function(q, res) {
  content <- content_response(
    res,
    status_codes = "200",
    content_types = "application/.*json",
    key_message = c("message", "description", "detail")
  )
  RSTACDocument(content = content, q = q, subclass = "Conformance")
}
