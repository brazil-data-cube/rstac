#' @title Query development functions
#'
#' @describeIn extensions
#' The `rstac_query()` function is a constructor of `rstac_query`
#' objects. Every extension must implement a subclass of `rstac_query` to
#' represent its queries. This is done by informing to the `subclass`
#' parameter the extension's subclass name.
#'
#' The `params` parameter is a named `list` where user parameters
#' must be stored. It is important to know if previous query parameters needs
#' to be keeped in the new query. If so, it is recommended do use
#' [utils::modifyList()] function to merge the old and new
#' query parameters.
#'
#' If the `version` parameter is `NULL`, `rstac` will detect
#' STAC API version automatically.
#'
#' In general, if you are implementing a new subclass, the parameters
#' `version` and `url` will be the same as the previous query. The
#' `params` parameter will be merged with previous query. And subclass
#' is the extension's subclass name.
#'
#' @param version    a `character` with the STAC version.
#'
#' @param base_url   a `character` informing the base URL of a
#' STAC web service.
#'
#' @param params     a named `list` with all URL query parameters to be
#' appended in the URL.
#'
#' @param subclass   a `character` corresponding to the subclass of the
#' object to be created.
#'
#' @return
#' The `rstac_query()` function returns a `STACQuery` object with
#' subclass defined by `subclass` parameter.
rstac_query <- function(version = NULL, base_url, params = list(), subclass) {
  structure(
    list(version = version,
         base_url = base_url,
         endpoint = NULL,
         params = params,
         verb = "GET",
         encode = NULL),
    class = c(subclass, "rstac_query"))
}

#' @export
stac_version.rstac_query <- function(x, ...) {
  if (!is.null(x$version))
    return(x$version)
  version <- NULL
  # check in '/' endpoint
  res <- make_get_request(
    url = resolve_url(x$base_url, "./"),
    ...
  )
  if (!is.null(res)) {
    content <- content_response_json(res)
    version <- content$stac_version
  }
  # if no version was found, try './stac' endpoint
  if (is.null(version)) {
    res <- make_get_request(
      url = resolve_url(x$base_url, "./stac"),
      ...
    )
    if (!is.null(res)) {
      content <- content_response_json(res)
      version <- content$stac_version
    }
  }
  if (is.null(version))
    .error(paste(
      "Could not determine STAC version in URL '%s'.",
      "Please, use 'force_version' parameter in stac() function"
    ), x$base_url)
  version
}

#' @export
subclass.rstac_query <- function(x) {
  setdiff(class(x), "rstac_query")
}

query_class <- function(q) {
  class(q)[[1]]
}
