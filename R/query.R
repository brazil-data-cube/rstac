#' @title Query development functions
#'
#' @describeIn extensions
#' The \code{RSTACQuery()} function is a constructor of \code{RSTACQuery}
#' objects. Every extension must implement a subclass of \code{RSTACQuery} to
#' represent its queries. This is done by informing to the \code{subclass}
#' parameter the extension's subclass name.
#'
#' The \code{params} parameter is a named \code{list} where user parameters
#' must be stored. It is important to know if previous query parameters needs
#' to be keeped in the new query. If so, it is recommended do use
#' \code{\link[utils]{modifyList}()} function to merge the old and new
#' query parameters.
#'
#' If the \code{version} parameter is \code{NULL}, \code{rstac} will detect
#' STAC API version automatically.
#'
#' In general, if you are implementing a new subclass, the parameters
#' \code{version} and \code{url} will be the same as the previous query. The
#' \code{params} parameter will be merged with previous query. And subclass
#' is the extension's subclass name.
#'
#' @param version    a \code{character} with the STAC version.
#'
#' @param base_url   a \code{character} informing the base URL of a
#' STAC web service.
#'
#' @param params     a named \code{list} with all URL query parameters to be
#' appended in the URL.
#'
#' @param subclass   a \code{character} corresponding to the subclass of the
#' object to be created.
#'
#' @return
#' The \code{RSTACQuery()} function returns a \code{STACQuery} object with
#' subclass defined by \code{subclass} parameter.
#'
#' @export
RSTACQuery <- function(version = NULL, base_url, params = list(), subclass) {

  structure(
    list(version = version,
         base_url = base_url,
         endpoint = NULL,
         params = params,
         verb = "GET",
         encode = NULL),
    class = c(subclass, "RSTACQuery"))
}

#' @export
stac_version.RSTACQuery <- function(x, ...) {

  if (!is.null(x$version))
    return(x$version)

  # check in '/' endpoint
  res <- httr::GET(url = .make_url(x$base_url, endpoint = "/"), ...)

  content <- tryCatch({content_response(res, "200", "application/json")},
                      error = function(e) NULL)
  version <- content[["stac_version"]]

  # if no version was found, try '/stac' endpoint
  if (is.null(version)) {

    res <- httr::GET(url = .make_url(x$base_url, endpoint = "/stac"), ..., )
    content <- tryCatch({content_response(res, "200", "application/json")},
                        error = function(e) NULL)
    version <- content[["stac_version"]]
  }
  if (is.null(version))
    .error("Could not determine STAC version in URL '%s'.", x$base_url)

  return(version)
}

#' @export
subclass.RSTACQuery <- function(x) {

  class(x)[[1]]
}

#' @export
check_subclass.RSTACQuery <- function(x, subclasses) {

  if (!subclass(x) %in% subclasses)
    .error("Expecting %s query.",
           paste0("`", subclasses, "`", collapse = " or "))
}

#' @export
endpoint.RSTACQuery <- function(q) {

  .error("No endpoint was defined for the extension `%s`.", subclass(q))
}

#' @export
before_request.RSTACQuery <- function(q) {

  check_query_verb(q, "")
}

#' @export
after_response.RSTACQuery <- function(q, res) {

  check_query_verb(q, "")
}
