
#' @export
stac_version.RSTACQuery <- function(x, ...) {

  if (!is.null(x$version))
    return(x$version)

  # check in '/' endpoint
  res <- httr::GET(url = .make_url(x$url, endpoint = "/"), ...)

  content <- tryCatch({content_response(res, "200", "application/json")},
                      error = function(e) NULL)
  version <- content[["stac_version"]]

  # if no version was found, try '/stac' endpoint
  if (is.null(version)) {

    res <- httr::GET(url = .make_url(x$url, endpoint = "/stac"), ..., )
    content <- tryCatch({content_response(res, "200", "application/json")},
                        error = function(e) NULL)
    version <- content[["stac_version"]]
  }
  if (is.null(version))
    .error("Could not determine STAC version in URL '%s'.", x$url)

  return(version)
}

#' @export
subclass.RSTACQuery <- function(x) {

  class(x)[[1]]
}

#' @export
print.RSTACQuery <- function(x, ...) {

  cat("###RSTACQuery", fill = TRUE)
  cat("- url:", x$url, fill = TRUE)
  cat("- params:", fill = TRUE)
  for (n in names(x$params)) {
    cat(paste0("  - ", n, ":"), fill = TRUE)
  }
  cat("- attributes:", paste0(names(x), collapse = ", "), fill = TRUE)
  invisible(x)
}

#' @export
repr_html.RSTACQuery <- function(obj, ...) {

  #TODO: HTML representation
}

before_request.RSTACQuery <- function(s) {

  check_query_verb(s, "")
}

after_response.RSTACQuery <- function(s) {

  check_query_verb(s, "")
}

get_endpoint.RSTACQuery <- function(s) {

  .error("No endpoint was defined for the extension `%s`.", class(s)[[1]])
}
