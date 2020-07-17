#' @title HTTP request
#'
#' @name http_request
#'
#' @author Rolf Simoes
#'
#' @description \code{.get_request} is a function that generates HTTP
#' GET requests. The object returned includes the following fields:
#' \itemize{
#'   \item \code{url}: \code{character} containing requested url
#'   \item \code{status_code}: \code{integer} indicating HTTP response status
#'   \item \code{type}: \code{character} informing response content type
#'   \item \code{headers}: \code{character} vector with all HTTP response
#'   headers
#'   \item \code{content}: \code{list} object representing JSON document
#' }
#' Calling rountines must parse this object to further return the result to
#' the user or raise error depending on the response content.
#'
#' @param s        A \code{stac} object expressing a STAC search criteria.
#'
#' @param headers  A named \code{list} informing HTTP request headers to be
#' passed to \code{curl} package request functions.
#'
#' @return
#' A regular \code{list} object containing all HTTP parsed responses
#' returned by \code{curl} package.
#'
.get_request <- function(s, headers = list()) {

  if (!inherits(s, "stac"))
    stop(sprintf("Invalid `stac` object."), call. = FALSE)

  url <- .make_url(url = s$url, endpoint = s$endpoint, params = s$params)

  tryCatch({
    h <- curl::new_handle()
    curl::handle_setheaders(h, .list = headers)

    res <- curl::curl_fetch_memory(url = url, handle = h)
  },
  error = function(e) {

    stop(paste("Request error.", e$message), call. = FALSE)
  })

  res$content <- rawToChar(res$content)

  # TODO: check content type response
  if (!jsonlite::validate(res$content))
    stop("Invalid JSON response.", call. = FALSE)

  res$content <- jsonlite::fromJSON(res$content,
                                    simplifyVector = TRUE,
                                    simplifyDataFrame = FALSE,
                                    simplifyMatrix = FALSE)

  res$headers <- curl::parse_headers(res$headers)

  res$method <- "get"

  return(res)
}

#' @rdname http_request
#'
#' @param enctype   A \code{character} informing the request body Content-Type.
#' Accepted types \code{'application/json'},
#' \code{'application/x-www-form-urlencoded'}, and
#' \code{'multipart/form-data'}
#'
#' @description \code{.post_request} is a function that generates HTTP POST
#' requests. The object returned includes the same field of \code{.get_request}.
#'
.post_request <- function(s, enctype, headers = list()) {

  if (!inherits(s, "stac"))
    stop(sprintf("Invalid `stac` object."), call. = FALSE)

  h <- curl::new_handle()
  curl::handle_setheaders(h, .list = headers)

  enctype <- enctype[[1]]
  if (enctype == "application/json") {

    curl::handle_setheaders(h, "Content-Type" = enctype)
    curl::handle_setopt(h, copypostfields =
                          jsonlite::toJSON(s$params, auto_unbox = TRUE))
  } else if (enctype == "application/x-www-form-urlencoded") {

    curl::handle_setheaders(h, "Content-Type" = enctype)
    curl::handle_setopt(h, copypostfields = .query_encode(s$params))
  } else if (enctype == "multipart/form-data") {

    curl::handle_setform(h, .list  = lapply(s$params, .query_encode))
  }

  url <- .make_url(url = s$url, endpoint = s$endpoint)

  tryCatch({
    res <- curl::curl_fetch_memory(url = url, handle = h)
  },
  error = function(e) {

    stop(paste("Request error.", e$message), call. = FALSE)
  })

  # stopifnot(res$response_headers$`content-type` %in% c('application/json',
  #                                                      application/geo+json')

  res$content <- rawToChar(res$content)

  # TODO: check content type response
  if (!jsonlite::validate(res$content))
    stop("Invalid JSON response.", call. = FALSE)

  res$content <- jsonlite::fromJSON(res$content,
                                    simplifyVector = TRUE,
                                    simplifyDataFrame = FALSE,
                                    simplifyMatrix = FALSE)

  res$headers <- curl::parse_headers(res$headers)

  res$method <- "post"

  return(res)
}

#' @rdname http_request
#'
#' @description
#' \code{.make_url} is a helper function to generate url. The returned
#' url is formed by appending \code{endpoint} at the end of base url
#' informed by \code{url} parameter. If \code{endpoint} has multiple elements
#' it will be collapsed using \code{'/'} character.
#'
#' Note that \code{.make_url} function differs from standards of relative URI
#' path resolution (RFC 3986). Any existing path in base url
#' is maintained in the final url, and a simple string contatenation is made
#' whithout including any character separator. For this reason, this function
#' does not support the query and fragment URI components in the base url.
#'
#' @param url         A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param endpoint    A \code{character} a path to be appended in the final
#' url.
#'
#' @param params      A \code{list} with all url query parameters to be
#' appended in final url.
#'
#' @return
#' \code{.make_url} returns an url to access STAC endpoints.
#'
.make_url <- function(url, endpoint = "", params = list()) {

  endpoint <- paste0(endpoint, collapse = "/")

  # TODO: URI resolution for previous existing query and fragment URI components
  # in informed url.
  res <- paste0(url, endpoint)

  if (length(params) > 0) {

    if (is.null(names(params)))
      stop("URL query values must be named.", call. = FALSE)
    params <- .query_encode(params)
    res <- paste(res, params, sep = "?")
  }

  return(res)
}


.query_encode <- function(params) {

  if (!is.null(names(params)))
    return(paste(names(params),
                 sapply(unname(params), paste0, collapse = ","),
                 sep = "=", collapse = "&"))
  return(paste0(params, collapse = ","))
}
