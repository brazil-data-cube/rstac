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

  url <- .make_url(url = s$url, params = s$params)

  tryCatch({
    h <- curl::new_handle()
    curl::handle_setheaders(h, .list = headers)

    res <- curl::curl_fetch_memory(url = url, handle = h)
  },
  error = function(e) {

    stop(paste("Request error.", e$message), call. = FALSE)
  })

  res$content <- rawToChar(res$content)

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

  tryCatch({
    res <- curl::curl_fetch_memory(url = s$url, handle = h)
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

  res$method <- "post"

  return(res)
}
