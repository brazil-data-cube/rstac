#' @title STAC utils
#'
#' @author Rolf Simoes
#'
#' @description The \code{.check_response} function that checks if the request's
#' response is in accordance with the \code{expected} parameters.
#'
#' @param res  a \code{httr} \code{response} object.
#'
#' @param expected a \code{list} containing the expected parameters values.
#'
#' @return a \code{character} with document class
.check_response <- function(res, expected) {

  method <- expected[[tolower(res$request$method)]]
  if (is.null(method))
    .error("HTTP method '%s' not defined for this operation.", res$method)

  # TODO: validate stac response
  # .stac_response_type(res, expected)

  status_code <- method$responses[[as.character(res$status_code)]]
  if (is.null(status_code)) {
    content <- httr::content(res)
    if (!is.null(content$code))
      .error("%s %s", content$code, content$description)
    .error("HTTP status '%s' not defined for this operation.",
           res$status_code)
  }
  content_type  <- httr::http_type(res)
  content_class <- status_code[[content_type]]
  if (is.null(content_class))
    .error("HTTP content type response '%s' not defined for this operation.",
           content_type)

  if (content_class == "")
    content_class <- NULL

  return(content_class)
}


