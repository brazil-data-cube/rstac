#' @title STAC utils
#'
#' @author Rolf Simoes
#'
#' @description The \code{stac_request} ...
#'
#' @param res ...
#'
#' @param expected ...
#'
#' @return ...
.check_response <- function(res, expected) {

  #browser()


  method <- expected[[res$method]]
  if (is.null(method))
    stop(sprintf("HTTP method '%s' not defined for this operation.", res$method),
         call. = FALSE)

  # TODO: validate stac response
  # .stac_response_type(res, expected)

  status_code <- method$responses[[as.character(res$status_code)]]
  if (is.null(status_code)) {
    if (!is.null(res$content$code))
      stop(sprintf("%s %s", res$content$code, res$content$description),
           call. = FALSE)
    stop(sprintf("HTTP status '%s' not defined for this operation.",
                 res$status_code), call. = FALSE)
  }

  content_class <- status_code[[res$type]]
  if (is.null(content_class))
    stop(sprintf("HTTP content type response '%s' not defined for this operation.",
                 res$type))

  if (content_class == "")
    content_class <- NULL

  return(content_class)
}
