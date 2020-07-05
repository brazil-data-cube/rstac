#' @description
#'
#' @name
#'
#' @param
#'
#' @param
#'
#' @param
#'
#' @param
#'
#' @author
#'
.make_url <- function(url, endpoint = "/stac", params = list()) {

  # TODO: refactor
  browser()

  res <- url

  if (length(endpoint) > 0)
    res <- paste0(res, paste0(endpoint, collapse = "/"))

  if (length(params) > 0) {

    #params_refac <- paste(names(params), params, sep="=", collapse = "&")

    params <- paste(mapply(function(k, v) {
      paste(k, paste(v, collapse = ","), sep = "=")
    }, names(params), params, SIMPLIFY = TRUE), collapse = "&")

    res <- paste(res, params, sep = "?")
  }

  return(res)
}
