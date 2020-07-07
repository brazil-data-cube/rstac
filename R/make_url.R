#' @title make url
#'
#' @author Rolf Simoes
#'
#' @description This function
#'
#' @param url      A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param endpoint A \code{character} ..
#'
#' @param params A \code{list} ...
#'
#' @return A url ...
.make_url <- function(url, endpoint = "/stac", params = list()) {

  res <- url

  if (length(endpoint) > 0)
    res <- paste0(res, paste0(endpoint, collapse = "/"))

  if (length(params) > 0) {

    #params_refac <- paste0(names(params), params, sep="=", collapse = "&")

    params <- paste(mapply(function(k, v) {
      paste(k, paste(v, collapse = ","), sep = "=")
    }, names(params), params, SIMPLIFY = TRUE), collapse = "&")

    res <- paste(res, params, sep = "?")
  }
  cat(res)
  return(res)
}
