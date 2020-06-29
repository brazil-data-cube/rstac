
.make_url <- function(url, endpoint = "/stac", params = list()) {

  res <- url

  if (length(endpoint) > 0)
    res <- paste0(res, paste0(endpoint, collapse = "/"))

  if (length(params) > 0) {

    params <- paste(mapply(function(k, v) {
      paste(k, paste(v, collapse = ","), sep = "=")
    }, names(params), params, SIMPLIFY = TRUE), collapse = "&")

    res <- paste(res, params, sep = "?")
  }

  return(res)
}
