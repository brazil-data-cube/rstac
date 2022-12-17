#' @title Utility functions
#'
#' @rdname http_request
#'
#' @description
#' `make_url` is a helper function to generate url. The returned
#' url is formed by appending `endpoint` at the end of base url
#' informed by `url` parameter. If `endpoint` has multiple elements
#' it will be collapsed using `'/'` character.
#'
#' Note that `make_url` function differs from standards of relative URI
#' path resolution (RFC 3986). Any existing path in base url
#' is maintained in the final url, and a simple string contatenation is made
#' whithout including any character separator. For this reason, this function
#' does not support the query and fragment URI components in the base url.
#'
#' @param url         a `character` informing the base url of a
#' STAC web service.
#'
#' @param endpoint    a `character` a path to be appended in the final
#' url.
#'
#' @param params      a named `list` with all url query parameters to be
#' appended in the url.
#'
#' @return
#' `make_url` returns an url to access STAC endpoints.
#'
#' @noRd
make_url <- function(url, endpoint = "", params = list()) {
  # remove trailing '/' char
  if (substring(url, nchar(url)) == "/")
    url <- substring(url, 1, nchar(url) - 1)

  endpoint <- paste0(endpoint, collapse = "/")

  res <- paste0(url, endpoint)

  return(res)
}

#' @title Utility functions
#'
#' @param params a `list` of parameters received from stac objects.
#'
#' @return a `character` representing the encode parameters of the query.
#'
#' @noRd
.querystrings_encode <- function(params) {
  return(lapply(params, paste0, collapse = ","))
}

#' @title Utility functions
#'
#' @param querystring a `character` with the query to be decoded.
#'
#' @return a `list` with the query params.
#'
#' @noRd
.querystring_decode <- function(querystring) {
  # first decode and remove all coded spaces
  querystring <- URLdecode(querystring)
  querystring_spplited <- strsplit(querystring, split = "&")[[1]]
  # remove empty spaces
  querystring_spplited <- querystring_spplited[nzchar(querystring_spplited)]
  values <- lapply(querystring_spplited,
                   function(x) regmatches(x, regexpr("=", x), invert = TRUE)[[1]])


  params <- lapply(values, `[[`, 2)
  names(params) <- vapply(values, `[[`, 1, FUN.VALUE = character(1))

  return(params)
}

#' @title Utility functions
#'
#' @param params a `list` with the parameters of query.
#'
#' @return a `list` with the query parameters.
#'
#' @noRd
.validate_query <- function(params) {

  if (!is.null(params$query) && is.character(params$query)) {
    params$query <- jsonlite::fromJSON(params$query, simplifyVector = FALSE)

    if (is.list(params$query))
      params$query <- list(params$query)
  }

  return(params)
}

gdalvsi_schema <- function(url) {
  if (grepl("^(.+):.*$", url)) gsub("^(.+):.*$", "\\1", url)
}

gdalvsi_switch <- function(url, ...) {
  switch(gdalvsi_schema(url), ...)
}

gdalvsi_append <- function(url) {
  vapply(url, function(x) {
    gdalvsi_switch(
      x,
      https = , http = paste("/vsicurl", x, sep = "/"),
      s3 = paste("/vsis3", gsub("^s3://", "", x), sep = "/"),
      gs = paste("/vsigs", gsub("^gs://", "", x), sep = "/"),
      url
    )
  }, character(1), USE.NAMES = FALSE)
}

#' @title Utility functions
#'
#' @param bbox a `numeric` vector with only features that have a
#' geometry that intersects the bounding box are selected. The bounding box is
#' provided as four or six numbers, depending on whether the coordinate
#' reference system includes a vertical axis (elevation or depth):
#' \itemize{ \item Lower left corner, coordinate axis 1
#'           \item Lower left corner, coordinate axis 2
#'           \item Lower left corner, coordinate axis 3 (optional)
#'           \item Upper right corner, coordinate axis 1
#'           \item Upper right corner, coordinate axis 2
#'           \item Upper right corner, coordinate axis 3 (optional) }.
#'
#' @return A `character` with `bbox` formatted based on min and max
#'  values.
#'
#' @noRd
.format_bbox <- function(bbox) {

  if (!is.null(bbox) & length(bbox) == 4)
    return(paste(c("xmin:", "ymin:", "xmax:", "ymax:"),
                 sprintf("%.5f", bbox), collapse = ", "))

  if (!is.null(bbox) & length(bbox) == 6)
    return(paste(c("xmin:", "ymin:", "zmin:", "xmax:", "ymax:", "zmax:"),
                 sprintf("%.5f", bbox), collapse = ", "))
}
