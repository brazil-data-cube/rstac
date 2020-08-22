#' @title Endpoint functions
#'
#' @description (This document is based on STAC specification documentation
#' \url{https://github.com/radiantearth/stac-spec/blob/v0.8.1/api-spec/STAC.yaml}
#' and reproduces some of its parts)
#'
#' The \code{stac_search} function implements \code{/stac/search} API endpoint
#' (v0.8.1). It prepares query parameters used in search API request, a
#' \code{stac} object with all filter parameters to be provided to
#' \code{get_request} or \code{post_request} functions. The GeoJSON content
#' returned by these requests is a \code{stac_items} object, a regular R
#' \code{list} representing a STAC Item Collection document.
#'
#' @param s           a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{collections},
#' or \code{items} functions.
#'
#' @param collections a \code{character} vector of collection IDs to include in
#' the search for items. Only items in one of the provided collections will be
#' searched.
#'
#' @param ids         a \code{character} vector with item IDs. All other filter
#' parameters that futher restrict the number of search results are ignored.
#'
#' @param datetime    a \code{character} with a date-time or an interval. Date
#'  and time strings needs to conform RFC 3339. Intervals are expressed by
#'  separating two date-time strings by \code{'/'} character. Open intervals are
#'  expressed by using \code{'..'} in place of date-time.
#'
#' Examples: \itemize{
#'  \item A date-time: \code{"2018-02-12T23:20:50Z"}
#'  \item A closed interval: \code{"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"}
#'  \item Open intervals: \code{"2018-02-12T00:00:00Z/.."} or
#'  \code{"../2018-03-18T12:31:12Z"} }
#'
#' Only features that have a \code{datetime} property that intersects the
#' interval or date-time informed in \code{datetime} are selected.
#'
#' @param bbox        a \code{numeric} vector with only features that have a
#' geometry that intersects the bounding box are selected. The bounding box is
#' provided as four or six numbers, depending on whether the coordinate
#' reference system includes a vertical axis (elevation or depth):
#' \itemize{ \item Lower left corner, coordinate axis 1
#'           \item Lower left corner, coordinate axis 2
#'           \item Lower left corner, coordinate axis 3 (optional)
#'           \item Upper right corner, coordinate axis 1
#'           \item Upper right corner, coordinate axis 2
#'           \item Upper right corner, coordinate axis 3 (optional) }
#'
#' The coordinate reference system of the values is WGS84 longitude/latitude
#' (\url{http://www.opengis.net/def/crs/OGC/1.3/CRS84}). The values are in
#' most cases the sequence of minimum longitude, minimum latitude, maximum
#' longitude and maximum latitude. However, in cases where the box spans the
#' antimeridian the first value (west-most box edge) is larger than the third
#' value (east-most box edge).
#'
#' @param intersects  a \code{character} value expressing GeoJSON geometries
#' objects as specified in RFC 7946. Only returns items that intersect with
#' the provided polygon.
#'
#' @param limit       an \code{integer} defining the maximum number of results
#' to return. If not informed it defaults to the service implementation.
#'
#' @param ...         any additional non standard filter parameter.
#'
#' @seealso \code{\link{stac}}, \code{\link{extension_query}},
#' \code{\link{get_request}}, \code{\link{post_request}}
#'
#' @return
#' A \code{stac} object containing all search field parameters to be provided
#' to STAC API web service.
#'
#' @examples
#' \dontrun{
#' # GET request
#' stac(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     stac_search(collections = "MOD13Q1", limit = 1) %>%
#'     get_request()
#'
#' # POST request
#' stac(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     stac_search(collections = "MOD13Q1",
#'         bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'     post_request()
#' }
#'
#' @export
stac_search <- function(s, collections, ids, bbox, datetime, intersects,
                        limit, ...) {

  # check s parameter
  if (!"search" %in% class(s))
    .check_obj(s, expected = "stac", exclusive = TRUE)

  params <- list()

  if (!missing(collections)) {

    if (length(collections) == 1 && !is.list(collections))
      collections <- list(collections)
    params[["collections"]] <- collections
  }

  if (!missing(ids)) {

    if (length(ids) == 1 && !is.list(ids)) ids <- list(ids)
    params[["ids"]] <- ids
  }

  if (!missing(datetime)) {

    .verify_datetime(datetime)
    params[["datetime"]] <- datetime
  }

  if (!missing(bbox)) {

    if (!length(bbox) %in% c(4, 6))
      .error("Param `bbox` must have 4 or 6 numbers, not %s.", length(bbox))
    params[["bbox"]] <- bbox
  }

  # TODO: validate polygon
  if (!missing(intersects)) {

    params[["intersects"]] <- intersects
  }

  if (!missing(limit) && !is.null(limit))
    params[["limit"]] <- as.integer(limit)

  if (!missing(...))
    params <- c(params, list(...))

  # TODO: how to provide support to other versions?
  content <- .build_stac(url = s$url,
                         endpoint = .stac_search_endpoint(s$version),
                         params = params,
                         subclass = "search",
                         base_stac = s)

  return(content)
}

params_get_request.search <- function(s) {

  if (!is.null(s$params[["intersects"]]))
    .error(paste0("Search param `intersects` is not supported by HTTP GET",
                  "method. Try use `post_request` method instead."))

  # process stac params
  params <- params_get_request.stac(s)
  return(params)
}

params_post_request.search <- function(s, enctype) {

  # process stac params
  params <- params_post_request.stac(s, enctype = enctype)
  return(params)
}

content_get_response.search <- function(s, res) {

  content <- structure(
    .check_response(res, "200", c("application/geo+json", "application/json")),
    stac = s,
    request = list(method = "get"),
    class = "stac_items")

  return(content)
}

content_post_response.search <- function(s, res, enctype) {

  # the same as GET response
  content <- content_get_response.search(s, res)
  content$request <- list(method = "post", enctype = enctype)
  return(content)
}
