#' @title Endpoint functions
#'
#' @description (This document is based on STAC specification documentation
#' <https://github.com/radiantearth/stac-spec/>
#' and reproduces some of its parts)
#'
#' The `stac_search` function implements `/stac/search` API endpoint
#' (v0.8.1) and `/search` (v0.9.0 or v1.0.0).
#' It prepares query parameters used in the search API request, a
#' `stac` object with all filter parameters to be provided to
#' `get_request` or `post_request` functions. The GeoJSON content
#' returned by these requests is a `STACItemCollection` object, a regular R
#' `list` representing a STAC Item Collection document.
#'
#' @param q           a `RSTACQuery` object expressing a STAC query
#' criteria.
#'
#' @param collections a `character` vector of collection IDs to include in
#' the search for items. Only items in one of the provided collections will be
#' searched.
#'
#' @param ids         a `character` vector with item IDs. All other filters
#' parameters that further restrict the number of search results are ignored.
#'
#' @param datetime    a `character` with a date-time or an interval. Date
#'  and time strings needs to conform to RFC 3339. Intervals are expressed by
#'  separating two date-time strings by `'/'` character. Open intervals are
#'  expressed by using `'..'` in place of date-time.
#'
#' Examples:
#' \itemize{
#' \item A date-time: `"2018-02-12T23:20:50Z"`
#' \item A closed interval: `"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"`
#' \item Open intervals: `"2018-02-12T00:00:00Z/.."` or
#'   `"../2018-03-18T12:31:12Z"`
#' }
#'
#' Only features that have a `datetime` property that intersects the
#' interval or date-time informed in `datetime` are selected.
#'
#' @param bbox        a `numeric` vector with only features that have a
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
#' (<http://www.opengis.net/def/crs/OGC/1.3/CRS84>). The values are, in
#' most cases, the sequence of minimum longitude, minimum latitude, maximum
#' longitude, and maximum latitude. However, in cases where the box spans the
#' antimeridian, the first value (west-most box edge) is larger than the third
#' value (east-most box edge).
#'
#' @param intersects  a `list` expressing GeoJSON geometries
#' objects as specified in RFC 7946. Only returns items that intersect with
#' the provided geometry. To turn a GeoJSON into a list the packages
#' `geojsonsf` or `jsonlite` can be used.
#'
#' @param limit       an `integer` defining the maximum number of results
#' to return. If not informed, it defaults to the service implementation.
#'
#' @seealso [stac()], [ext_query()],
#' [get_request()], [post_request()]
#'
#' @return
#' A `RSTACQuery` object with the subclass `search` containing all
#' search field parameters to be provided to STAC API web service.
#'
#' @examples
#' \dontrun{
#'  # GET request
#'  stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4-16D-2", limit = 10,
#'          datetime = "2017-08-01/2018-03-01") %>%
#'   get_request()
#'
#'  # POST request
#'  stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4-16D-2",
#'          bbox = c(-47.02148, -17.35063, -42.53906, -12.98314)) %>%
#'   post_request()
#' }
#'
#' @export
stac_search <- function(q,
                        collections = NULL,
                        ids = NULL,
                        bbox = NULL,
                        datetime = NULL,
                        intersects = NULL,
                        limit = NULL) {

  # check q parameter
  check_subclass(q, c("stac", "search"))

  params <- list()

  if (!is.null(collections))
    params[["collections"]] <- .parse_collections(collections)

  if (!is.null(ids))
    params[["ids"]] <- .parse_ids(ids)

  if (!is.null(datetime))
    params[["datetime"]] <- .parse_datetime(datetime)

  if (!is.null(bbox))
    params[["bbox"]] <- .parse_bbox(bbox)

  if (!is.null(intersects))
    params[["intersects"]] <- .parse_intersects(intersects)

  if (!is.null(limit))
    params[["limit"]] <- .parse_limit(limit)

  RSTACQuery(
    version = q$version,
    base_url = q$base_url,
    params = utils::modifyList(q$params, params),
    subclass = "search"
  )
}

#' @export
parse_params.search <- function(q, params) {

  if (!is.null(params[["collections"]]))
    params[["collections"]] <- .parse_collections(params[["collections"]])

  if (!is.null(params[["ids"]]))
    params[["ids"]] <- .parse_ids(params[["ids"]])

  if (!is.null(params[["datetime"]]))
    params[["datetime"]] <- .parse_datetime(params[["datetime"]])

  if (!is.null(params[["bbox"]]))
    params[["bbox"]] <- .parse_bbox(params[["bbox"]])

  if (!is.null(params[["intersects"]]))
    params[["intersects"]] <- .parse_intersects(params[["intersects"]])

  if (!is.null(params[["limit"]]))
    params[["limit"]] <- .parse_limit(params[["limit"]])

  params
}

#' @export
endpoint.search <- function(q) {

  if (q$version < "0.9.0")
    return("/stac/search")
  return("/search")
}

#' @export
before_request.search <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  if (!is.null(q$params[["intersects"]]) && q$verb == "GET")
    .error(paste0("Search param `intersects` is not supported by HTTP GET",
                  "method. Try use `post_request` method instead."))

  return(q)
}

#' @export
after_response.search <- function(q, res) {
  after_response.items(q, res)
}
