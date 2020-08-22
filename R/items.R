#' @title Endpoint functions
#'
#' @description
#' The \code{items} function implements WFS3
#' \code{/collections/\{collectionId\}/items}, and
#' \code{/collections/\{collectionId\}/items/\{itemId\}}
#' endpoints (v0.8.0).
#'
#' Each endpoint retrieves specific STAC objects:
#' \itemize{
#'   \item \code{/collections/\{collectionId\}/items}: Returns a STAC Items
#'     collection (GeoJSON)
#'   \item \code{/collections/\{collectionId\}/items/\{itemId\}}: Returns a
#'     STAC Item (GeoJSON Feature)
#' }
#'
#' The endpoint \code{/collections/\{collectionId\}/items} accepts the same
#' filters parameters of \code{\link{stac_search}} function.
#'
#' @param s           a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{collections},
#' or \code{items} functions.
#'
#' @param feature_id  a \code{character} with item id to be fetched.
#' Only works if the \code{collection_id} is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{itemId\}}.
#'
#' @param datetime    a \code{character} with a date-time or an interval.
#' Date and time strings needs to conform RFC 3339. Intervals are
#' expressed by separating two date-time strings by \code{'/'} character.
#' Open intervals are expressed by using \code{'..'} in place of date-time.
#'
#' Examples:
#' \itemize{
#'   \item A date-time: \code{"2018-02-12T23:20:50Z"}
#'   \item A closed interval: \code{"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"}
#'   \item Open intervals: \code{"2018-02-12T00:00:00Z/.."} or
#'     \code{"../2018-03-18T12:31:12Z"}
#' }
#'
#' Only features that have a \code{datetime} property that intersects
#' the interval or date-time informed in \code{datetime} are selected.
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
#' The coordinate reference system of the values is WGS84
#' longitude/latitude (\url{http://www.opengis.net/def/crs/OGC/1.3/CRS84}).
#' The values are in most cases the sequence of minimum longitude,
#' minimum latitude, maximum longitude and maximum latitude. However,
#' in cases where the box spans the antimeridian the first value
#' (west-most box edge) is larger than the third value
#' (east-most box edge).
#'
#' @param limit       an \code{integer} defining the maximum number of results
#' to return. If not informed it defaults to the service implementation.
#'
#' @param ...         filter parameters. Accept the same filter parameters
#' of \code{\link{stac_search}} function.
#'
#' @seealso
#' \code{\link{get_request}},  \code{\link{post_request}},
#'  \code{\link{collections}}
#'
#' @return
#' A \code{stac} object containing all search field parameters to be provided
#' to STAC API web service.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/dev/bdc-stac/") %>%
#'   collections("MOD13Q1") %>%
#'   items(bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'   get_request()
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'   collections("MOD13Q1") %>%
#'   items("MOD13Q1.A2019241.h13v09.006.2019262164754") %>%
#'   get_request()
#' }
#'
#' @export
items <- function(s, feature_id, datetime, bbox, limit, ...) {

  # check s parameter
  .check_obj(s, expected = c("collections", "items"))

  if (is.null(s$params[["collection_id"]]))
    .error("Not informed `collection_id` parameter.")

  params <- list()

  if (!missing(datetime)) {

    .verify_datetime(datetime)
    params[["datetime"]] <- datetime
  }

  if (!missing(bbox)) {

    if (!length(bbox) %in% c(4, 6))
      .error("Param `bbox` must have 4 or 6 numbers, not %s.", length(bbox))
    params[["bbox"]] <- bbox
  }

  if (!missing(limit) && !is.null(limit))
    params[["limit"]] <- limit

  if (!missing(...))
    params <- c(params, list(...))

  endpoint <- .OAFeat_items_endpoint(collection_id = s$params[["collection_id"]])

  if (!missing(feature_id)) {

    if (length(feature_id) != 1)
      .error("Parameter `feature_id` must be a single value.")

    params[["feature_id"]] <- feature_id

    endpoint <- .OAFeat_items_endpoint(collection_id = s$params[["collection_id"]],
                                       feature_id = params[["feature_id"]])
  }

  content <- .build_stac(url = s$url,
                         endpoint = endpoint,
                         params = params,
                         subclass = "items",
                         base_stac = s)

  return(content)
}

params_get_request.items <- function(s) {

  if (!is.null(s$params[["feature_id"]]))
    return(list())

  # process collections params
  params <- params_get_request.collections(s)

  return(params)
}

params_post_request.items <- function(s, enctype) {

  if (!is.null(s$params[["feature_id"]]))
    return(list())

  # process collections params
  params <- params_get_request.collections(s)

  return(params)
}

content_get_response.items <- function(s, res) {

  # detect expected response object class
  content_class <- "stac_items"
  if (!is.null(s$params[["feature_id"]]))
    content_class <- "stac_item"

  content <- structure(
    .check_response(res, "200", c("application/geo+json", "application/json")),
    stac = s,
    request = list(method = "get"),
    class = content_class)

  return(content)
}

content_post_response.items <- function(s, res, enctype) {

  # detect expected response object class
  content_class <- "stac_items"
  if (!is.null(s$params[["feature_id"]]))
    content_class <- "stac_item"

  content <- structure(
    .check_response(res, "200", c("application/geo+json", "application/json")),
    stac = s,
    request = list(method = "post", enctype = enctype),
    class = content_class)

  return(content)
}

`[[.stac_items` <- function(x, i){

  x <- x$features[[i]]
  class(x) <- "stac_item"

  return(x)
}

`[.stac_items` <- function(x, i){

  x$features <- x$features[i]

  return(x)
}

# TODO: implement head and tail S3 methods for stac_items object
