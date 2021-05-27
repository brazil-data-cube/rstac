#' @title Endpoint functions
#'
#' @description
#' The \code{items} function implements WFS3
#' \code{/collections/\{collectionId\}/items}, and
#' \code{/collections/\{collectionId\}/items/\{featureId\}} endpoints.
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
#' @param q           a \code{RSTACQuery} object expressing a STAC query
#' criteria.
#'
#' @param feature_id  a \code{character} with item id to be fetched.
#' Only works if the \code{collection_id} is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{featureId\}}.
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
#' @seealso
#' \code{\link{get_request}},  \code{\link{post_request}},
#'  \code{\link{collections}}
#'
#' @return
#' A \code{RSTACQuery} object with the subclass \code{items} for
#'  \code{/collections/{collection_id}/items} endpoint, or a
#'  \code{item_id} subclass for
#'  \code{/collections/{collection_id}/items/{feature_id}} endpoint,
#'  containing all search field parameters to be provided to STAC API web
#'  service.
#'
#' @examples
#' \donttest{
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   collections("CB4_64_16D_STK-1") %>%
#'   items(bbox = c(-47.02148, -12.98314, -42.53906, -17.35063)) %>%
#'   get_request()
#'
#' stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   collections("CB4_64_16D_STK-1") %>%
#'   items("CB4_64_16D_STK_v001_022023_2020-07-11_2020-07-26") %>%
#'   get_request()
#' }
#'
#' @export
items <- function(q, feature_id = NULL,
                  datetime = NULL,
                  bbox = NULL,
                  limit = NULL) {

  # check q parameter
  check_subclass(q, c("collection_id", "items"))

  params <- list()

  if (!is.null(datetime))
    params[["datetime"]] <- .parse_datetime(datetime)

  if (!is.null(bbox))
    params[["bbox"]] <- .parse_bbox(bbox)

  if (!is.null(limit) && !is.null(limit))
    params[["limit"]] <- .parse_limit(limit)

  # set subclass
  subclass <- "items"
  if (!is.null(feature_id)) {

    params[["feature_id"]] <- .parse_feature_id(feature_id)

    subclass <- "item_id"
  }

  RSTACQuery(version = q$version,
             base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = subclass)
}

#' @export
endpoint.items <- function(q) {

  return(paste("/collections", q$params[["collection_id"]], "items", sep = "/"))
}

#' @export
before_request.items <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  # don't send 'collection_id' in url's query string or content body
  q <- omit_query_params(q, names = "collection_id")

  return(q)
}

#' @export
after_response.items <- function(q, res) {

  content <- content_response(res, "200", c("application/geo+json",
                                            "application/json"))

  RSTACDocument(content = content, q = q, subclass = "STACItemCollection")
}

#' @export
endpoint.item_id <- function(q) {

  return(paste("/collections", q$params[["collection_id"]], "items",
               q$params[["feature_id"]], sep = "/"))
}

#' @export
before_request.item_id <- function(q) {

  check_query_verb(q, verbs = c("GET", "POST"))

  # don't send 'collection_id' and 'feature_id' in
  # url's query string or content body
  q <- omit_query_params(q, names = c("collection_id", "feature_id"))

  return(q)
}

#' @export
after_response.item_id <- function(q, res) {

  content <- content_response(res, "200", c("application/geo+json",
                                            "application/json"))

  RSTACDocument(content = content, q = q, subclass = "STACItem")
}
