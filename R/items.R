#' @title Endpoint functions
#'
#' @author Rolf Simoes
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
#' @param item_id     a \code{character} with item id to be fetched.
#' Only works if the \code{collection_id} is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{itemId\}}.
#'
#' @param datetime    either a date-time or an interval.
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
#' @param bbox        only features that have a geometry that intersects the
#' bounding box are selected. The bounding box is provided as four or six
#' numbers, depending on whether the coordinate reference system includes a
#' vertical axis (elevation or depth):
#' \itemize{
#'   \item Lower left corner, coordinate axis 1
#'   \item Lower left corner, coordinate axis 2
#'   \item Lower left corner, coordinate axis 3 (optional)
#'   \item Upper right corner, coordinate axis 1
#'   \item Upper right corner, coordinate axis 2
#'   \item Upper right corner, coordinate axis 3 (optional)
#' }
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
#' A \code{stac} object containing all request parameters to be
#' provided to \code{stac_request}.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
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
items <- function(s, item_id, datetime, bbox, limit, ...) {

  # check s parameter
  .check_obj(s, "stac")

  # check mutator
  .check_mutator(s, c("collections", "items"))


  if (is.null(s$params[["collection_id"]]) && s$mutator == "collections")
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

  if (missing(item_id)) {

    # TODO: follow specification strictly
    endpoint <- s$endpoint
    if (s$mutator == "collections")
      endpoint <- paste("/collections", s$params[["collection_id"]], "items",
                        sep = "/")

    # TODO: add these code excerpts bellow in different file
    expected <- list("get" =
                       list(responses =
                              list("200" =
                                     list("application/geo+json" = "stac_items",
                                          "application/json" = "stac_items"))),
                     "post" =
                       list(enctypes = c("application/x-www-form-urlencoded",
                                         "multipart/form-data"),
                            responses =
                              list("200" =
                                     list("application/geo+json" = "stac_items",
                                          "application/json" = "stac_items"))))
  } else {

    # TODO: follow specification strictly
    endpoint <- s$endpoint
    if (s$mutator == "collections")
      endpoint <- paste("/collections", s$params[["collection_id"]], "items",
                        item_id, sep = "/")

    # TODO: add these code excerpts bellow in different file
    # TODO: this could be returned by the STAC service
    expected <- list("get" =
                       list(responses =
                              list("200" =
                                     list("application/geo+json" = "stac_item",
                                          "application/json" = "stac_item"))),
                     "post" =
                       list(enctypes = c("application/x-www-form-urlencoded",
                                         "multipart/form-data"),
                            responses =
                              list("200" =
                                     list("application/geo+json" = "stac_item",
                                          "application/json" = "stac_item"))))
    params <- list()
  }

  s$params[["collection_id"]] <- NULL

  content <- build_stac(url = s$url,
                        endpoint = endpoint,
                        params = params,
                        expected_responses = expected,
                        mutator = "items",
                        old_stac = s)

  return(content)
}

#' @export
`[[.stac_items` <- function(x, i){

  result <- x$features[[i]]
  class(result) <- "stac_item"

  return(result)
}

#' @export
`[.stac_items` <- function(x, i){

  x$features <- x$features[i]

  return(x)
}
