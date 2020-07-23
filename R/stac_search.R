#' @title Endpoint functions
#'
#' @author Rolf Simoes
#'
#' @description
#' (This document is based on STAC specification documentation
#' \url{https://github.com/radiantearth/stac-spec/blob/v0.8.0/api-spec/STAC.yaml}
#' and reproduces some of its parts)
#'
#' The \code{stac_search} function implements \code{/stac/search} API
#' endpoint (v0.8.0). It prepares query parameters used in search API request,
#' a \code{stac} object with all filter parameters to be provided to
#' \code{stac_request}. The GeoJSON content returned by the \code{stac_request}
#' is a \code{stac_items} object, a regular R \code{list} representing
#' a STAC ItemCollection.
#'
#' @param url         A \code{character} informing the base url of a
#' STAC web service or any \code{stac} object containing \code{request}
#' property.
#'
#' @param collections A \code{character} vector of collection IDs to include in
#' the search for items. Only items in one of the provided collections will be
#' searched.
#'
#' @param ids         A \code{character} vector of item IDs. All other filter
#' parameters that futher restrict the number of search results
#' (except \code{.next} and \code{.limit}) are ignored.
#'
#' @param datetime    Either a date-time or an interval.
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
#' @note Param \code{intersects} is a \code{character} value expressing GeoJSON
#' geometries objects as specified in RFC 7946. This param is not supported in
#' current version.
#'
#' @param bbox        Only features that have a geometry that intersects the
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
#' @param intersects Only returns items that intersect with the provided
#' polygon.
#'
#' @param limit       An \code{integer} defining the maximum number of results
#' to return. If \code{NULL} it defaults to the service implementation.
#' Defaults to 10.
#'
#' @param ...         Any additional non standard filter parameter.
#'
#' @seealso
#' \code{\link{stac}}, \code{\link{stac_request}}
#'
#' @return
#' A \code{stac} object containing all request parameters to be
#' provided to \code{stac_request}.
#'
#' @examples
#' \dontrun{
#'
#' stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'             collections = "MOD13Q1",
#'             bbox = c(-55.16335, -4.26325, -49.31739, -1.18355)) %>%
#'     stac_request()
#' }
#'
#' @export
stac_search <- function(url, collections, ids, bbox, datetime, intersects,
                        limit = 10, ...) {

  params <- list()

  if (!missing(collections))
    params[["collections"]] <- .query_encode(collections)

  if (!missing(ids))
    params[["ids"]] <- .query_encode(ids)

  if (!missing(datetime)) {

    .verify_datetime(datetime)
    params[["datetime"]] <- datetime
  }

  if (!missing(bbox)) {

    if (!length(bbox) %in% c(4, 6))
      stop(sprintf("Param `bbox` must have 4 or 6 numbers, not %s.",
                   length(bbox)))
    params[["bbox"]] <- .query_encode(bbox)
  }

  # TODO: validate polygon
  if (!missing(intersects)) {

    params[["intersects"]] <- .query_encode(intersects)
  }

  if (!is.null(limit))
    params[["limit"]] <- limit

  if (!missing(...))
    params <- c(params, list(...))

  # TODO: follow specification strictly
  if (!is.null(params[["intersects"]])) {
    if ("bbox" %in% names(params)) {
      warning("Only one of either `intersects` or bbox should be specified.
              The `bbox` parameter will be ignored.", call. = FALSE)

      params[["bbox"]] <- NULL
    }

    # TODO: add these code excerpts bellow in different file
    expected <- list("post" =
                       list(enctypes = c("application/json"),
                            responses =
                              list("200" =
                                     list("application/geo+json" = "stac_items",
                                          "application/json" = "stac_items"))))
  } else{
    expected <- list("get" =
                       list(responses =
                              list("200" =
                                     list("application/geo+json" = "stac_items",
                                          "application/json" = "stac_items"))),
                     "post" =
                       list(enctypes = c("application/json"),
                            responses =
                              list("200" =
                                     list("application/geo+json" = "stac_items",
                                          "application/json" = "stac_items"))))
  }

  content <- structure(list(url = .make_url(url, endpoint = "/stac/search"),
                            params = params,
                            expected_responses = expected),
                       class = "stac")
  return(content)
}
