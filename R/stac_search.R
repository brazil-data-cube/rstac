

#' @title stac functions
#'
#' @author Rolf Simoes
#'
#' @description This function implements \code{/stac/search} API
#' endpoint (v0.8.0). It retrieves items using search parameters criteria.
#'
#' (This document is based on STAC specification documentation
#' \url{https://stacspec.org/})
#'
#' @param url         A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param collections A \code{character} vector of collection IDs to include in
#' the search for items. Only items in one of the provided collections will be
#' searched.
#'
#' @param ids         A \code{character} vector of item IDs. All other filter
#' parameters that futher restrict the number of search results
#' (except \code{.next} and \code{.limit}) are ignored.
#'
#' @param datetime    Either a date-time or an interval, open or closed.
#' Date and time expressions adhere to RFC 3339. Open intervals are
#' expressed using double-dots.
#' Examples:
#' \itemize{
#'   \item A date-time: \code{"2018-02-12T23:20:50Z"}
#'   \item A closed interval: \code{"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"}
#'   \item Open intervals: \code{"2018-02-12T00:00:00Z/.."} or
#'     \code{"../2018-03-18T12:31:12Z"}
#' }
#'
#' Only features that have a temporal property that intersects the value of
#' \code{datetime} are selected.
#'
#' If a feature has multiple temporal properties, it is the decision of the
#' server whether only a single temporal property is used to determine
#' the extent or all relevant temporal properties.
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
#' longitude/latitude (\url{http://www.opengis.net/def/crs/OGC/1.3/CRS84})
#' unless a different coordinate reference system is specified in the
#' parameter \code{bbox-crs}.
#'
#' For WGS84 longitude/latitude the values are in most cases the sequence
#' of minimum longitude, minimum latitude, maximum longitude and maximum
#' latitude. However, in cases where the box spans the antimeridian the
#' first value (west-most box edge) is larger than the third value
#' (east-most box edge).
#'
#' If a feature has multiple spatial geometry properties, it is the
#' decision of the server whether only a single spatial geometry property
#' is used to determine the extent or all relevant geometries.
#'
#' @param ...         Any additional non standard filter parameter.
#'
#' @param .limit      An \code{integer} defining the maximum number of results
#' to return. Defaults to 10.
#'
#' @param .next       An \code{integer} informing which set of results
#' to return. Values less than 1 means all pages will be retrieved.
#'
#' @param .headers    A \code{list} of named arguments to be passed as
#' http request headers.
#'
#' @return
#' A STAC item collection.
#'
#' @examples
#' \dontrun{
#'
#' stac_search(url = "http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'      collections = "MOD13Q1",
#'      bbox = c(-55.16335, -4.26325, -49.31739, -1.18355))
#' }
#'
#' @export
stac_search <- function(url, collections, ids, bbox, datetime, ...,
                        .limit = 10, .next = 1, .headers = list()) {

  params <- list()

  if (!missing(collections))
    params[["collections"]] <- collections

  if (!missing(ids))
    params[["ids"]] <- ids

  # TODO check valid datetime & interval
  if (!missing(datetime))
    params[["datetime"]] <- datetime

  # TODO check valid bbox
  if (!missing(bbox))
    params[["bbox"]] <- bbox

  if (!missing(...))
    params <- c(params, list(...))

  if (!missing(.limit))
    params["limit"] <- .limit

  if (!missing(.next))
    params["next"] <- .next

  # TODO check valid stac response
  res <- .stac_get(url = url,
                   endpoint = "/stac/search",
                   params = params,
                   headers = .headers)
  if (is.null(res))
    return(invisible(NULL))

  return(res)
}

################################################################################
#'@description a new function
#'
#'@export
stac_search_new <- function(url, query = list(), .headers = list()) {

  # Creating a base url
  base_url <- crul::HttpClient$new(url     = url,
                                   headers = .headers)
  # query adjustment
  query_redefined <- .query_format(query)

  # making a get request
  res <- base_url$get(query = query_redefined)
  cat(res$url)
  # Verify status code
  if(res$status_code > 203){
    stop(paste(res$status_http()[2]$message, ". \nStatus code =", res$status_code),
         call. = FALSE)
  }

  # verifying the output type of API
  stopifnot(res$response_headers$`content-type` == 'application/json')

  # Parsing res file
  parsed_res <- res$parse("UTF-8")
  content    <- jsonlite::fromJSON(parsed_res, flatten = TRUE)

  return(content)
}



