#' CQL2 helper function
#'
#' Function to convert bounding box (bbox) to a GeoJSON object
#' to be used as argument of CQL2 spatial operators.
#'
#' @param bbox  A `numeric` containing a bbox with c(xmin, ymin, xmax, ymax).
#'
#' @return GeoJSON object
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#'
#' @export
cql2_bbox_as_geojson <- function(bbox) {
  # bbox = c(xmin, ymin, xmax, ymax)
  list(
    type = "Polygon",
    coordinates = list(
      rbind(
        c(bbox[[1]], bbox[[2]]),
        c(bbox[[3]], bbox[[2]]),
        c(bbox[[3]], bbox[[4]]),
        c(bbox[[1]], bbox[[4]]),
        c(bbox[[1]], bbox[[2]])
      )
    )
  )
}

#' CQL2 temporal types
#'
#' These are helper functions to create temporal literal values to be
#' passed into CQL2 expressions.
#'
#' @param x,start,end `character` string containing valid `date` or `timestamp`.
#'
#' @return CQL2 temporal value
#'
#' @name cql2_temporal
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#'
NULL

#' @rdname cql2_temporal
#'
#' @export
cql2_date <- function(x) {
  stopifnot(is_date(x))
  unquote(quote(date({{x}})), environment())
}

#' @rdname cql2_temporal
#'
#' @export
cql2_timestamp <- function(x) {
  stopifnot(is_time(x))
  unquote(quote(timestamp({{x}})), environment())
}

#' @rdname cql2_temporal
#'
#' @export
cql2_interval <- function(start = "..", end = "..") {
  if (start != "..")
    stopifnot(is_instant_param(start))
  if (end != "..")
    stopifnot(is_instant_param(end))
  unquote(quote(interval({{start}}, {{end}})), environment())
}
