#' `CQL2` helper function
#'
#' These are helper functions to easy construction `CQL2` expressions.
#' These functions are not meant to be used in expressions and they must
#' be escaped using `{{` to be evaluated before request.
#'
#' \itemize{
#' \item `cql2_bbox_as_geojson()`: used to convert bounding box (`bbox`) to a
#' `GeoJSON` object to be used as argument of `CQL2` spatial operators.
#'
#' \item `cql2_date()`, `cql2_timestamp()`, and `cql2_interval()`:
#' create temporal literal values to be passed into `CQL2` expressions.
#' }
#'
#'
#' @param bbox          a `numeric` containing a `bbox` with
#' `c(xmin, ymin, xmax, ymax)`.
#'
#' @param x,start,end   a `character` string containing valid `date` or
#' `timestamp`.
#'
#' @return
#' \itemize{
#' \item `cql2_bbox_as_geojson()`: GeoJSON object.
#'
#' \item `cql2_date()`, `cql2_timestamp()`, and `cql2_interval()`:
#' internal `rstac` expressions representing temporal values.
#' }
#'
#' @examples
#' \dontrun{
#' bbox <- c(-122.2751, 47.5469, -121.9613, 47.7458)
#'
#' cql2_json(
#'   collection == "landsat-c2-l2" &&
#'     t_intersects(datetime, {{ cql2_interval("2020-12-01", "2020-12-31") }}) &&
#'     s_intersects(geometry, {{ cql2_bbox_as_geojson(bbox) }})
#' )
#' }
#'
#' @name cql2_helpers
NULL

#' @rdname cql2_helpers
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

#' @rdname cql2_helpers
#'
#' @export
cql2_date <- function(x) {
  check_is_date(x)
  unquote(quote(date({{ x }})), environment())
}

#' @rdname cql2_helpers
#'
#' @export
cql2_timestamp <- function(x) {
  check_is_time(x)
  unquote(quote(timestamp({{ x }})), environment())
}

#' @rdname cql2_helpers
#'
#' @export
cql2_interval <- function(start = "..", end = "..") {
  if (start != "..") {
    check_is_instant_param(start)
  }
  if (end != "..") {
    check_is_instant_param(end)
  }
  unquote(quote(interval({{ start }}, {{ end }})), environment())
}
