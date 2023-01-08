
#' @export
cql2_bbox <- function(bbox) {
  # bbox = c(xmin, ymin, xmax, ymax)
  list(
    type = "Polygon",
    coordinates = list(
      list(
        c(bbox[[1]], bbox[[2]]),
        c(bbox[[3]], bbox[[2]]),
        c(bbox[[3]], bbox[[4]]),
        c(bbox[[1]], bbox[[4]]),
        c(bbox[[1]], bbox[[2]])
      )
    )
  )
}

#' @export
cql2_date <- function(x) {
  stopifnot(is_date(x))
  unquote(quote(date({{x}})), environment())
}

#' @export
cql2_timestamp <- function(x) {
  stopifnot(is_time(x))
  unquote(quote(timestamp({{x}})), environment())
}

#' @export
cql2_interval <- function(start = "..", end = "..") {
  if (start != "..")
    stopifnot(is_instant_param(start))
  if (end != "..")
    stopifnot(is_instant_param(end))
  unquote(quote(interval({{start}}, {{end}})), environment())
}
