#' @title Utility functions
#'
#' @description Auxiliary function to check that the provided date time follows
#' the standards of RFC 3339
#'
#' @param datetime Either a date-time or an interval, open or closed.
#' Date and time expressions adhere to RFC 3339. Open intervals are
#' expressed using double-dots.
#' Examples:
#' \itemize{
#'   \item A date-time: `"2018-02-12T23:20:50Z"`
#'   \item A closed interval: `"2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"`
#'   \item Open intervals: `"2018-02-12T00:00:00Z/.."` or
#'     `"../2018-03-18T12:31:12Z"`
#' }
#'
#' @return A `logical` if TRUE the date time provided is correct,
#' otherwise not.
#'
#' @noRd
.check_rfc_3339 <- function(datetime) {
  # Standard regexp of RFC 3339
  pattern_rfc   <- "^\\d{4}-\\d{2}-\\d{2}?(T\\d{2}:\\d{2}:\\d{2}Z)?$"
  check_pattern <- grepl(pattern_rfc, datetime, perl = TRUE)
  return(check_pattern)
}

check_link <- function(link) {
  if (!is.list(link) || is.null(names(link)))
    .error("Invalid doc_link object.")
  if (!"href" %in% names(link))
    .error("Invalid doc_link object. Expecting `href` key.")
  link
}

check_item <- function(items) {
  if (!is.list(items) || is.null(names(items)))
    .error("Invalid doc_item object.")
  if (!"type" %in% names(items) || items$type != "Feature")
    .error("Invalid doc_item object. Expecting 'type': 'Feature' key value.")
  if (!"geometry" %in% names(items))
    .error("Invalid doc_item object. Expecting `geometry` key.")
  if (!"properties" %in% names(items))
    .error("Invalid doc_item object. Expecting `properties` key")
  items
}

check_items <- function(items) {
  if (!is.list(items) || is.null(names(items)))
    .error("Invalid doc_items object.")
  if (!"type" %in% names(items) || items$type != "FeatureCollection")
    .error("Invalid doc_items object. Expecting ",
           "'type': 'FeatureCollection' key value.")
  if (!"features" %in% names(items))
    .error("Invalid doc_items object. Expecting `features` key")
  items
}

check_catalog <- function(catalog) {
  if (!is.list(catalog) || is.null(names(catalog)))
    .error("Invalid doc_catalog object.")
  if (!"links" %in% names(catalog))
    .error("Invalid doc_catalog object. Expecting `links` key.")
  catalog
}

check_collection <- function(collection) {
  if (!is.list(collection) || is.null(names(collection)))
    .error("Invalid doc_collection object.")
  if (!"id" %in% names(collection))
    .error("Invalid doc_collection object. Expecting `id` key.")
  if (!"links" %in% names(collection))
    .error("Invalid doc_collection object. Expecting `links` key.")
  collection
}

check_character <- function(x, msg, ...) {
  if (!is.character(x))
    .error(msg, ...)
}
