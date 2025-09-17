#' @title Utility functions
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
#'
#' @return A `numeric` with the bbox provided,or an error if the supplied
#'   `bbox` does not meet the specifications.
#'
#' @noRd
.parse_bbox <- function(bbox) {
  if (is.character(bbox))
    bbox <- strsplit(bbox, ",")[[1]]
  if (!length(bbox) %in% c(4, 6))
    .error("Param `bbox` must have 4 or 6 numbers, not %s.", length(bbox))
  if (length(bbox) == 4) {
    if (bbox[[2]] > bbox[[4]])
      bbox <- bbox[c(1, 4, 3, 2)]
  } else {
    if (bbox[[2]] > bbox[[5]])
      bbox <- bbox[c(1, 5, 3, 4, 2, 6)]
    if (bbox[[3]] > bbox[[6]])
      bbox <- bbox[c(1, 2, 6, 4, 5, 3)]
  }
  return(bbox)
}

#' @title Utility functions
#'
#' @param limit       an `integer` defining the maximum number of results
#' to return. If not informed it defaults to the service implementation.
#'
#' @return A `integer` with the limit provided,or an error if the supplied
#'  `limit` has a length different from 1.
#'
#' @noRd
.parse_limit <- function(limit) {
  if (length(limit) != 1)
    .error("Parameter `limit` must be a single value.")
  limit <- suppressWarnings(as.integer(limit))
  if (is.na(limit))
    .error("Param `limit` must be an integer.")
  return(limit_int)
}

#' @title Utility functions
#'
#' @param feature_id  a `character` with item id to be fetched.
#' Only works if the `collection_id` is informed. This is equivalent to
#' the endpoint \code{/collections/\{collectionId\}/items/\{featureId\}}.
#'
#' @return A `character` with the parameter provided,or an error if the
#'  supplied `feature_id` has a length different from 1.
#'
#' @noRd
.parse_feature_id <- function(feature_id) {
  if (length(feature_id) != 1)
    .error("Parameter `feature_id` must be a single value.")
  return(feature_id)
}

#' @title Utility functions
#'
#' @param collections a `character` vector of collection IDs to include in
#' the search for items. Only items in one of the provided collections will be
#' searched.
#'
#' @return A `list` of collections.
#'
#' @noRd
.parse_collections <- function(collections) {
  if (is.list(collections)) {
    for (col in collections)
      check_character(col, "Collection name must be a character value.")
  } else
    check_character(collections, "Collection name must be a character value.")
  if (is.character(collections) && length(collections) == 1)
    collections <- strsplit(collections, ",")[[1]]
  if (is.character(collections))
    collections <- as.list(collections)
  return(collections)
}

#' @title Utility functions
#'
#' @param ids a `character` vector with item IDs. All other filter
#' parameters that further restrict the number of search results are ignored.
#'
#' @return A `list` with the ids.
#'
#' @noRd
.parse_ids <- function(ids) {
  if (is.list(ids)) {
    ids <- lapply(ids, function(id) {
      if (is.numeric(id))
        return(paste(id))
      check_character(id, "Item id must be a character value.")
      return(id)
    })
  } else if (is.numeric(ids)) {
    ids <- as.list(paste(ids))
  } else {
    check_character(ids, "Item id must be a character value.")
    if (length(ids) == 1)
      ids <- strsplit(ids, ",")[[1]]
    ids <- as.list(ids)
  }
  return(ids)
}

#' @title Utility functions
#'
#' @param intersects a `character` value expressing GeoJSON geometries
#' objects as specified in RFC 7946. Only returns items that intersect with
#' the provided polygon.
#'
#' @return A `character` with the validate polygon.
#'
#' @noRd
.parse_intersects <- function(intersects) {
  intersects <- get_spatial(intersects)
  if (!is.list(intersects))
    .error("Invalid GeoJSON object in `intersects` param.")
  return(intersects)
}

#' @title Utility functions
#'
#' @description Auxiliary function to check whether the date time follows
#' RFC 3339 standard.
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
#' @return An error if the date does not follow the specified standards or the
#'  date time provided as `character`.
#'
#' @noRd
.parse_datetime <- function(datetime) {
  # check if the date time provided is an open interval
  check_interval <-
    grepl("(?=^(\\..\\/.*)).+|(.*/\\..)", datetime, perl = TRUE)
  if (check_interval) {
    # regex to separate the open interval elements
    split_datetime <- strsplit(datetime, "(\\/\\..)|(\\..\\/)", perl = TRUE)
    split_datetime <- split_datetime[[1]][which(unlist(split_datetime) != "")]
    # checking if date time is in the RFC standards
    match_rfc <- .check_rfc_3339(split_datetime)
    if (!match_rfc)
      .error(paste("The interval date time provided is not in RFC format,",
                   "please check the RFC 3339 rules."))
    return(datetime)
  } else {
    # Splits the vector elements with the dates by the backslash
    split_datetime <- strsplit(datetime, "/", perl = TRUE)
    split_datetime <- unlist(split_datetime)
    # In case the vector has two elements it is a closed date time
    if (length(split_datetime) == 2) {
      # Checks if there is FALSE value in vector
      if (!all(.check_rfc_3339(split_datetime)))
        .error(paste0("The date time provided not follow the RFC 3339 format,",
                      "please check the RFC 3339 rules."))
      # formatting the closed date time according to the RFC
      interval_dt <- as.POSIXct(split_datetime,
                                tz = "UTC",
                                tryFormats = c("%Y-%m-%dT%H:%M:%SZ",
                                               "%Y-%m-%d"))
      # Check the interval, if the interval is wrong an error is returned
      if (interval_dt[1] > interval_dt[2]) {
        .error(paste("The closed date time provided is not in correct",
                     "interval, the first date time shold be less than",
                     "second."))
      }
      return(datetime)
    }
    else {
      # Check if date time is a fixed interval
      if (!all(.check_rfc_3339(split_datetime)) || length(split_datetime) != 1)
        .error(paste("The date time provided not follow the RFC 3339 format,",
                     "please check the RFC 3339 rules."))
      return(datetime)
    }
  }
}
