
.parse_bbox <- function(bbox) {

  if (!length(bbox) %in% c(4, 6))
    .error("Param `bbox` must have 4 or 6 numbers, not %s.", length(bbox))

  return(bbox)
}

.parse_limit <- function(limit) {

  if (length(limit) != 1)
    .error("Parameter `limit` must be a single value.")

  limit <- as.character(limit)

  limit_int <- suppressWarnings(as.integer(limit))

  if (any(is.na(as.integer(limit))) || as.character(limit_int) != limit)
    .error("Param `limit` must be an integer.")

  return(limit)
}

.parse_feature_id <- function(feature_id) {

  if (length(feature_id) != 1)
    .error("Parameter `feature_id` must be a single value.")

  return(feature_id)
}

.parse_collections <- function(collections) {

  if (length(collections) == 1 && !is.list(collections))
    collections <- list(collections)

  return(collections)
}

.parse_ids <- function(ids) {

  if (length(ids) == 1 && !is.list(ids))
    ids <- list(ids)

  return(ids)
}

.parse_geometry <- function(geom) {

  # TODO: validate polygon
}

.parse_items_size <- function(items) {

  if (items_length(items) != items_matched(items))
    .message(paste("The length of items in your object, does not correspond",
                   "with the total of matched items. Consider using the",
                   "function `items_fetch()`. By default, items_max = %d"),
             items_length(items))

  return(items_length(items))
}


#' @title utils functions
#'
#' @description Auxiliary function to check whether the date time follows
#' RFC 3339 standard.
#'
#' @param datetime Either a date-time or an interval, open or closed.
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
#' @return An error if the date does not follow the specified standards or the
#'  date time provided as \code{character}.
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

    # Check if date time is a fixed interval
    else {
      if (!all(.check_rfc_3339(split_datetime)) || length(split_datetime) != 1)
        .error(paste("The date time provided not follow the RFC 3339 format,",
                     "please check the RFC 3339 rules."))

      return(datetime)
    }
  }
}

#' @title utils functions
#'
#' @description Auxiliary function to check that the provided date time follows
#' the standards of RFC 3339
#'
#' @param datetime Either a date-time or an interval, open or closed.
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
#' @return A \code{logical} if TRUE the date time provided is correct,
#' otherwise not.
#'
#' @noRd
.check_rfc_3339 <- function(datetime) {

  # Standard regexp of RFC 3339
  pattern_rfc   <- "^\\d{4}-\\d{2}-\\d{2}?(T\\d{2}:\\d{2}:\\d{2}Z)?$"
  check_pattern <- grepl(pattern_rfc, datetime, perl = TRUE)

  return(check_pattern)
}

#' @title utils functions
#'
#' @param msg   a \code{character} string with format error message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.error <- function(msg, ...) {

  stop(sprintf(msg, ...), call. = FALSE)
}

#' @title utils functions
#'
#' @param msg   a \code{character} string with format text message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.message <- function(msg, ...) {

  message(sprintf(msg, ...))
}

#' @title utils functions
#'
#' @param msg   a \code{character} string with format warning message.
#'
#' @param ...   values to be passed to \code{msg} parameter.
#'
#' @noRd
.warning <- function(msg, ...) {

  warning(sprintf(msg, ...), call. = FALSE)
}

#' @title utils functions
#'
#' @param obj       an \code{object} to compare.
#'
#' @param expected  a \code{character} with the expected classes.
#'
#' @noRd
.check_obj <- function(obj, expected) {

  obj_name <- as.character(substitute(obj))

  if (missing(obj))
    .error("Param `%s` is missing.", obj_name)

  if (!inherits(obj, expected))
    .error("Invalid %s value in `%s` param.",
           paste0("`", expected, "`", collapse = " or "), obj_name)
}


#' @title utils functions
#'
#' @rdname http_request
#'
#' @description
#' \code{.make_url} is a helper function to generate url. The returned
#' url is formed by appending \code{endpoint} at the end of base url
#' informed by \code{url} parameter. If \code{endpoint} has multiple elements
#' it will be collapsed using \code{'/'} character.
#'
#' Note that \code{.make_url} function differs from standards of relative URI
#' path resolution (RFC 3986). Any existing path in base url
#' is maintained in the final url, and a simple string contatenation is made
#' whithout including any character separator. For this reason, this function
#' does not support the query and fragment URI components in the base url.
#'
#' @param url         a \code{character} informing the base url of a
#' STAC web service.
#'
#' @param endpoint    a \code{character} a path to be appended in the final
#' url.
#'
#' @param params      a named \code{list} with all url query parameters to be
#' appended in the url.
#'
#' @return
#' \code{.make_url} returns an url to access STAC endpoints.
#'
#' @noRd
.make_url <- function(url, endpoint = "", params = list()) {

  # remove trailing '/' char
  if (substring(url, nchar(url)) == "/")
    url <- substring(url, 1, nchar(url) - 1)

  endpoint <- paste0(endpoint, collapse = "/")

  # TODO: URI resolution for previous existing query and fragment URI components
  # in informed url.
  res <- paste0(url, endpoint)

  if (length(params) > 0) {

    if (is.null(names(params)))
      stop("URL query values must be named.", call. = FALSE)
    params <- .querystring_encode(params)
    res <- paste(res, params, sep = "?")
  }

  return(res)
}

#' @title utils functions
#'
#' @param params a \code{list} of parameters received from stac objects.
#'
#' @return a \code{character} representing the encode parameters of the query.
#'
#' @noRd
.querystring_encode <- function(params) {

  if (!is.null(names(params)))
    return(paste(names(params),
                 sapply(unname(params), paste0, collapse = ","),
                 sep = "=", collapse = "&"))
  return(paste0(params, collapse = ","))
}

#' @title utils functions
#'
#' @param querystring a \code{character} with the query to be decoded.
#'
#' @return a \code{list} with the query params.
#'
#' @noRd
.querystring_decode <- function(querystring) {

  values <- lapply(strsplit(querystring, split = "&")[[1]],
                   function(x) strsplit(x, split = "=")[[1]])

  params <- lapply(values, `[[`, 2)
  names(params) <- sapply(values, `[[`, 1)

  return(params)
}

#' @title Utility functions
#'
#' @description
#' These function retrieves information about either \code{rstac} queries
#' (\code{RSTACQuery} objects) or \code{rstac} documents
#' (\code{RSTACDocument} objects).
#'
#' @param x        either a \code{RSTACQuery} object expressing a STAC query
#' criteria or any \code{RSTACDocument}.
#'
#' @param ...      config parameters to be passed to \link[httr]{GET}
#' method, such as \link[httr]{add_headers} or \link[httr]{set_cookies}.
#'
#' @return
#' The \code{stac_version()} function returns a \code{character} STAC API
#' version.
#'
#' @name utilities
#'
#' @export
stac_version <- function(x, ...) {

  UseMethod("stac_version")
}

#' @export
subfields <- function(x) {

  if (length(x) == 0) return(invisible(NULL))
  names(x[[1]])
}

#' @export
get_subfields <- function(x, ..., fields = NULL, merge = TRUE) {

  if (length(x) == 0) return(invisible(NULL))
  dots <- substitute(list(...))[-1]
  if (!is.character(dots)) dots <- as.character(dots)
  fields <- c(fields, dots)
  if (length(fields) == 0) fields <- names(x[[1]])
  x <- lapply(x, function(y) {
    y <- y[fields]
    names(y) <- fields
    y
  })
  x <- lapply(x, `names<-`, fields)
  if (merge)
    x <- do.call(mapply,
                 c(list(FUN = function(...) {
                   # equivalent to `c` but convert NULL --> NA
                   dots <- list(...)
                   if (length(dots) == 0)
                     return(NULL)
                   sapply(dots, function(x) {
                     if (is.null(x)) return(NA); x
                   }, simplify = is.atomic(dots[[1]]) && length(dots[[1]]) == 1)
                 },
                 SIMPLIFY = FALSE),
                 unname(x)))
  x
}


.format_bbox <- function(bbox) {

  if (!is.null(bbox) & length(bbox) == 4)
    return(paste(c("xmin:", "ymin:", "xmax:", "ymax:"),
                 sprintf("%.5f", bbox), collapse = ", "))

  if (!is.null(bbox) & length(bbox) == 6)
    return(paste(c("xmin:", "ymin:", "zmin:", "xmax:", "ymax:", "zmax:"),
                 sprintf("%.5f", bbox), collapse = ", "))
}
