#' @title verify date time
#'
#' @name .verify_datetime
#'
#' @author Felipe Carvalho
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
.verify_datetime <- function(datetime) {

  # check if the date time provided is an open interval
  check_interval <-
    grepl("(?=^(\\..\\/.*)).+|(.*/\\..)", datetime, perl = TRUE)

  if (check_interval) {
    # regex to separate the open interval elements
    split_datetime <- strsplit(datetime, "(\\/\\..)|(\\..\\/)", perl = TRUE)
    split_datetime <- split_datetime[[1]][which(unlist(split_datetime) != "")]

    # checking if date time is in the RFC standards
    match_rfc <- .check_rfc(split_datetime)

    if (match_rfc) {
      return(datetime)
    } else
      stop(paste0("The interval date time provided is not in RFC format,
                  please check the RFC 3339 rules."), call. = FALSE)

    return(datetime)

  } else {

    # Splits the vector elements with the dates by the backslash
    split_datetime <- strsplit(datetime, "/", perl = TRUE)
    split_datetime <- unlist(split_datetime)

    # In case the vector has two elements it is a closed date time
    if (length(split_datetime) == 2) {
      # Checks if there is FALSE value in vector
      if (!all(.check_rfc(split_datetime)))
        stop(paste0("The date time provided not follow the RFC 3339 format,
                    please check the RFC 3339 rules."), call. = F)

      # formatting the closed date time according to the RFC
      interval_dt <- as.POSIXct(split_datetime,
                                tz = "UTC",
                                tryFormats = c("%Y-%m-%dT%H:%M:%SZ",
                                               "%Y-%m-%d"))

      # Check the interval, if the interval is wrong an error is returned
      ifelse(interval_dt[1] < interval_dt[2],
             return(datetime),
             stop(
             paste0("The closed date time provided is not in correct interval,
                  the first date time shold be less than second."), call. = F))
    }

    # Check if date time is a fixed interval
    else {
      if (!all(.check_rfc(split_datetime)) || length(split_datetime) != 1)
        stop(paste0("The date time provided not follow the RFC 3339 format,
                    please check the RFC 3339 rules."), call. = F)

      return(datetime)
    }
  }
}

#' @title check date time as RFC patterns
#'
#' @name .check_rfc
#'
#' @author Felipe Carvalho
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
.check_rfc <- function(datetime) {

  # Standard Regex of RFC 3339
  pattern_rfc   <- "^\\d{4}-\\d{2}-\\d{2}?(T\\d{2}:\\d{2}:\\d{2}Z)?$"
  check_pattern <- grepl(pattern_rfc, datetime, perl = TRUE)

  return(check_pattern)
}


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
#' @param url         A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param endpoint    A \code{character} a path to be appended in the final
#' url.
#'
#' @param params      A \code{list} with all url query parameters to be
#' appended in final url.
#'
#' @return
#' \code{.make_url} returns an url to access STAC endpoints.
#'
.make_url <- function(url, endpoint = "", params = list()) {

  endpoint <- paste0(endpoint, collapse = "/")

  # TODO: URI resolution for previous existing query and fragment URI components
  # in informed url.
  res <- paste0(url, endpoint)

  if (length(params) > 0) {

    if (is.null(names(params)))
      stop("URL query values must be named.", call. = FALSE)
    params <- .query_encode(params)
    res <- paste(res, params, sep = "?")
  }

  return(res)
}

#' @title STAC utils
#'
#' @author Rolf Simoes
#'
#' @description The \code{.query_encode} ...
#'
#' @param query ...
#'
#' @return ...
.query_encode <- function(params) {

  if (!is.null(names(params)))
    return(paste(names(params),
                 sapply(unname(params), paste0, collapse = ","),
                 sep = "=", collapse = "&"))
  return(paste0(params, collapse = ","))
}

#' @title STAC utils
#'
#' @author Rolf Simoes
#'
#' @description The \code{.url_to_stac} ...
#'
#' @param query ...
#'
#' @return ...
.url_to_stac <- function(url) {

  url <- url[[1]]

  base_url <- gsub("^([^?]+)\\?(.*)$", "\\1", url)
  query <- ""
  if (grepl("^([^?]+)\\?(.*)$", url))
    query <- gsub("^([^?]+)\\?(.*)$", "\\2", url)

  s <- structure(list(url = base_url,
                      params = .query_decode(query)),
                 class = "stac")
  return(s)
}

#' @title STAC utils
#'
#' @author Rolf Simoes
#'
#' @description The \code{.query_decode} ...
#'
#' @param query ...
#'
#' @return ...
.query_decode <- function(query) {

  values <- lapply(strsplit(query, split = "&")[[1]],
                   function(x) strsplit(x, split = "=")[[1]])

  params <- lapply(values, `[[`, 2)
  names(params) <- sapply(values, `[[`, 1)

  return(params)
}
