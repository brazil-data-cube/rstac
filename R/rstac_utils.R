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
.verify_datetime <- function(datetime){

  # checks if the date time provided is an open interval
  check_interval <-
    grepl("(?=^(\\..\\/.*)).+|(.*/\\..)", datetime, perl = TRUE)

  if (check_interval) {
    # regex to separate the open interval elements
    split_datetime <- strsplit(datetime, "(?(?=/..)|$(?=../))", perl = TRUE)
    # checking if date time is in the RFC standards
    match_rfc      <- .check_rfc(split_datetime)

    if (match_rfc) {
      return(datetime)
    } else{
      stop(paste0("The interval date time provided is not in RFC format,
                  please check the RFC 3339 rules."), call. = F)
    }
  } else{
    # Splits the vector elements with the dates by the backslash
    split_datetime <- strsplit(datetime, "/", perl = TRUE)
    split_datetime <- unlist(split_datetime)

    # In case the vector has two elements it is a closed date time
    if (length(split_datetime) == 2) {
      # formatting the closed date time according to the RFC
      interval_dt <- as.POSIXct(split_datetime,
                                tz = "UTC",
                                tryFormats = c("%Y-%m-%dT%H:%M:%SZ",
                                               "%Y-%m-%d"))

      # Checks if there is FALSE value in vector
      if (!all(.check_rfc(interval_dt)))
        stop(paste0("The date time provided not follow the RFC 3339 format,
                    please check the RFC 3339 rules."), call. = F)

      # Check the interval, if the interval is wrong an error is returned
      ifelse(interval_dt[1] < interval_dt[2],
             return(datetime),
             stop(paste0("The closed date time provided is not in correct interval,
                  the first date time shold be less than second."), call. = F))
    }
    # Check if date time is a fixed interval
    else{
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
.check_rfc <- function(datetime){
  # Standard Regex of RFC 3339
  pattern_rfc <- "^\\d{4}-\\d{2}-\\d{2}?(T\\d{2}:\\d{2}:\\d{2}Z)?$"

  check_pattern <- grepl(pattern_rfc, datetime, perl = TRUE)

  return(check_pattern)
}

# .stac_version <- function(url){
#
# }


