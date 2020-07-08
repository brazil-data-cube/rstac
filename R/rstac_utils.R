#' @title verify date time
#'
#' @author Felipe Carvalho
#'
#' @description This function
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
#'
#' @return A boolean for a verified date time; Return True if the date time is
#' correct and else otherwise
.verify_datetime <- function(datetime){
  return(invisible(NULL))
}
