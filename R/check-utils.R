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

#' @title Utility functions
#'
#' @param obj       an `object` to compare.
#'
#' @param expected  a `character` with the expected classes.
#'
#' @noRd
.check_obj <- function(obj, expected) {

  obj_name <- as.character(substitute(obj, env = environment()))

  if (!inherits(obj, expected))
    .error("Invalid %s value in `%s` param.",
           paste0("`", expected, "`", collapse = " or "), obj_name)
}

items_check <- function(items) {
  if (is.list(items) && !"assets" %in% names(items)) {
    .error("Parameter `items` is invalid.")
  }
}

select_check_eval <- function(val) {
  if (!is.logical(val)) {
    .error("Select expressions must be evaluated as logical.")
  }
  if (length(val) > 1) {
    .error("Select function must return a logical value of length 1.")
  }
}

select_eval <- function(asset, expr) {
  val <- tryCatch({
    eval(expr, envir = asset, enclos = parent.env(parent.frame()))
  }, error = function(e) {
    return(FALSE)
  })
  select_check_eval(val)
  return(val)
}

select_exec <- function(asset, select_fn) {
  val <- select_fn(asset)
  select_check_eval(val)
  return(val)
}
