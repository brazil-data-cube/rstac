#' Convert R expressions to CQL2
#'
#' @description
#' These functions convert R expressions to CQL2 standard (`TEXT` or `JSON`).
#'
#' @param expr  An R expression to be represented in CQL2
#'
#' @name cql2_functions
NULL

cql2 <- function(expr, lang = NULL, crs = NULL) {
  cql2_update_ident_env(expr)
  # create cql2 object
  obj <- structure(list(), class = c("cql2", "list"))
  cql2_filter(obj) <- cql2_eval(expr)
  cql2_lang(obj) <- lang
  cql2_crs(obj) <- crs

  class(obj) <- c(lang[[1]], "cql2", "list")
  obj
}

#' @rdname cql2_functions
#' @export
cql2_json <- function(expr) {
  expr <- unquote(
    substitute(expr, environment()),
    parent.frame(1)
  )
  cat(to_json(cql2(expr, lang = "cql2-json")))
}

#' @rdname cql2_functions
#' @export
cql2_text <- function(expr) {
  expr <- unquote(
    substitute(expr, environment()),
    parent.frame(1)
  )
  cat(cql2_filter(cql2(expr, lang = "cql2-text")))
}

# ---- cast functions ----

#' @exportS3Method
print.cql2_filter <- function(x, ...) {
  cat(to_text(x))
}

#' @exportS3Method
as.character.cql2_filter <- function(x, ...) {
  to_text(x)
}

#' @exportS3Method
print.cql2_json <- function(x, ...) {
  if (!is.null(x[["filter-crs"]])) {
    cat("<", x[["filter-crs"]], ">", sep = "", fill = TRUE)
  }
  cat(to_json(x[["filter"]]))
}

#' @exportS3Method
print.cql2_text <- function(x, ...) {
  if (!is.null(x[["filter-crs"]])) {
    cat("<", x[["filter-crs"]], ">", sep = "", fill = TRUE)
  }
  cat(x[["filter"]])
}

cql2_lang <- function(obj) {
  obj[["filter-lang"]]
}

`cql2_lang<-` <- function(obj, value) {
  if (!is.null(value)) obj[["filter-lang"]] <- value
  obj
}

cql2_crs <- function(obj) {
  obj[["filter-crs"]]
}

`cql2_crs<-` <- function(obj, value) {
  if (!is.null(value)) obj[["filter-crs"]] <- value
  obj
}

cql2_filter <- function(obj) {
  obj[["filter"]]
}

`cql2_filter<-` <- function(obj, value) {
  obj[["filter"]] <- value
  obj
}
