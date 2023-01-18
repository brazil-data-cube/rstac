
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

# ---- cast functions ----

#' @exportS3Method
print.cql2_filter <- function(x, ...) {
  cat(to_text(x))
}

#' @exportS3Method
as.character.cql2_filter <- function(x, ...) {
  to_text(x)
}

# ---- getters and setters functions ----

cql2_lang <- function(obj) {
  obj[["filter-lang"]]
}

`cql2_lang<-` <- function(obj, value) {
  if (length(value) > 0) {
    check_lang(value)
    obj[["filter-lang"]] <- value[[1]]
  }
  obj
}

cql2_crs <- function(obj) {
  obj[["filter-crs"]]
}

`cql2_crs<-` <- function(obj, value) {
  if (length(value) > 0) obj[["filter-crs"]] <- value[[1]]
  obj
}

cql2_filter <- function(obj) {
  obj[["filter"]]
}

`cql2_filter<-` <- function(obj, value) {
  obj[["filter"]] <- value
  obj
}
