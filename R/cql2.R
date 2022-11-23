#' Convert R expressions to CQL2
#'
#' @description
#' These functions convert R expressions to CQL2 standard (text or JSON).
#'
#' @name cql2
#'
#' @param expr  An R expression to be represented in CQL2
#' @param lang a character with the syntax used in the filter.
#' It can be used in text format \code{cql2-text} or in JSON format
#' \code{cql2-json}. By default, \code{cql2-text} is used in \code{GET}
#' requests and \code{cql2-json} in \code{POST} requests.
#' @param crs a character with coordinate reference systems.
#' By default WGS84 is used, this parameter will rarely be used.
#' @param env a environment to evaluate the expression
cql2 <- function(expr, lang = NULL, crs = NULL, env = environment()) {
    expr <- unquote(
        expr = substitute(expr = expr, env = env),
        env = .GlobalEnv
    )
    cql2_update_ident_env(expr)
    # create cql2 object
    obj <- structure(list(), class = c("cql2", "list"))
    cql2_filter(obj) <- cql2_eval(expr)
    if (!is.null(lang)) {
        obj <- switch(
            lang,
            "cql2-json" = cql2_json(obj),
            "cql2-text" = cql2_text(obj)
        )
    }
    cql2_crs(obj) <- crs
    obj
}

cql2_text <- function(obj) {
    x <- structure(list(), class = c("cql2_text", "cql2", "list"))
    cql2_filter(x) <- cql2_filter(obj)
    cql2_lang(x) <- "cql2-text"
    cql2_crs(x) <- cql2_crs(obj)
    x[["filter"]] <- to_text(x[["filter"]])
    x
}

cql2_json <- function(obj) {
    x <- structure(list(), class = c("cql2_json", "cql2", "list"))
    cql2_filter(x) <- cql2_filter(obj)
    cql2_lang(x) <- "cql2-json"
    cql2_crs(x) <- cql2_crs(obj)
    x
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
