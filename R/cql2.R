#' Convert R expressions to CQL2
#'
#' @description
#' These functions convert R expressions to CQL2 standard (text or JSON).
#'
#' @name cql2
#'
#' @param expr  An R expression to be represented in CQL2
#'
#' @examples
#' # basic cql2 examples
#' cql2("This is a literal string.", lang = "cql2-text")
#' cql2("Via dell'Avvento", lang = "cql2-text")
#' cql2("Via dell'Avvento", lang = "cql2-json")
#' cql2(-100, lang = "cql2-text")
#' cql2(-100, lang = "cql2-json")
#' cql2(3.14159, lang = "cql2-text")
#' cql2(TRUE, lang = "cql2-text")
#' cql2(FALSE, lang = "cql2-text")
#' cql2(timestamp("1969-07-20T20:17:40Z"), lang = "cql2-text")
#' cql2(timestamp("1969-07-20T20:17:40Z"), lang = "cql2-json")
#' cql2(date("1969-07-20"), lang = "cql2-text")
#' cql2(date("1969-07-20"), lang = "cql2-json")
#' cql2(interval("1969-07-16", "1969-07-24"), lang = "cql2-text")
#' cql2(interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"), lang = "cql2-text")
#' cql2(interval("2019-09-09", ".."), lang = "cql2-text")
#' cql2(interval("1969-07-16", "1969-07-24"), lang = "cql2-json")
#' cql2(interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"), lang = "cql2-json")
#' cql2(interval("2019-09-09", ".."), lang = "cql2-json")
#' cql2(city == "Toronto", lang = "cql2-text")
#' cql2(city == "Toronto", lang = "cql2-json")
#' cql2(balance - 150.0 > 0, lang = "cql2-text")
#' cql2(balance - 150.0 > 0, lang = "cql2-json")
#' cql2(updated >= date("1970-01-01"), lang = "cql2-text")
#' cql2(updated >= date("1970-01-01"), lang = "cql2-json")
#' cql2(!is_null(geometry), lang = "cql2-text")
#' cql2(!is_null(geometry), lang = "cql2-json")
#' poly_sf <- sfheaders::sf_polygon(matrix(c(0,0,0,0,1,1), ncol = 2))
#' cql2(s_intersects({{poly_sf}}, geometry), lang = "cql2-text")
#' cql2(s_intersects({{poly_sf}}, geometry), lang = "cql2-json")
#' cql2(s_crosses(geometry, {{poly_sf}}), lang = "cql2-text")
#' cql2(s_crosses(geometry, {{poly_sf}}), lang = "cql2-json")
#' cql2(t_intersects(event_date,
#'           interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z")),
#'           lang = "cql2-text")
#' cql2(t_intersects(event_date,
#'           interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z")),
#'           lang = "cql2-json")
#' cql2(t_during(touchdown,
#'           interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")),
#'           lang = "cql2-text")
#' cql2(t_during(touchdown,
#'           interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")),
#'           lang = "cql2-json")
#' cql2(s_within(road,Buffer(geometry,10,"m")), lang = "cql2-text")
#' cql2(s_within(road,Buffer(geometry,10,"m")), lang = "cql2-json")
#' cql2(t_during(timestamp("1969-07-20T20:17:40Z"),
#'      interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")),
#'      lang = "cql2-text")
#' cql2(t_during(timestamp("1969-07-20T20:17:40Z"),
#'      interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")),
#'      lang = "cql2-json")
NULL
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
    cat(to_text(x[["filter"]]))
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
