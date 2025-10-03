#' @title Filter extension
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' `ext_filter()` implements Common Query Language (CQL2) filter extension
#' on `rstac`. This extension expands the filter capabilities providing a
#' query language to construct more complex expressions. CQL2 is an OGC
#' standard and defines how filters can be constructed. It supports predicates
#' for standard data types like strings, numbers, and boolean as well as
#' for spatial geometries (point, lines, polygons) and temporal
#' data (instants and intervals).
#'
#' `r lifecycle::badge('experimental')`
#' `cql2_json()` and `cql2_text()` are helper functions that can be used
#' to show how expressions are converted into CQL2 standard, either
#' JSON or TEXT formats.
#'
#' `rstac` translates R expressions to CQL2, allowing users to express their
#' filter criteria using R language. For more details on how to create
#' CQL2 expressions in `rstac`. See the details section.
#'
#' @param q    a `rstac_query` object expressing a STAC query
#' criteria.
#' @param expr a valid R expression to be translated to CQL2 (see details).
#' @param lang a character value indicating which CQL2 representation
#' to be used. It can be either `"cql2-text"` (for plain text) or
#' `"cql2-json"` (for JSON format). If `NULL` (default), `"cql2-text"` is
#' used for HTTP `GET` requests and `"cql2-json"` for `POST` requests.
#' @param crs  an optional character value informing the coordinate reference
#' system used by geometry objects. If `NULL` (default), STAC services assume
#' `"WGS 84"`.
#'
#' @details
#' To allow users to express filter criteria in R language, `rstac` takes
#' advantage of the abstract syntax tree (AST) to translate R expressions
#' to CQL2 expressions. The following topics describe the correspondences
#' between `rstac` expressions and CQL2 operators.
#'
#' ## Non-standard evaluation
#' - `ext_filter()` uses non-standard evaluation to evaluate its expressions.
#'   That means users must escape any variable or call to be able to use them
#'   in the expressions. The escape is done by using `double-curly-braces`,
#'   i.e., `{{variable}}`.
#'
#' ## Standard comparison operators
#' - `==`, `>=`, `<=`, `>`, `<`, and `!=` operators correspond to
#'   `=`, `>=`, `<=`, `>`, `<`, and `<>` in CQL2, respectively.
#' - function `is_null(a)` and `!is_null(a)` corresponds to `a IS NULL` and
#'   `a IS NOT NULL` CQL2 operators, respectively.
#'
#' ## Advanced comparison operators
#' - `a %like% b` corresponds to CQL2 `a LIKE b`, `a` and `b` `strings` values.
#' - `between(a, b, c)` corresponds to CQL2 `a BETWEEN b AND c`, where
#'   `b` and `c` `integer` values.
#' - `a %in% b` corresponds to CQL2 `a IN (b)`, where `b` should be
#'   a list of values of the same type as `a`.
#'
#' ## Spatial operators
#' - functions `s_intersects(a, b)`, `s_touches(a, b)`, `s_within(a, b)`,
#'   `s_overlaps(a, b)`, `s_crosses(a, b)`, and `s_contains(a, b)` corresponds
#'   to CQL2 `S_INTERSECTS(a, b)`, `S_TOUCHES(a, b)`, `S_WITHIN(a, b)`,
#'   `S_OVERLAPS(a, b)`, `S_CROSSES(a, b)`, and `S_CONTAINS(a, b)` operators,
#'   respectively. Here, `a` and `b` should be `geometry` objects. `rstac`
#'   accepts `sf`, `sfc`, `sfg`, `list` (representing GeoJSON objects), or
#'   `character` (representing either GeoJSON or WKT).
#'
#' - **NOTE**: All of the above spatial object types, except for the
#'   `character`, representing a WKT, may lose precision due to numeric
#'   truncation when R converts numbers to JSON text. WKT strings are
#'   sent "as is" to the service. Therefore, the only way for users to
#'   retain precision on spatial objects is to represent them as a WKT
#'   string. However, user can control numeric precision using the
#'   `options(stac_digits = ...)`. The default value is 15 digits.
#'
#' ## Temporal operators
#' - functions `date(a)`, `timestamp(a)`, and `interval(a, b)` corresponds to
#'   CQL2 `DATE(a)`, `TIMESTAMP(a)`, and `INTERVAL(a, b)` operators,
#'   respectively. These functions create literal `temporal` values.
#'   The first two define an `instant` type, and the third an `interval` type.
#' - functions `t_after(a, b)`, `t_before(a, b)`, `t_contains(a, b)`,
#'   `t_disjoint(a, b)`, `t_during(a, b)`, `t_equals(a, b)`,
#'   `t_finishedby(a, b)`, `t_finishes(a, b)`, `t_intersects(a, b)`,
#'   `t_meets(a, b)`, `t_meet(a, b)`, `t_metby(a, b)`, `t_overlappedby(a, b)`,
#'   `t_overlaps(a, b)`, `t_startedby(a, b)`, and `t_starts(a, b)` corresponds
#'   to CQL2 `T_AFTER(a, b)`, `T_BEFORE(a, b)`, `T_CONTAINS(a, b)`,
#'   `T_DISJOINT(a, b)`, `T_DURING(a, b)`, `T_EQUALS(a, b)`,
#'   `T_FINISHEDBY(a, b)`, `T_FINISHES(a, b)`, `T_INTERSECTS(a, b)`,
#'   `T_MEETS(a, b)`, `T_MEET(a, b)`, `T_METBY(a, b)`, `T_OVERLAPPEDBY(a, b)`,
#'   `T_OVERLAPS(a, b)`, `T_STARTEDBY(a, b)`, and `T_STARTS(a, b)` operators,
#'   respectively. Here, `a` and `b` are `temporal` values (`instant` or
#'   `interval`, depending on function).
#'
#' ## Array Operators
#' - R unnamed lists (or vectors of size > 1) are translated to arrays by
#'   `rstac`. `list()` and `c()` functions always create `array` values
#'   in CQL2 context, no matter the number of its arguments.
#' - functions `a_equals(a, b)`, `a_contains(a, b)`, `a_containedby(a, b)`,
#'   and `a_overlaps(a, b)` corresponds to CQL2 `A_EQUALS(a, b)`,
#'   `A_CONTAINS(a, b)`, `A_CONTAINEDBY(a, b)`, and `A_OVERLAPS(a, b)`
#'   operators, respectively. Here, `a` and `b` should be `arrays`.
#'
#' @note
#' The specification states that double-quoted identifiers should be
#' interpreted as properties. However, the R language does not distinguish
#' double quote from single quote strings. The right way to represent
#' double quoted properties in R is to use the escape character (`),
#' for example `"date"`.
#'
#' @seealso [ext_query()], [stac_search()], [post_request()],
#' [before_request()], [after_response()], [content_response()]
#'
#' @return
#' A `rstac_query` object  with the subclass `ext_filter` containing
#'  all request parameters to be passed to `get_request()` or
#'  `post_request()` function.
#'
#' @examples
#' \dontrun{
#' # Standard comparison operators in rstac:
#' # Creating a stac search query
#' req <- stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   stac_search(limit = 5)
#'
#' # Equal operator '=' with collection property
#' req %>% ext_filter(collection == "sentinel-2-l2a") %>% post_request()
#'
#' # Not equal operator '!=' with collection property
#' req %>% ext_filter(collection != "sentinel-2-l2a") %>% post_request()
#'
#' # Less than or equal operator '<=' with datetime property
#' req %>% ext_filter(datetime <= "1986-01-01") %>% post_request()
#'
#' # Greater than or equal '>=' with AND operator
#' req %>% ext_filter(collection == "sentinel-2-l2a"   &&
#'                    `s2:vegetation_percentage` >= 50 &&
#'                    `eo:cloud_cover` <= 10) %>% post_request()
#' # Advanced comparison operators
#' # 'LIKE' operator
#' req %>% ext_filter(collection %like% "modis%") %>% post_request()
#'
#' # 'IN' operator
#' req %>% ext_filter(
#'   collection %in% c("landsat-c2-l2", "sentinel-2-l2a") &&
#'     datetime > "2019-01-01" &&
#'     datetime < "2019-06-01") %>%
#'   post_request()
#'
#' # Spatial operator
#' # Lets create a polygon with list
#' polygon <- list(
#'   type = "Polygon",
#'   coordinates = list(
#'     matrix(c(-62.34499836, -8.57414572,
#'              -62.18858174, -8.57414572,
#'              -62.18858174, -8.15351185,
#'              -62.34499836, -8.15351185,
#'              -62.34499836, -8.57414572),
#'            ncol = 2, byrow = TRUE)
#'   )
#' )
#' # 'S_INTERSECTS' spatial operator with polygon and geometry property
#' req %>% ext_filter(collection == "sentinel-2-l2a" &&
#'                    s_intersects(geometry, {{polygon}})) %>% post_request()
#'
#' # 'S_CONTAINS' spatial operator with point and geometry property
#' point <- list(type = "Point", coordinates = c(-62.45792211, -8.61158488))
#' req %>% ext_filter(collection == "landsat-c2-l2" &&
#'                    s_contains(geometry, {{point}})) %>% post_request()
#'
#' # 'S_CROSSES' spatial operator with linestring and geometry property
#' linestring <- list(
#'   type = "LineString",
#'   coordinates = matrix(
#'          c(-62.55735320, -8.43329465, -62.21791603, -8.36815014),
#'          ncol = 2, byrow = TRUE
#'   )
#' )
#' req %>% ext_filter(collection == "landsat-c2-l2" &&
#'                    s_crosses(geometry, {{linestring}})) %>% post_request()
#'
#' # Temporal operator
#' # 'T_INTERSECTS' temporal operator with datetime property
#' req %>% ext_filter(
#'   collection == "landsat-c2-l2" &&
#'     t_intersects(datetime, interval("1985-07-16T05:32:00Z",
#'                                     "1985-07-24T16:50:35Z"))) %>%
#'  post_request()
#'
#' # 'T_DURING' temporal operator with datetime property
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_during(datetime,
#'             interval("2022-07-16T05:32:00Z", ".."))) %>%
#'  post_request()
#'
#' # 'T_BEFORE' temporal operator with datetime property
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_before(datetime, timestamp("2022-07-16T05:32:00Z"))) %>%
#'  post_request()
#'
#' # 'T_AFTER' temporal operator with datetime property
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_after(datetime, timestamp("2022-07-16T05:32:00Z"))) %>%
#'   post_request()
#'
#' # Shows how CQL2 expression (TEXT format)
#' cql2_text(collection == "landsat-c2-l2" &&
#'   s_crosses(geometry, {{linestring}}))
#'
#' # Shows how CQL2 expression (JSON format)
#' cql2_json(collection == "landsat-c2-l2" &&
#'             t_after(datetime, timestamp("2022-07-16T05:32:00Z")))
#' }
#'
#' @export
ext_filter <- function(q, expr, lang = NULL, crs = NULL) {
  # check parameter
  check_query(q, c("stac", "search", "items"))
  check_lang(lang)
  # get expression
  expr <- unquote(
    expr = substitute(expr = expr, env = environment()),
    env = parent.frame()
  )
  params <- cql2(expr, lang = lang, crs = crs)
  # check filter expression is appropriate for types based on schema
  check_filter_schema(q, params)
  if (any(c("search", "items") %in% subclass(q)))
    subclass <- unique(c("ext_filter", subclass(q)))
  else
    subclass <- unique(c("ext_filter", "search", subclass(q)))
  rstac_query(
    version = q$version,
    base_url = q$base_url,
    params = modify_list(q$params, params),
    subclass = subclass
  )
}

check_lang <- function(lang) {
  if (!is.null(lang) && !lang[[1]] %in% c("cql2-json", "cql2-text"))
    .error("Language '%s' is not supported", lang[[1]])
}

#' @export
before_request.ext_filter <- function(q) {
  # call super class
  q <- NextMethod("before_request", q)
  if (q$verb == "GET") {
    # transform list into string to provide as querystring in GET
    if (!is.null(cql2_lang(q$params)) && cql2_lang(q$params) == "cql2-json") {
      cql2_filter(q$params) <- to_json(cql2_filter(q$params))
    } else {
      cql2_lang(q$params) <- "cql2-text"
      cql2_filter(q$params) <- to_text(cql2_filter(q$params))
    }
  } else {
    if (!is.null(cql2_lang(q$params)) && cql2_lang(q$params) == "cql2-text") {
      cql2_filter(q$params) <- to_text(cql2_filter(q$params))
    } else {
      cql2_lang(q$params) <- "cql2-json"
    }
  }
  q
}

#' @export
after_response.ext_filter <- function(q, res, simplify_vector = TRUE) {
  after_response.items(q, res, simplify_vector)
}

#' @export
parse_params.ext_filter <- function(q, params) {
  # call super class
  params <- NextMethod("parse_params")
  params
}

#' @rdname ext_filter
#' @export
cql2_json <- function(expr) {
  expr <- unquote(
    substitute(expr, environment()),
    parent.frame(1)
  )
  filter_expr <- to_json(cql2(expr, lang = "cql2-json"))
  cat(filter_expr)
  invisible(filter_expr)
}

#' @rdname ext_filter
#' @export
cql2_text <- function(expr) {
  expr <- unquote(
    substitute(expr, environment()),
    parent.frame(1)
  )
  filter_expr <- to_text(cql2(expr, lang = "cql2-text"))
  cat(filter_expr)
  invisible(filter_expr)
}

check_filter_schema <- function(q, params) {
  schema <- openapi_schema(q$base_url)

  # Go through each filter, extract the variable being filtered and the operation being used
  # Get its type (if available) from the schema
  # Check if the operation matches the variable schema
  # Only checking array variable vs not
  # i.e. if an array, check that the operation is an array operator (starts with "a_")
  # if it is _not_ an array, check that the operation is _not_ an array operation

  # The CQL2 logic looks a bit different depending on whether there is more than one filter or not:
  # if there is more than one filter, it is in e.g. params$filter$args[[1]]$args[[1]]
  # if there is only one, it is in e.g. params$filter$args[[1]]
  multiple_filters <- is.list(params$filter$args) & ("args" %in% names(params$filter$args[[1]]))

  if (multiple_filters) {
    for (params_filter in params$filter$args) {
      check_variable_op(params_filter, schema)
    }
  } else {
    params_filter <- params$filter
    check_variable_op(params_filter, schema)
  }
}

check_variable_op <- function(params_filter, schema) {
  filter_variable <- as.character(params_filter$args[[1]])
  filter_op <- params_filter$op
  filter_variable_schema <- schema[[filter_variable]]

  if (!is.null(filter_variable_schema)) {
    filter_variable_type <- filter_variable_schema$anyOf[[1]]$type

    if (filter_variable_type == "array") {
      if (!grepl("a_", filter_op)) {
        stop(paste0("`", filter_variable, "` is an array, must use an array operator in `ext_filter()`. See ?ext_filter for details."), call. = FALSE)
      }
    } else {
      if (grepl("a_", filter_op)) {
        stop(paste0("`", filter_variable, "` not is an array, cannot use an array operator in `ext_filter()`. See ?ext_filter for details."), call. = FALSE)
      }
    }
  }
}

openapi_schema <- function(url) {
  api_res <- stac(url) |>
    get_request()

  service_desc_link <- api_res |>
    links(rel == "service-desc")

  # TODO: handle if there is no open api spec

  openapi_spec <- service_desc_link[[1]] |>
    link_open()

  openapi_spec$components$schemas$ItemProperties$properties
}
