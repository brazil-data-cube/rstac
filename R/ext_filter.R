#' @title Filter extension
#'
#' @description
#' `ext_filter()` implements Common Query Language (CQL2) filter extension
#' on `rstac`. This extension expands the filter capabilities providing a
#' query language to construct more complex expressions. CQL2 is an OGC
#' standard and defines how filters can be constructed. It supports predicates
#' for standard data types like strings, numbers, and boolean as well as
#' for spatial geometries (point, lines, polygons) and temporal
#' data (instants and intervals).
#'
#' `rstac` translates R expressions to CQL2 allowing users to express their
#' filter criteria using R language. For more details on how to create
#' CQL2 expressions in `rstac`, see the details section.
#'
#' @param q    a `RSTACQuery` object expressing a STAC query
#' criteria.
#' @param expr a valid R expression to be translated to CQL2 (see details).
#' @param lang a character value indicating which CQL2 representation
#' to be used. It can be either `"cql2-text"` (for plain text) or
#' `"cql2-json"` (for JSON format). If `NULL` (default), `"cql2-text"` is
#' used for HTTP `GET` requests and `"cql2-json"` for `POST` requests.
#' @param crs  an optional character value informing the coordinate reference
#' system used by geometry objects. If `NULL` (default) STAC services assume
#' `"WGS 84"`.
#'
#' @details
#' To allow users to express filter criteria in R language, `rstac` takes
#' advantage of the abstract syntax tree (AST) to translate R expressions
#' to CQL2 expressions. The following topics describes the correspondences
#' between `rstac` expressions and CQL2 operators.
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
#'   a list of values of same type as `a`.
#'
#' ## Spatial operators
#' - functions `s_intersects(a, b)`, `s_touches(a, b)`, `s_within(a, b)`,
#'   `s_overlaps(a, b)`, `s_crosses(a, b)`, and `s_contains(a, b)` corresponds
#'   to CQL2 `S_INTERSECTS(a, b)`, `S_TOUCHES(a, b)`, `S_WITHIN(a, b)`,
#'   `S_OVERLAPS(a, b)`, `S_CROSSES(a, b)`, and `S_CONTAINS(a, b)` operators,
#'   respectively. Here, `a` and `b` should be `geometry` objects. `rstac`
#'   accepts `sf`, `sfc`, `sfg`, or `geojson` objects.
#'
#' ## Temporal operators
#' - functions `date(a)`, `timestamp(a)`, and `interval(a, b)` corresponds to
#'   CQL2 `DATE(a)`, `TIMESTAMP(a)`, and `INTERVAL(a, b)` operators,
#'   respectively. These functions creates literal `temporal` values.
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
#' The specification states that double quoted identifiers should be
#' interpreted as a properties. However, the R language does not distinguishes
#' double quote from single quote strings. The right way to represent
#' double quoted properties in R is to use the escape character (`),
#' for example `"date"`.
#'
#' @seealso [ext_query()], [stac_search()], [post_request()],
#' [endpoint()], [before_request()],
#' [after_response()], [content_response()]
#'
#' @return
#' A `RSTACQuery` object  with the subclass `ext_filter` containing
#'  all request parameters to be passed to `get_request()` or
#'  `post_request()` function.
#'
#' @examples
#' \donttest{
#' # Standard comparison operators in rstac:
#' # Creating a stac search query
#' req <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'   rstac::stac_search(limit = 5)
#'
#' # Equal operator '=' with collection property
#' req %>% ext_filter(collection == "sentinel-2-l2a") %>% post_request()
#'
#' # Not equal operator '!=' with collection property
#' req %>% ext_filter(collection != "sentinel-2-l2a") %>% post_request()
#'
#' # Less than operator '<' with cloud_cover property
#' req %>% ext_filter(`eo:cloud_cover` < 10) %>% post_request()
#'
#' # Greater than operator '>' with vegetation_percentage property
#' req %>% ext_filter(`s2:vegetation_percentage` > 50) %>% post_request()
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
#' req %>%
#'  ext_filter(collection == "landsat-c2-l2" &&
#'             t_intersects(datetime,
#'             interval("1985-07-16T05:32:00Z", "1985-07-24T16:50:35Z"))) %>%
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
#' }
#' @export
ext_filter <- function(q, expr, lang = NULL, crs = NULL) {

  # check parameter
  check_subclass(q, c("search", "items"))
  .check_lang(lang)

  # get expression
  expr <- unquote(
    expr = substitute(expr = expr, env = environment()),
    env = parent.frame()
  )
  params <- cql2(expr, lang = lang, crs = crs)

  RSTACQuery(version = q$version,
             base_url = q$base_url,
             params = modify_list(q$params, params),
             subclass = unique(c("ext_filter", subclass(q))))
}

.check_lang <- function(lang) {
  lang
}

#' @export
endpoint.ext_filter <- function(q) {
  # using endpoint from search or items document
  if ("search" %in% subclass(q))
    return(endpoint.search(q))
  return(endpoint.items(q))
}

#' @export
before_request.ext_filter <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  if (is.null(cql2_lang(q$params))) {
     if (q$verb == "GET") {
       cql2_lang(q$params) <- "cql2-text"
     } else {
       cql2_lang(q$params) <- "cql2-json"
     }
  } else {
    if (q$verb == "GET" && cql2_lang(q$params) == "cql2-json") {
      # transform list into string to provide as querystring in GET
      cql2_filter(q$params) <- to_json(cql2_filter(q$params))
    }
  }
  if ("items" %in% subclass(q)) {
    # don't send 'collection_id' in url's query string or content body
    q <- omit_query_params(q, names = "collection_id")
  }
  return(q)
}

#' @export
after_response.ext_filter <- function(q, res) {

  content <- content_response(res, "200", c("application/geo+json",
                                            "application/json"))

  RSTACDocument(content = content, q = q, subclass = "STACItemCollection")
}

#' @export
parse_params.ext_filter <- function(q, params) {

  # call super class
  params <- NextMethod("parse_params")

  params
}
