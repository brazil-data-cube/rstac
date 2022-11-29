#' @title Filter extension
#'
#' @description
#' The filter extension expands the capabilities of the \code{/search} endpoint
#' providing an expressive query language to construct more complex filter
#' predicates. These filters are designed following the Common Query Language
#' (CQL2). CQL2 supports filter predicates for standard data types like strings,
#' numbers and boolean as well as for spatial geometries
#' (point, lines, polygons) and temporal geometries (instants and intervals).
#'
#' Standard comparison operators in rstac:
#' \itemize{
#'  \item \code{==} corresponds to \code{=}
#'  \item \code{>} corresponds to \code{>}
#'  \item \code{>=} corresponds to \code{>=}
#'  \item \code{<} corresponds to \code{<}
#'  \item \code{<=} corresponds to \code{<=}
#'  \item \code{is_null} corresponds to \code{IS NOT NULL}
#' }
#'
#' Advanced operators in rstac:
#' \itemize{
#'  \item \code{\%like\%} corresponds to \code{LIKE}
#'  \item \code{between()} corresponds to \code{BETWEEN}
#'  \item \code{\%in\%} corresponds to \code{IN}
#' }
#'
#' Spatial operators in rstac:
#' \itemize{
#'  \item \code{s_intersects} corresponds to \code{S_INTERSECTS}
#'  \item \code{s_touches} corresponds to \code{S_TOUCHES}
#'  \item \code{s_within} corresponds to \code{S_WITHIN}
#'  \item \code{s_overlaps} corresponds to \code{S_OVERLAPS}
#'  \item \code{s_crosses} corresponds to \code{S_CROSSES}
#'  \item \code{s_contains} corresponds to \code{S_CONTAINS}
#' }
#'
#' Temporal operators in rstac:
#' \itemize{
#'  \item \code{t_after} corresponds to \code{T_AFTER}
#'  \item \code{t_before} corresponds to \code{T_BEFORE}
#'  \item \code{t_contains} corresponds to \code{T_CONTAINS}
#'  \item \code{t_disjoint} corresponds to \code{T_DISJOINT}
#'  \item \code{t_during} corresponds to \code{T_DURING}
#'  \item \code{t_equals} corresponds to \code{T_EQUALS}
#'  \item \code{t_finishedby} corresponds to \code{T_FINISHEDBY}
#'  \item \code{t_finishes} corresponds to \code{T_FINISHES}
#'  \item \code{t_intersects} corresponds to \code{T_INTERSECTS}
#'  \item \code{t_meets} corresponds to \code{T_MEETS}
#'  \item \code{t_meet} corresponds to \code{T_MEET}
#'  \item \code{t_metby} corresponds to \code{T_METBY}
#'  \item \code{t_overlappedby} corresponds to \code{T_OVERLAPPEDBY}
#'  \item \code{t_overlaps} corresponds to \code{T_OVERLAPS}
#'  \item \code{t_startedby} corresponds to \code{T_STARTEDBY}
#'  \item \code{t_starts} corresponds to \code{T_STARTS}
#' }
#'
#' Array Operators in rstac:
#' \itemize{
#'  \item \code{a_equals} corresponds to \code{A_EQUALS}
#'  \item \code{a_contains} corresponds to \code{A_CONTAINS}
#'  \item \code{a_containedby} corresponds to \code{A_CONTAINEDBY}
#'  \item \code{a_overlaps} corresponds to \code{A_OVERLAPS}
#' }
#'
#' @param q    a `RSTACQuery` object expressing a STAC query
#' criteria.
#' @param expr a valid R expression following the filters supported by the
#' CQL2 predicates.
#' @param lang a character with the syntax used in the filter.
#' It can be used in text format \code{cql2-text} or in JSON format
#' \code{cql2-json}. By default, \code{cql2-text} is used in \code{GET}
#' requests and \code{cql2-json} in \code{POST} requests.
#' @param crs a character with coordinate reference systems.
#' By default WGS84 is used, this parameter will rarely be used.
#'
#'
#' @details TODO: add how we implement spatial objects: sf objects and list
#'
#' @details The specification shows that double quoted should be interpreted as
#' property. However, the R language does not distinguishes double quote
#' from single quote. The right way to represent especification double
#' quotes in R is to use the variable `name` without double quotes. For
#' exemple, use `date` intead of `"date"`.
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
#' req %>% ext_filter(collection %in% c("modis-64A1-061",
#'                                      "landsat-c2-l2",
#'                                      "sentinel-2-l2a") &&
#'                    datetime == "2019-01-01") %>%
#'   post_request()
#'
#' # Spatial operator
#' # TODO: remove sf poly by list poly
#' # Lets create a polygon with sf package
#' polygon <- sf::st_polygon(
#'   list(
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
#' point <- sf::st_point(c(-62.45792211, -8.61158488))
#' req %>% ext_filter(collection == "landsat-c2-l2" &&
#'                    s_contains(geometry, {{point}})) %>% post_request()
#'
#' # 'S_CROSSES' spatial operator with linestring and geometry property
#' linestring <- sf::st_linestring(
#'   matrix(c(-62.55735320, -8.43329465,
#'            -62.21791603, -8.36815014),
#'          ncol = 2, byrow = TRUE)
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
  params <- cql2(expr, lang = lang, crs = crs, env_expr = environment())

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
  params <- list()
  if (is.null(q$params[["filter-lang"]])) {
     if (q$verb == "GET") {
       params <- cql2_text(q$params)
     } else {
       params <- cql2_json(q$params)
     }
    q$params <- utils::modifyList(q$params, params)
  } else {
    if (q$verb == "GET" && q$params[["filter-lang"]] == "cql2-json") {
      json_params <- cql2_json(q$params)
      params[["filter"]] <- to_json(json_params[["filter"]])
      params[["filter-lang"]] <- to_json(json_params[["filter-lang"]])
      q$params <- utils::modifyList(q$params, params)
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
