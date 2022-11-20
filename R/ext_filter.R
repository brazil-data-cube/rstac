#' @title CQL2 filter extension
#'
#' @description
#' CQL2 is an OGC standard that enables complex filter expressions on OAFeat3
#' or STAC web services. CQL2 standard states that expressions can be
#' represented in JSON or TEXT formats.
#'
#' TODO: add docs
#'
#' @export
ext_filter <- function(q, expr, lang = NULL, crs = NULL) {

  # check parameter
  check_subclass(q, c("search", "ext_filter"))
  .check_lang(lang)

  # get expression
  params <- cql2(expr, lang = lang, crs = crs, env = environment())

  RSTACQuery(version = q$version,
             base_url = q$base_url,
             params = utils::modifyList(q$params, params),
             subclass = "ext_filter")
}

.check_lang <- function(lang) {
  lang
}

#' @export
endpoint.ext_filter <- function(q) {

  # using endpoint from search document
  endpoint.search(q)
}

#' @export
before_request.ext_filter <- function(q) {
  check_query_verb(q, verbs = c("GET", "POST"))
  if (is.null(q$params$`filter-lang`)) {
     if (q$verb == "GET") {
       params <- cql2_text(q$params)
     } else {
       params <- cql2_json(q$params)
     }
    q$params <- utils::modifyList(q$params, params)
  } else {
    if (q$verb == "GET" && q$params$`filter-lang` == "JSON") {
      params <- cql2_json(q$params)
      q$params <- utils::modifyList(q$params, params)
    }
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

  # # call super class
  # params <- parse_params.search(q, params)
  #
  # params$query <- .parse_values_keys(params$query)

  params
}
