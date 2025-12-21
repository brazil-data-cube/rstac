#' @title STAC API request functions
#'
#' @rdname request
#'
#' @description The `get_request` is function that makes HTTP GET
#' requests to STAC web services, retrieves, and parse the data.
#'
#' The `post_request` is function that makes HTTP POST
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param q                a `rstac_query` object expressing a STAC query
#' criteria.
#'
#' @param encode           a `character` informing the request body
#' Content-Type. Accepted types are `'json'` (`'application/json'`),
#' `'form'` (`'application/x-www-form-urlencoded'`),
#' and `'multipart'` (`'multipart/form-data'`). Defaults to
#' `'json'`.
#'
#' @param simplify_vector  a `logical` describing whether length-one nested
#' lists should be simplified into vectors. Defaults to TRUE. Can also be set
#' for an entire session via e.g. \code{options(rstac.simplify_vector = FALSE)}.
#'
#' @param ...              config parameters to be passed to [GET][httr::GET] or
#' [POST][httr::POST] methods, such as [add_headers][httr::add_headers] or
#' [set_cookies][httr::set_cookies].
#'
#' @seealso
#' [stac()] [stac_search()] [collections()]
#' [items()]
#'
#' @return
#' Either a `doc_catalog`, `doc_collection`,
#' `doc_collections`, `doc_items` or `doc_item`
#' object depending on the subclass and search fields parameters of `q`
#' argument.
#'
#' @examples
#' \dontrun{
#'  stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'      get_request()
#'
#'  stac("https://data.inpe.br/bdc/stac/v1/") %>%
#'      stac_search(collections = "CBERS4-WFI-16D-2") %>%
#'      post_request()
#' }
#' @export
get_request <- function(q, simplify_vector = NULL, ...) {
  simplify_vector <- simplify_vector_argument(simplify_vector)

  check_query(q)
  q$verb <- "GET"
  q$encode <- NULL
  q$version <- stac_version(q, ...)
  q <- before_request(q)
  res <- make_get_request(
    url = resolve_url(q$base_url, q$endpoint),
    query = query_encode(q$params),
    ...,
    error_msg = "Error while requesting"
  )
  # process content and return
  after_response(q, res = res, simplify_vector = simplify_vector)
}

#' @rdname request
#' @export
post_request <- function(q, simplify_vector = NULL, ..., encode = c("json", "multipart", "form")) {
  simplify_vector <- simplify_vector_argument(simplify_vector)

  check_query(q)
  # check request settings
  encode <- encode[[1]]
  check_body_encode(encode)
  q$verb <- "POST"
  q$encode <- encode
  q$version <- stac_version(q, ...)
  q <- before_request(q)
  res <- make_post_request(
    url = resolve_url(q$base_url, q$endpoint),
    body = q$params,
    encode = q$encode,
    ...,
    error_msg = "Error while requesting"
  )
  # process content and return
  after_response(q, res = res, simplify_vector = simplify_vector)
}

simplify_vector_argument <- function(simplify_vector = NULL) {
  if (!is.null(simplify_vector)) simplify_vector else getOption("rstac.simplify_vector", default = TRUE)
}
