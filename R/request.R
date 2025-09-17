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
#' @param q         a `rstac_query` object expressing a STAC query
#' criteria.
#'
#' @param encode    a `character` informing the request body
#' Content-Type. Accepted types are `'json'` (`'application/json'`),
#' `'form'` (`'application/x-www-form-urlencoded'`),
#' and `'multipart'` (`'multipart/form-data'`). Defaults to
#' `'json'`.
#'
#' @param ...       config parameters to be passed to [GET][httr::GET] or
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
get_request <- function(q, ...) {
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
  after_response(q, res = res)
}

#' @rdname request
#' @export
post_request <- function(q, ..., encode = c("json", "multipart", "form")) {
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
  after_response(q, res = res)
}
