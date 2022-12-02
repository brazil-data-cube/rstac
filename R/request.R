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
#' @param q         a `RSTACQuery` object expressing a STAC query
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
#' Either a `STACCatalog`, `STACCollection`,
#' `STACCollectionList`, `STACItemCollection` or `STACItem`
#' object depending on the subclass and search fields parameters of `q`
#' argument.
#'
#' @examples
#' \dontrun{
#'  stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   get_request()
#'
#'  stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1") %>%
#'   post_request()
#' }
#' @export
get_request <- function(q, ...) {

  # check the object class
  .check_obj(q, "RSTACQuery")

  # stamp verb
  q$verb <- "GET"
  q$encode <- NULL

  # check version
  q$version <- stac_version(q, ...)

  # set endpoint
  q$endpoint <- endpoint(q)

  # process STAC object
  q <- before_request(q)

  # process omitted params
  q <- .do_omit_query_params(q)

  tryCatch({
    res <- httr::GET(url = make_url(q$base_url, endpoint = q$endpoint),
                     query = .querystrings_encode(q$params), ...)
  },
  error = function(e) {

    .error("Request error. %s", e$message)
  })

  # restore omitted params
  q <- .undo_omit_query_params(q)

  # process content according to status-code and content-type
  content <- after_response(q, res = res)

  return(content)
}

#' @rdname request
#' @export
post_request <- function(q, ..., encode = c("json", "multipart", "form")) {

  # check the object class
  .check_obj(q, "RSTACQuery")

  # check request settings
  httr_encode <- c("json", "multipart", "form")
  encode <- encode[[1]]
  if (!encode %in% httr_encode)
    .error("Invalid body `encode` '%s'. Allowed `econde` are %s.",
           encode, paste0("'", httr_encode, "'", collapse = ", "))

  # stamp verb
  q$verb <- "POST"
  q$encode <- encode

  # detect version
  q$version <- stac_version(q, ...)

  # set endpoint
  q$endpoint <- endpoint(q)

  # process STAC object
  q <- before_request(q)

  # process omitted params
  q <- .do_omit_query_params(q)

  tryCatch({
    res <- httr::POST(url = make_url(q$base_url, endpoint = q$endpoint), ...,
                      body = q$params, encode = q$encode)
  },
  error = function(e) {
    .error("Request error. %s", e$message)
  })

  # restore omitted params
  q <- .undo_omit_query_params(q)

  # process content according to status-code and content-type
  content <- after_response(q, res = res)

  return(content)
}

#' @describeIn extensions
#' The `.do_omit_query_params()` Function to make the omission of the
#'  parameters that were omitted in function `omit_query_params()`.
#'
#' @param q a `RSTACQuery` object.
#'
#' @noRd
.do_omit_query_params <- function(q) {

  if (is.character(q$omitted)) {

    to_omit <- names(q$param) %in% q$omitted
    if (length(to_omit) > 0) {
      q$omitted <- q$params[to_omit]
      q$params[to_omit] <- NULL
    }
  }
  q
}

#' @describeIn extensions
#' The `.undo_omit_query_params()` function to undo the omission of
#'  parameters that were omitted in function `omit_query_params()`.
#'
#' @param q a `RSTACQuery` object.
#'
#' @noRd
.undo_omit_query_params <- function(q) {

  if (is.list(q$omitted))
    q$params <- utils::modifyList(q$params, q$omitted)
  q$omitted <- NULL
  q
}
