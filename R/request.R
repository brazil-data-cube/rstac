#' @title STAC API request functions
#'
#' @rdname request
#'
#' @description The \code{get_request} is function that makes HTTP GET
#' requests to STAC web services, retrieves, and parse the data.
#'
#' The \code{post_request} is function that makes HTTP POST
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param q         a \code{RSTACQuery} object expressing a STAC query
#' criteria.
#'
#' @param encode    a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @param ...       config parameters to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods, such as \link[httr]{add_headers} or
#' \link[httr]{set_cookies}.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{collections}}
#' \code{\link{items}}
#'
#' @return
#' Either a \code{stac_catalog}, \code{stac_collection},
#' \code{stac_collection_list}, \code{stac_item_collection} or \code{stac_item}
#' object depending on the subclass and search fields parameters of \code{s}
#' argument.
#'
#' @examples
#' \donttest{
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'  get_request()
#'
#' stac("http://brazildatacube.dpi.inpe.br/stac") %>%
#'  stac_search(collections = "CB4_64_16D_STK-1") %>%
#'  post_request()
#' }
#' @export
get_request <- function(q, ...) {

  # check the object class
  .check_obj(q, "RSTACQuery")

  # stamp verb
  q$verb <- "GET"
  q$encode <- NULL

  if (is.null(q$url)) {

    # check version
    q$version <- stac_version(q, ...)

    # set endpoint
    q$endpoint <- get_endpoint(q)

    # process STAC object
    q <- before_request(q)

    # build url
    q$url <- .make_url(q$base_url, endpoint = q$endpoint, params = q$params)
  }

  tryCatch({
    res <- httr::GET(url = q$url, ...)
  },
  error = function(e) {

    .error("Request error. %s", e$message)
  })

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

  if (is.null(q$url)) {

    # detect version
    q$version <- stac_version(q, ...)

    # set endpoint
    q$endpoint <- get_endpoint(q)

    # process STAC object
    q <- before_request(q)

    # build url
    q$url <- .make_url(q$base_url, endpoint = q$endpoint)
  }

  tryCatch({
    res <- httr::POST(url = q$url, ..., body = q$params,
                      encode = q$encode)
  },
  error = function(e) {
    .error("Request error. %s", e$message)
  })

  # process content according to status-code and content-type
  content <- after_response(q, res = res)

  return(content)
}
