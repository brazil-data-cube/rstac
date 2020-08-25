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
#' @param s          a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{collections},
#' or \code{items} functions.
#'
#' @param enctype    a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @param ...        other params to be passed to \link[httr]{GET} or
#' \link[httr]{POST} methods
#'
#' @param headers    a \code{character} of named arguments to be passed as
#' HTTP request headers.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{collections}}
#' \code{\link{items}}
#'
#' @return
#' Either a \code{stac_catalog}, \code{stac_collection},
#'  \code{stac_collection_list}, \code{stac_item_collection} or \code{stac_item}
#'  object depending on the subclass and search fields parameters of \code{s}
#'  argument.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
#'       force_version = "0.8.1") %>%
#'  get_request()
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.1",
#'      force_version = "0.8.1") %>%
#'  stac_search(collections = "MOD13Q1") %>%
#'  post_request()
#' }
#' @export
get_request <- function(s, ..., headers = character()) {

  # check the object class
  .check_obj(s, "stac")

  # get params
  params <- params_get_request(s)

  tryCatch({
    res <- httr::GET(url = .make_url(s$url, endpoint = s$endpoint,
                                     params = params),
                     httr::add_headers(headers), ...)
  },
  error = function(e) {

    .error("Request error. %s", e$message)
  })

  # process content according to status-code and content-type
  content <- content_get_response(s, res)

  return(content)
}

#' @rdname request
#' @export
post_request <- function(s, ...,
                         enctype = c("json", "multipart", "form"),
                         headers = character()) {
  # check the object class
  .check_obj(s, "stac")

  httr_encode <- c("json", "multipart", "form")
  enctype <- enctype[[1]]
  if (!enctype %in% httr_encode)
    .error("Invalid body `enctype` '%s'. Allowed `enctypes` are %s.",
           enctype, paste0("'", httr_encode, "'", collapse = ", "))

  # get params
  params <- params_post_request(s, enctype = enctype)

  tryCatch({
    res <- httr::POST(url = .make_url(s$url, endpoint = s$endpoint),
                      body = params, encode = enctype,
                      httr::add_headers(headers), ...)
  },
  error = function(e) {
    .error("Request error. %s", e$message)
  })

  # process content according to status-code and content-type
  content <- content_post_response(s, res, enctype = enctype)

  return(content)
}
