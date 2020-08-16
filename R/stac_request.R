#' @title STAC functions
#'
#' @rdname stac_request
#'
#' @author Rolf Simoes and Felipe Carvalho
#'
#' @description The \code{get_request} is function that makes HTTP GET
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param s          a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{collections},
#' or \code{items} functions.
#'
#' @param ...        other params to be passed to \link[httr]{GET} method
#'
#' @param headers    a \code{character} of named arguments to be passed as
#' HTTP request headers.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{collections}}
#' \code{\link{items}}
#'
#' @return
#' Either a \code{stac_catalog}, \code{stac_collection}, \code{stac_items},
#' or \code{stac_item} object depending on the \code{s} parameter.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     get_request()
#' }
#'
#' @export
get_request <- function(s, ..., headers = character()) {

  # check the object class
  .check_obj(s, "stac")

  if (!"get" %in% names(s$expected_responses))
    .error("HTTP GET method is invalid for this request.")

  tryCatch({
    res <- httr::GET(url = .make_url(s$url, endpoint = s$endpoint,
                                     params = s$params),
                     httr::add_headers(headers), ...)
  },
  error = function(e) {

    .error("Request error. %s", e$message)
  })

  # check expected status-code and content-type
  content_class <- .check_response(res, s$expected_responses)
  content <- httr::content(res,
                           simplifyVector = TRUE,
                           simplifyDataFrame = FALSE,
                           simplifyMatrix = FALSE)

  # apply corresponding stac class
  if (!is.null(content_class))
    content <- structure(content,
                         stac = s,
                         request = list(
                           method = "get"),
                         class = content_class)

  return(content)
}

#' @title STAC functions
#'
#' @rdname stac_request
#'
#' @description The \code{post_request} is function that makes HTTP POST
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param ...         other params to be passed to \link[httr]{POST} method
#'
#' @param enctype     a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @examples
#' \dontrun{
#'
#' stac_search("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0",
#'             collections = "MOD13Q1") %>%
#'      post_request(enctype = "json")
#' }
#'
#' @export
post_request <- function(s, ...,
                         enctype = c("application/json",
                                     "multipart/form-data",
                                     "application/x-www-form-urlencoded"),
                         headers = character()) {
  # check the object class
  .check_obj(s, "stac")

  # check if the provided expected response is valid for this endpoint...
  # ...check for method
  if (!"post" %in% names(s$expected_responses))
    .error("HTTP POST method is invalid for this request.")

  # ...check for body request content-type
  enctype <- enctype[[1]]
  if ((length(s$expected_responses$post$enctypes) > 0) &&
      !enctype %in% s$expected_responses$post$enctypes)
    .error(paste("The body request enctype '%s' is invalid",
                 "for this operation. Allowed enctypes are %s."),
           enctype, paste0("'", s$expected_responses$post$enctypes, "'",
                           collapse = " or "))

  # call the requisition subroutine
  httr_encode <-
    list("application/json" = "json",
         "multipart/form-data" = "multipart",
         "application/x-www-form-urlencoded" = "form")
  tryCatch({
    res <- httr::POST(url = .make_url(s$url, endpoint = s$endpoint),
                      body = s$params, encode = httr_encode[[enctype]],
                      httr::add_headers(headers), ...)
  },
  error = function(e) {
    .error("Request error. %s", e$message)
  })

  # check expected status-code and content-type
  content_class <- .check_response(res, s$expected_responses)
  content <- httr::content(res,
                           simplifyVector = TRUE,
                           simplifyDataFrame = FALSE,
                           simplifyMatrix = FALSE)

  # apply corresponding stac class
  if (!is.null(content_class))
    content <- structure(content,
                         stac = s,
                         request = list(
                           method = "post",
                           enctype = enctype),
                         class = content_class)

  return(content)
}
