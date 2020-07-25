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
#' provided by \code{stac}, \code{stac_search}, \code{stac_collections},
#' or \code{stac_items} functions.
#'
#' @param headers    a \code{character} of named arguments to be passed as
#' HTTP request headers.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{stac_collections}}
#' \code{\link{stac_items}}
#'
#' @return
#' Either a \code{stac_collection} or a \code{stac_items} object
#' depending of the \code{s} parameter.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     get_request()
#' }
#'
#' @export
get_request <- function(s, headers = c()) {

  # check the object class
  .check_obj(s, "stac")

  if (!"get" %in% names(s$expected_responses))
    .error("HTTP GET method is invalid for this request.")

  tryCatch({
    res <- httr::GET(url =  .make_url(s$url, params = s$params),
                     httr::add_headers(headers))
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
#' @param enctype     a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0", limit = 100) %>%
#'     post_request()
#' }
#'
#' @export
post_request <- function(s, enctype =  c("json", "multipart", "form"),
                         headers = c()) {

  # check the object class
  .check_obj(s, "stac")

  if (!"post" %in% names(s$expected_responses))
    .error("HTTP POST method is invalid for this request.")

  # check if the provided expected response is valid for this endpoint
  enctype <- enctype[[1]]
  if (length(s$expected_responses$post$enctypes) > 0 &&
      !enctype %in% s$expected_responses$post$enctypes)
    .error(paste("The body request enctype '%s' is invalid",
                 "for this operation. Allowed enctypes are %s."),
           enctype, paste0("'", s$expected_responses$post$enctypes, "'",
                           collapse = " or "))

  # call the requisition subroutine
  tryCatch({
    res <- httr::POST(url =  s$url, body = s$params,
                      encode = enctype,
                      httr::add_headers(headers))
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
