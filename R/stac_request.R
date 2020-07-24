#' @title STAC functions
#'
#' @author Rolf Simoes
#'
#' @description The \code{get_request} is function that makes HTTP
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param s          A \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{stac_collections},
#' or \code{stac_items} functions.
#'
#' @param headers    A \code{list} of named arguments to be passed as
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

  tryCatch({
    res <- httr::GET(url =  .make_url(s$url, params = s$params),
                     httr::add_headers(headers))
  },
  error = function(e) {

    stop(paste("Request error.", e$message), call. = FALSE)
  })


  # check expected status-code and content-type
  content_class <- .check_response(res, s$expected_responses)
  content <- httr::content(res, simplifyVector = TRUE,
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
#' @author Rolf Simoes
#'
#' @description The \code{post_request} is function that makes HTTP
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param s          A \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{stac_collections},
#' or \code{stac_items} functions.
#'
#' @param encode A \code{character} informing the request body
#' Content-Type. Accepted types \code{'json'} \code{('application/json')},
#' \code{'form'} \code{('application/x-www-form-urlencoded')},
#' and \code{'multipart'} \code{('multipart/form-data')}.
#'
#' @param headers    A \code{character} of named arguments to be passed as
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
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0", limit = 100) %>%
#'     post_request()
#' }
#'
#' @export
post_request <- function(s, encode =  c("multipart", "form", "json"),
                         headers = c()) {

  # check the object class
  .check_obj(s, expected = c("stac"))

  # check if the provided expected response is valid for this endpoint
  if (!encode %in% s$expected_responses$post$enctypes)
    stop(sprintf("Invalid HTTP body request enctype '%s' for this operation.",
                 encode),
         call. = FALSE)

  # call the requisition subroutine
  tryCatch({
    res <- httr::POST(url =  s$url, body = s$params,
                      encode = encode,
                      httr::add_headers(headers))
  },
  error = function(e) {
    stop(paste("Request error.", e$message), call. = FALSE)
  })

  # check expected status-code and content-type
  content_class <- .check_response(res, s$expected_responses)
  content <- httr::content(res, simplifyVector = TRUE,
                             simplifyDataFrame = FALSE,
                                simplifyMatrix = FALSE)

  # apply corresponding stac class
  if (!is.null(content_class))
    content <- structure(content,
                         stac = s,
                         request = list(
                           method = "post",
                           enctype = encode),
                         class = content_class)

  return(content)
}
