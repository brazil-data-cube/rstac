#' @title STAC functions
#'
#' @author Rolf Simoes
#'
#' @description The \code{stac_request} is function that makes HTTP
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param s          A \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{stac_collections},
#' or \code{stac_items} functions.
#'
#' @param method     A \code{character} value informing the HTTP method to be
#' used for this request. Accepted methods are \code{'get'} or \code{'post'}.
#'
#' @param post_enctype A \code{character} informing the request body
#' Content-Type. Accepted types \code{'application/json'},
#' \code{'application/x-www-form-urlencoded'}, and
#' \code{'multipart/form-data'}
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
#'     stac_request()
#' }
#'
#' @export
stac_request <- function(s, method = c("get", "post"),
                         post_enctype = c("application/json",
                                          "application/x-www-form-urlencoded",
                                          "multipart/form-data"),
                         headers = list()) {

  if (!inherits(s, "stac"))
    stop(sprintf("Invalid `stac` object."), call. = FALSE)

  method <- method[[1]]
  if (!method %in% names(s$expected_responses))
    stop(sprintf("Invalid HTTP method '%s' for this operation.", method),
         call. = FALSE)

  if (method == "get") {

    res <- .get_request(s, headers = headers)

  } else if (method == "post") {

    post_enctype <- post_enctype[[1]]
    if (!post_enctype %in% s$expected_responses$post$enctypes)
      stop(sprintf("Invalid HTTP body request enctype '%s' for this operation.",
                   post_enctype),
           call. = FALSE)

    res <- .post_request(s, enctype = post_enctype, headers = headers)

  }

  # check expected status-code and content-type
  content_class <- .check_response(res, s$expected_responses)
  content <- res$content
  if (!is.null(content_class))
    content <- structure(content,
                         stac = s,
                         class = content_class)

  return(content)
}
