#' @title STAC functions
#'
#' @author Rolf Simoes
#'
#' @description The \code{stac_request} is function that makes HTTP
#' requests to STAC web services, retrieves, and parse the data.
#'
#' @param stac       A \code{stac} object expressing a STAC search criteria or
#' any \code{stac_*} object.
#'
#' @param method     A \code{character} value informing the HTTP method to be
#' used for this request. Accepted methods are \code{'get'} or \code{'post'}.
#' Only used if no HTTP \code{method} is defined in \code{stac} object
#' parameter.
#'
#' @param headers    A \code{list} of named arguments to be passed as
#' http request headers. This is used in \emph{addition} to eventual headers
#' defined in \code{stac} object parameter.
#'
#' @seealso
#' \code{\link{stac}} \code{\link{stac_search}} \code{\link{stac_collections}}
#' \code{\link{stac_items}}
#'
#' @return
#' Either a \code{stac_collection} or a \code{stac_items} object
#' depending of the \code{stac} parameter details.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0") %>%
#'     stac_request()
#' }
#'
#' @export
stac_request <- function(stac, method = c("get", "post"), headers = list()) {

  if (!inherits(stac, c("stac", "stac_collection", "stac_items")))
    stop(sprintf("Invalid `stac` parameter value."), call. = FALSE)

  method <- method[[1]]
  if (!is.null(.stac_method(stac)))
    method <- .stac_method(stac)

  if (!method %in% c("get", "post"))
    stop(sprintf("Invalid request method '%s'.", method), call. = FALSE)

  # TODO: implement POST request support
  if (method == "get") {

    # TODO: validate stac response
    res <- .get_request(stac, headers = headers)

    # TODO: check responses for method and content-type according to
    # STAC spec v0.8.0. Maybe we need to create a function like
    # .check_get_resquest() <<-- inspired by MVC
    if (res$status_code != 200)
      stop(sprintf("Error %s %s", res$content$code, res$content$description),
           call. = FALSE)

    content <- structure(res$content,
                         stac = stac,
                         class = c("stac_items"))
  }

  return(content)
}

#' @title stac method
#'
#' @author Rolf Simoes
#'
#' @description checks if the \code{stac} object inherits the \code{stac} class
#'
#' @param stac A \code{stac} object expressing a STAC search criteria or
#' any \code{stac_*} object.
#'
#' @return An error if the given object does not belong to a \code{stac} class,
#' otherwise the http request method is added to the attributes.
.stac_method <- function(stac) {

  if (inherits(stac, "stac"))
    return(stac$method[[1]])

  if (inherits(stac, c("stac_collection", "stac_items")))
    return(attr(stac, "stac")$method[[1]])

  stop(sprintf("Invalid `stac` parameter value."))
}
