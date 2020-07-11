#' @title STAC functions
#'
#' @author Rolf Simoes
#'
#' @description This function implements \code{/stac} API
#' endpoint (v0.8.0). It retrieves the root STAC Catalog or STAC Collection
#' that is the entry point to access any data published in a STAC web service.
#'
#' @param stac       A \code{stac} object expressing a STAC search criteria.
#'
#' @param method     A \code{character} value informing the HTTP method to be
#' used for this request. Accepted methods are \code{'get'} or \code{'post'}.
#'
#' @param .headers   A \code{list} of named arguments to be passed as
#' http request headers.
#'
#' @seealso
#' \code{\link{stac}}, \code{\link{stac_search}},
#' \code{\link{stac_collections}}, and \code{\link{stac_items}}
#'
#' @return A \code{stac_catalog} object representing STAC Catalog definition.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0")
#' }
#'
#' @export
stac_request <- function(stac, method = c("get", "post"), .headers = list()) {

  method <- method[[1]]
  if (!method %in% c("get", "post"))
    stop(sprintf("Invalid request method '%s'.", method), call. = FALSE)

  # TODO: implement POST request support
  if (method == "get") {

    # TODO: validate stac response
    res <- .get_request(stac, headers = .headers)

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
