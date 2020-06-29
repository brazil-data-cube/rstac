

#' @title stac functions
#'
#' @author Rolf Simoes
#'
#' @description This function implements \code{/stac} API
#' endpoint (v0.8.0). It retrieves the root STAC Catalog or STAC Collection
#' that is the entry point to access any data published in STAC web service.
#'
#' @param url     A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param .headers   A \code{list} of named arguments to be passed as
#' http request headers.
#'
#' @return A STAC Catalog definition.
#'
#' @examples
#' \dontrun{
#'
#' stac("http://brazildatacube.dpi.inpe.br/bdc-stac/0.8.0")
#' }
#'
#' @export
stac <- function(url, .headers = list()) {

  # TODO check valid stac response
  res <- .stac_get(url = url, endpoint = "/stac", headers = .headers)

  if (is.null(res))
    return(invisible(NULL))

  return(res)
}
