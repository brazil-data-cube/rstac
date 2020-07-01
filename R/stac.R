

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

################################################################################
#' @export
stac_new <- function(url, .headers = list()) {

  # Creating a base url
  base_url <- crul::HttpClient$new(url     = url,
                                   headers = .headers)
  # making a get request
  res <- base_url$get()

  # Verify status code
  if(res$status_code > 203){
    stop(paste(res$status_http()[2]$message, ". \nStatus code =", res$status_code),
         call. = FALSE)
  }

  # verifying the output type of API
  stopifnot(res$response_headers$`content-type` == 'application/json')

  # Parsing res file
  parsed_res <- res$parse("UTF-8")
  content    <- jsonlite::fromJSON(parsed_res, flatten = TRUE)

  return(content)
}
