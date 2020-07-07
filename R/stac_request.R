#' @title make url
#'
#' @author Rolf Simoes
#'
#' @description This function
#'
#' @param url      A \code{character} informing the base url of a
#' STAC web service.
#'
#' @param endpoint A \code{character} ..
#'
#' @param params A \code{list} ...
#'
#' @return A res ...
.stac_request <- function(url, endpoint = "/stac", params = list(), headers = list(),
                        method = "get") {
  #browser()

  ifelse(method == "get",
         url <- .make_url(url = url, endpoint = endpoint, params = params),
         url <- .make_url(url = url, endpoint = endpoint))


  if (stac_dryrun()) {
    message(url)
    return(invisible(NULL))
  }

  tryCatch({
    h <- curl::new_handle()
    curl::handle_setheaders(h, .list = headers)
    if(method == "post"){
      curl::handle_setform(h, .list  = params)
    }
    res <- curl::curl_fetch_memory(url = url, handle = h)
  },
  error = function(e) {

    stop(paste("Request error.", e$message), call. = FALSE)
  })

  res$content <- rawToChar(res$content)

  if (!jsonlite::validate(res$content))
    stop("Invalid JSON response.", call. = FALSE)

  res$content <- jsonlite::fromJSON(res$content,
                                    simplifyVector = TRUE,
                                    simplifyDataFrame = FALSE,
                                    simplifyMatrix = FALSE)

  if (res$status_code != 200)
    stop(paste(res$content$code, res$content$description), call. = FALSE)

  return(res)
}
