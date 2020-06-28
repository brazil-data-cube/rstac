

.stac_get <- function(url, endpoint = "/stac", params = list(), headers = list()) {

  url <- .make_url(url = url, endpoint = endpoint, params = params)

  if (stac_dryrun()) {
    message(url)
    return(invisible(NULL))
  }

  tryCatch({

    h <- curl::new_handle()
    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url = url, handle = h)
  },
  error = function(e) {

    stop(paste("Request error.", e$message), call. = FALSE)
  })

  content <- rawToChar(res$content)

  if (!jsonlite::validate(content))
    stop("Invalid JSON response.", call. = FALSE)

  content <- jsonlite::fromJSON(content,
                                simplifyVector = TRUE,
                                simplifyDataFrame = FALSE,
                                simplifyMatrix = FALSE)

  if (res$status_code != 200)
    stop(paste(content$code, content$description), call. = FALSE)

  return(content)
}
