#' @name .stac_request
#' @param method

stac_request<- function(url, endpoint = "/stac", params = list(), headers = list(),
                        method = "get") {

  if(method == "get"){
    url <- .make_url(url = url, endpoint = endpoint, params = params)
  } else{
    url <- .make_url(url = url, endpoint = endpoint)
  }

  print(url)

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
  browser()
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
