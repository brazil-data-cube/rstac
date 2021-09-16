#' @title Signature in hrefs provided by the STAC from the Brazil Data Cube
#'  project.
#'
#' @description To sign the hrefs with your token you need to store it in an
#' environment variable in `BDC_ACCESS_KEY`or use `acess_token` parameter.
#'
#' @param access_token a `character` with the access token parameter to access
#'  Brazil Data Cube assets.
#'
#' @param ...          additional parameters can be supplied to the `GET`
#'  function of the `httr` package.
#'
#' @return a `function` that signs each item assets.
#'
#' @export
sign_bdc <- function(access_token = NULL, ...) {

  token <- list()

  # parse href to separate each query element, this will be used to dont
  # append the same token for an asset
  parse <- function(obj_req) {

    token_str <- paste0("?access_token=", obj_req[["token"]])
    obj_req[["token_value"]] <- httr::parse_url(token_str)[["query"]]

    obj_req
  }

  new_token <- function(item) {

    token[["default"]] <<- list("token" = access_token)

    if (is.null(access_token)) {

      if (!nzchar(Sys.getenv("BDC_ACCESS_KEY")))
        .error("No token informed in 'BDC_ACCESS_KEY' enviroment variable.")

      token[["default"]] <<- list("token" = Sys.getenv("BDC_ACCESS_KEY"))
    }

    token[["default"]] <<- parse(token[["default"]])
  }

  exists_token <- function(item) {
    "default" %in% names(token)
  }

  get_token_value <- function(item) {
    token[["default"]][["token_value"]]
  }

  # in the current implementation bdc tokens do not expire
  get_token_expiry <- function(item) {
    return(NULL)
  }

  is_token_expired <- function(item) {
    return(FALSE)
  }

  sign_asset <- function(asset, token) {

    asset_url <- httr::parse_url(asset[["href"]])

    # if the href is already sign it will not be modified
    asset_url$query <- .modify_list(asset_url$query, token)

    asset[["href"]] <- httr::build_url(asset_url)
    asset
  }

  sign_item <- function(item) {

    if (!exists_token(item) || is_token_expired(item))
      new_token(item)

    item[["assets"]] <- lapply(item[["assets"]], sign_asset,
                               get_token_value(item))

    return(item)
  }

  return(sign_item)
}

#' @title Signature in hrefs provided by the STAC from Microsoft's Planetary
#' Computer.
#'
#' @description To perform the signing of the hrefs a request is sent to
#' Planetary Computer servers and the returned content corresponds to the
#' token that will be used in the href.
#'
#' @param ...  additional parameters can be supplied to the `GET` function
#' of the `httr` package.
#'
#' @return a `function` that signs each item assets.
#'
#' @export
sign_planetary_computer <- function(...) {

  default_endpoint <- "https://planetarycomputer.microsoft.com/api/sas/v1/token"
  default_max_timeleft <- 300

  token <- list()

  # parse href to separate each query element, this will be used to dont
  # append the same token for an asset
  parse <- function(obj_req) {

    # transform to a datetime object
    obj_req[["msft:expiry"]] <- strptime(obj_req[["msft:expiry"]],
                                         "%Y-%m-%dT%H:%M:%SZ")

    token_str <- paste0("?", obj_req[["token"]])
    obj_req[["token_value"]] <- httr::parse_url(token_str)[["query"]]

    obj_req
  }

  new_token <- function(item) {

    url <- paste0(default_endpoint, "/", item$collection)

    tryCatch({
      res_content <- httr::content(httr::GET(url, ...), encoding = "UTF-8")
    },
    error = function(e) {
      .error("Request error. %s", e$message)
    })

    if (!"token" %in% names(res_content))
      .error("No collection found with id '%s'", item$collection)

    token[[item$collection]] <<- parse(res_content)
  }

  exists_token <- function(item) {
    item$collection %in% names(token)
  }

  get_token_value <- function(item) {
    token[[item$collection]][["token_value"]]
  }

  get_token_expiry <- function(item) {
    token[[item$collection]][["msft:expiry"]]
  }

  is_token_expired <- function(item) {

    difftime_token <- difftime(get_token_expiry(item),
                               as.POSIXlt(Sys.time(), tz = "UTC"),
                               units = "secs")

    difftime_token < default_max_timeleft
  }

  sign_asset <- function(asset, token) {

    asset_url <- httr::parse_url(asset[["href"]])

    # if the href is already sign it will not be modified
    asset_url$query <- .modify_list(asset_url$query, token)

    asset[["href"]] <- httr::build_url(asset_url)
    asset
  }

  sign_item <- function(item) {

    if (!exists_token(item) || is_token_expired(item))
      new_token(item)

    item[["assets"]] <- lapply(item[["assets"]], sign_asset,
                               get_token_value(item))

    return(item)
  }

  return(sign_item)
}
