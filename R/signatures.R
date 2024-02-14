ms_token <- new_env()

#' @title Signature in hrefs provided by the STAC from the Brazil Data Cube
#'  project.
#'
#' @description
#' These functions provide support to access assets from Brazil Data Cube.
#'
#' \itemize{
#'  \item `items_sign_bdc()`: `r lifecycle::badge('experimental')`
#'  A simplified function to sign assets' URL from Brazil Data Cube
#'    to be able to access the data.
#'
#'  \item `sign_bdc()`: Creates a signing function to be
#'  used by `items_sign()`. This function sign all the assets' URL.
#' }
#'
#' To sign the hrefs with your token you need to store it in an
#' environment variable in `BDC_ACCESS_KEY`or use `acess_token` parameter.
#'
#' @param items       a `doc_item` or `doc_items` object
#' representing the result of `/stac/search`,
#' \code{/collections/{collectionId}/items} or
#' \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param access_token a `character` with the access token parameter to access
#'  Brazil Data Cube assets.
#'
#' @param ...          additional parameters can be supplied to the `GET`
#'  function of the `httr` package.
#'
#' @return a `function` that signs each item assets.
#' \itemize{
#'  \item `items_sign_bdc()`: items with signed assets URLs.
#'
#'  \item `sign_bdc()`: a function to to be passed to `items_sign()`.
#' }
#'
#' @examples
#' \dontrun{
#'  # doc_items object
#'  stac_obj <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'    stac_search(collections = "CB4-16D-2",
#'                datetime = "2019-06-01/2019-08-01") %>%
#'    stac_search() %>%
#'    get_request()
#'
#'  # the new way to authenticate:
#'  stac_obj <- stac_obj %>%
#'    items_sign_bdc("<your-access-token>")
#'
#'  # this is the old way of authentication (still works):
#'  # stac_obj %>%
#'  #   items_sign(sign_fn = sign_bdc(access_token = "<your-access-token>"))
#' }
#'
#' @name items_sign_bdc
#'
#' @export
items_sign_bdc <- function(items, access_token = NULL, ...) {
  sign_fn <- sign_bdc(access_token)
  items_sign(items, sign_fn)
}

#' @rdname items_sign_bdc
#'
#' @export
sign_bdc <- function(access_token = NULL, ...) {

  token <- list()

  # parse href to separate each query element, this will be used to dont
  # append the same token for an asset
  parse <- function(obj_req) {

    token_str <- paste0("?access_token=", obj_req$token)
    parsed_url <- httr::parse_url(token_str)
    obj_req$token_value <- parsed_url$query

    obj_req
  }

  new_token <- function(item) {

    token[["default"]] <<- list("token" = access_token)

    if (is.null(access_token)) {

      if (!nzchar(Sys.getenv("BDC_ACCESS_KEY")))
        .error("No token informed in 'BDC_ACCESS_KEY' enviroment variable.")

      token[["default"]] <<- list("token" = Sys.getenv("BDC_ACCESS_KEY"))
    }
    token[["default"]] <<- parse(token$default)
  }

  exists_token <- function(item) {
    "default" %in% names(token)
  }

  get_token_value <- function(item) {
    token$default$token_value
  }

  # in the current implementation bdc tokens do not expire
  get_token_expiry <- function(item) {
    return(NULL)
  }

  is_token_expired <- function(item) {
    return(FALSE)
  }

  sign_asset <- function(asset, token) {

    asset_url <- httr::parse_url(asset$href)

    # if the href is already sign it will not be modified
    asset_url$query <- modify_list(asset_url$query, token)

    asset$href <- httr::build_url(asset_url)
    asset
  }

  sign_item <- function(item) {

    if (!exists_token(item) || is_token_expired(item))
      new_token(item)

    item$assets <- lapply(item$assets, sign_asset, get_token_value(item))

    return(item)
  }

  return(sign_item)
}

#' @title Signs URL to access assets from Microsoft's Planetary Computer.
#'
#' @description
#' These functions provide support to access assets from Planetary Computer.
#'
#' \itemize{
#'  \item `items_sign_planetary_computer()`: `r lifecycle::badge('experimental')`
#'  A simplified function to sign assets' URL from Microsoft Planetary
#'  Computer to be able to access the data.
#'
#'  \item `sign_planetary_computer()`: Creates a signing function to be
#'  used by `items_sign()`. This function sign all the assets' URL.
#' }
#'
#' @param items       a `doc_item` or `doc_items` object
#' representing the result of `/stac/search`,
#' \code{/collections/{collectionId}/items} or
#' \code{/collections/{collectionId}/items/{itemId}} endpoints.
#'
#' @param subscription_key the `subscription-key` to access restricted
#'   assets in Microsoft Planetary Computer. You can keep this parameter
#'   empty for non-protected assets.
#'
#' @param ...       additional parameters can be supplied to the `GET` function
#'   of the `httr` package.
#'
#' @param headers   a named character vector with headers key-value content.
#'
#' @param token_url a `character` with the URL that generates the tokens
#'  in the Microsoft service.
#'  By default is used:
#'  `"https://planetarycomputer.microsoft.com/api/sas/v1/token"`
#'
#' @return
#' \itemize{
#'  \item `items_sign_planetary_computer()`: items with signed assets URLs.
#'
#'  \item `sign_planetary_computer()`: a function to to be passed to
#'    `items_sign()`.
#' }
#'
#' @examples
#' \dontrun{
#'  # doc_items object
#'  stac_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
#'   stac_search(collections = "sentinel-2-l2a",
#'               bbox = c(-47.02148, -17.35063, -42.53906, -12.98314)) %>%
#'   get_request()
#'
#'  # the new way to authenticate:
#'  stac_obj <- stac_obj %>%
#'    items_sign_planetary_computer()
#'
#'  # this is the old way of authentication (still works):
#'  # stac_obj <- stac_obj %>%
#'  #   items_sign(sign_fn = sign_planetary_computer())
#'
#'  # example of access to collections that require authentication
#'  stac_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1") %>%
#'    stac_search(collections = c("sentinel-1-rtc"),
#'                bbox = c(-64.8597, -10.4919, -64.79272527, -10.4473),
#'                datetime = "2019-01-01/2019-01-28") %>%
#'    post_request()
#'
#'  # the new way to authenticate:
#'  # stac_obj <- stac_obj %>%
#'  #   items_sign_planetary_computer("<subscription-key>")
#'
#'  # this is the old way of authentication (still works):
#'  # stac_obj <- stac_obj %>%
#'  #   items_sign(
#'  #     sign_fn = sign_planetary_computer(
#'  #       headers = c("Ocp-Apim-Subscription-Key" = <your-mpc-token>)
#'  #     )
#'  #   )
#' }
#'
#' @name items_sign_planetary_computer
#' @export
items_sign_planetary_computer <- function(items, subscription_key = NULL, ...) {
  header <- NULL
  if (!is.null(subscription_key))
    header <- httr::add_headers(
      c("Ocp-Apim-Subscription-Key" = subscription_key)
    )
  sign_fn <- sign_planetary_computer(header)
  items_sign(items, sign_fn)
}

#' @rdname items_sign_planetary_computer
#' @export
sign_planetary_computer <- function(..., headers = NULL, token_url = NULL) {
  # general info
  ms_token_endpoint <- "https://planetarycomputer.microsoft.com/api/sas/v1/token"

  get_ms_info <- function(asset) {
    parsed_url <- httr::parse_url(asset$href)
    host_spplited <- strsplit(
      x = parsed_url$hostname, split = ".", fixed = TRUE
    )
    path_spplited <- strsplit(parsed_url$path, split = "/", fixed = TRUE)

    list(
      acc = host_spplited[[1]][[1]],
      cnt = path_spplited[[1]][[1]]
    )
  }

  get_ms_acc <- function(ms_info) {
    ms_info$acc
  }

  get_ms_cnt <- function(ms_info) {
    ms_info$cnt
  }

  is_public_asset <- function(parsed_url) {
    ms_blob_name <- ".blob.core.windows.net"
    ms_public_assets <- "ai4edatasetspublicassets.blob.core.windows.net"
    host <- parsed_url$hostname
    !endsWith(host, ms_blob_name) || host == ms_public_assets
  }

  if (!is.null(token_url)) {
    ms_token_endpoint <- token_url
  }

  # parse href to separate each query element, this will be used to don't
  # append the same token for an asset
  parse_token <- function(res) {
    # transform to a datetime object
    res[["msft:expiry"]] <- as.POSIXct(strptime(
      res[["msft:expiry"]], "%Y-%m-%dT%H:%M:%SZ"
    ))

    token_str <- paste0("?", res$token)
    parsed_url <- httr::parse_url(token_str)
    res$token_value <- parsed_url$query

    res
  }

  exists_token <- function(acc, cnt) {
    acc %in% names(ms_token) && cnt %in% names(ms_token[[acc]])
  }

  is_token_expired <- function(acc, cnt) {
    ms_max_timeleft <- 300

    difftime_token <- difftime(
      time1 = ms_token[[acc]][[cnt]][["msft:expiry"]],
      time2 = as.POSIXct(format(Sys.time(), tz = "UTC", usetz = TRUE)),
      units = "secs"
    )

    difftime_token < ms_max_timeleft
  }

  new_token <- function(acc, cnt) {
    if (exists_token(acc, cnt) && !is_token_expired(acc, cnt)) return(NULL)
    res <- make_get_request(
      url = paste(ms_token_endpoint, acc, cnt, sep = "/"),
      httr::add_headers(.headers = headers),
      ...,
      error_msg = "Error while requesting"
    )
    content <- content_response_json(res)
    if (!acc %in% names(ms_token)) {
      assign(acc, value = list(), envir = ms_token)
    }
    ms_token[[acc]][[cnt]] <- parse_token(content)
  }

  get_token <- function(acc, cnt) {
    new_token(acc, cnt)
    # get token value from global variable
    ms_token[[acc]][[cnt]]$token_value
  }

  sign_asset <- function(asset) {
    # public assets do not require a signature
    parsed_url <- httr::parse_url(asset$href)
    if (is_public_asset(parsed_url)) {
      return(asset)
    }
    ms_info <- get_ms_info(asset)
    account <- get_ms_acc(ms_info)
    container <- get_ms_cnt(ms_info)
    # get an existing token or generate a new one
    token_value <- get_token(account, container)
    # if the href is already sign it will not be modified
    parsed_url$query <- modify_list(parsed_url$query, token_value)

    asset$href <- httr::build_url(parsed_url)
    asset
  }

  sign_item <- function(item) {
    item$assets <- lapply(item$assets, sign_asset)
    return(item)
  }
  return(sign_item)
}
