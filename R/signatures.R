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
#' @examples
#' \dontrun{
#' # STACItemCollection object
#' stac_obj <- stac("https://brazildatacube.dpi.inpe.br/stac/") %>%
#'   stac_search(collections = "CB4_64_16D_STK-1",
#'               datetime = "2019-06-01/2019-08-01") %>%
#'   stac_search() %>%
#'   get_request()
#'
#' # signing each item href
#' stac_obj %>% items_sign(sign_fn = sign_bdc(access_token = "123"))
#'
#' }
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
#' @param ...       additional parameters can be supplied to the `GET` function
#' of the `httr` package.
#' @param token_url a `character` with the URL that generates the tokens
#'  in the Microsoft service.
#'  By default is used:
#'  `"https://planetarycomputer.microsoft.com/api/sas/v1/token"`
#'
#' @return a `function` that signs each item assets.
#'
#' @examples
#' \dontrun{
#' # STACItemCollection object
#' stac_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/") %>%
#'  stac_search(collections = "sentinel-2-l2a",
#'              bbox = c(-47.02148, -42.53906, -12.98314, -17.35063)) %>%
#'  get_request()
#'
#' # signing each item href
#' stac_obj %>% items_sign(sign_fn = sign_planetary_computer())
#'
#' }
#'
#' @export
sign_planetary_computer <- function(..., token_url = NULL) {

  default_endpoint <- "https://planetarycomputer.microsoft.com/api/sas/v1/token"
  if (!is.null(token_url))
    default_endpoint <- token_url

  default_max_timeleft <- 300

  token <- list()

  # parse href to separate each query element, this will be used to dont
  # append the same token for an asset
  parse <- function(obj_req) {

    # transform to a datetime object
    obj_req[["msft:expiry"]] <- strptime(obj_req[["msft:expiry"]],
                                         "%Y-%m-%dT%H:%M:%SZ", tz="UTC")

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
