#' @title Signature in hrefs provided by the STAC from the Brazil Data Cube
#'  project.
#'
#' @description To sign the hrefs with your token you need to store it in an
#' environment variable. Use the environment variable \code{BDC_ACCESS_KEY}.
#'
#' @param url a \code{character} with the reference href link provided by
#' objects \code{STACItem} or \code{STACItemCollection}.
#'
#' @param env a \code{enviroment} object created to store local variables of the
#'  package.
#'
#' @param ...  additional parameters can be supplied to the \code{GET} function
#' of the \code{httr} package.
#'
#'
#' @return a \code{character} with the href signed by the token.
#'
#' @export
sign_bdc <- function(url, env = NULL, ...) {

  parsed_href <- httr::parse_url(url)

  bdc_access_key <- Sys.getenv("BDC_ACCESS_KEY")
  if (nchar(bdc_access_key) == 0)
    .error(paste("items_sign: To sign the hrefs the environment variable",
                 "`BDC_ACCESS_KEY` needs to be provided."))

  # in case of href does not have query parameters
  if (is.null(parsed_href$query)) parsed_href$query <- list()

  # if the href is already sign it will not be modified
  url$query <- utils::modifyList(url$query,
                                 list("access_token" = bdc_access_key))

  # creates a new href with access token
  httr::build_url(url)
}

#' @title Signature in hrefs provided by the STAC from Microsoft's Planetary
#' Computer.
#'
#' @description To perform the signing of the hrefs a request is sent to
#' Planetary Computer servers and the returned content corresponds to the
#' token that will be used in the href.
#'
#' @param url a \code{character} with the reference href link provided by
#' objects \code{STACItem} or \code{STACItemCollection}.
#'
#' @param env a \code{enviroment} object created to store local variables of the
#'  package.
#'
#' @param ...  additional parameters can be supplied to the \code{GET} function
#' of the \code{httr} package.
#'
#' @return a \code{character} with the href signed by the token.
#'
#' @export
sign_planetary_computer <- function(url, env, ...) {

  if (!"res_token" %in% names(env))
    env$res_token <- list()

  url_token <- paste0("https://planetarycomputer.microsoft.com/api/sas/v1/sign",
                      "?href=", url)

  get_response_token <- function(url, ...) {
    tryCatch({
      res_content <- httr::content(httr::GET(url, ...), encoding = "UTF-8")
    }, error = function(error) {
      .warning(paste("\n", error, "in ", url))
    })

    # store token and datetime values in local environment
    env$res_token <- .parse_pc(res_content)
  }

  if (length(env$res_token) == 0 || env$res_token[["delta_time"]] < 60) {
    get_response_token(url_token, ...)
  }

  # update delta time in global variable
  env$res_token[["delta_time"]] <- difftime(env$res_token[["msft:expiry"]],
                                            Sys.time(),
                                            units = "secs")

  url_sign <- paste0(url, "?", env$res_token[["token"]])

  url_sign
}

#' @title Utility function
#'
#' @param r_obj a \code{list} returned from the requisition on the
#' microsoft planetary.
#'
#' @return a \code{list} with `delta_time` attribute.
#'
#' @noRd
.parse_pc <- function(r_obj) {

  # transform to a datetime object
  r_obj[["msft:expiry"]] <- strptime(r_obj[["msft:expiry"]],
                                     "%Y-%m-%dT%H:%M:%SZ")

  r_obj[["delta_time"]] <- difftime(r_obj[["msft:expiry"]], Sys.time(),
                                    units = "secs")

  r_obj[["token"]] <- strsplit(r_obj[["href"]], "\\?")[[1]][2]

  r_obj
}
