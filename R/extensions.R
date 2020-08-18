#' @title Generic methods of the stac object
#'
#' @description STAC objects implement four generic S3 methods, such methods are
#' \code{params} and \code{response}. The \code{params} method defines and
#' manages the parameters before HTTP requests and are implemented for each
#' object created in the package, its basis is \code{params_get_request} for
#' GET and \code{params_post_request} for POST. The methods \code{response}
#' manage the types of documents returned by the response, its base is
#' \code{content_get_response} for GET and \code{content_post_response} for
#' POST.
#'
#' @param s       a \code{stac} object expressing a STAC search criteria
#' provided by \code{stac}, \code{stac_search}, \code{stac_collections},
#' or \code{stac_items} functions.
#'
#' @param enctype a \code{character} informing the request body
#' Content-Type. Accepted types are \code{'json'} (\code{'application/json'}),
#' \code{'form'} (\code{'application/x-www-form-urlencoded'}),
#' and \code{'multipart'} (\code{'multipart/form-data'}). Defaults to
#' \code{'json'}.
#'
#' @param res  a \code{httr} \code{response} object.
#'
#' @rdname stac_methods
params_get_request <- function(s) {

  UseMethod("params_get_request")
}

#' @rdname stac_methods
params_post_request <- function(s, enctype) {

  UseMethod("params_post_request")
}

#' @rdname stac_methods
content_get_response <- function(s, res) {

  UseMethod("content_get_response")
}

#' @rdname stac_methods
content_post_response <- function(s, res, enctype) {

  UseMethod("content_post_response")
}
